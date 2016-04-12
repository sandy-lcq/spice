/* -*- Mode: C; c-basic-offset: 4; indent-tabs-mode: nil -*- */
/*
   Copyright (C) 2015 Jeremy White
   Copyright (C) 2015-2016 Francois Gouget

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, see <http://www.gnu.org/licenses/>.
*/
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <gst/gst.h>
#include <gst/app/gstappsrc.h>
#include <gst/app/gstappsink.h>

#include "red-common.h"
#include "video-encoder.h"


#define SPICE_GST_DEFAULT_FPS 30


typedef struct {
    SpiceBitmapFmt spice_format;
    const char *format;
    uint32_t bpp;
} SpiceFormatForGStreamer;

typedef struct SpiceGstVideoBuffer {
    VideoBuffer base;
    GstBuffer *gst_buffer;
    GstMapInfo map;
} SpiceGstVideoBuffer;

typedef struct SpiceGstEncoder {
    VideoEncoder base;

    /* Rate control callbacks */
    VideoEncoderRateControlCbs cbs;

    /* Spice's initial bit rate estimation in bits per second. */
    uint64_t starting_bit_rate;

    /* ---------- Video characteristics ---------- */

    uint32_t width;
    uint32_t height;
    const SpiceFormatForGStreamer *format;
    SpiceBitmapFmt spice_format;

    /* ---------- GStreamer pipeline ---------- */

    /* Pointers to the GStreamer pipeline elements. If pipeline is NULL the
     * other pointers are invalid.
     */
    GstElement *pipeline;
    GstAppSink *appsink;
    GstAppSrc *appsrc;
    GstCaps *src_caps;
    GstElement *gstenc;

    /* Pipeline parameters to modify before the next frame. */
#   define SPICE_GST_VIDEO_PIPELINE_STATE    0x1
#   define SPICE_GST_VIDEO_PIPELINE_BITRATE  0x2
#   define SPICE_GST_VIDEO_PIPELINE_CAPS     0x4
    uint32_t set_pipeline;

    /* Output buffer */
    GMutex outbuf_mutex;
    GCond outbuf_cond;
    VideoBuffer *outbuf;

    /* The bit rate target for the outgoing network stream. (bits per second) */
    uint64_t bit_rate;

    /* The minimum bit rate. */
#   define SPICE_GST_MIN_BITRATE (128 * 1024)

    /* The default bit rate. */
#   define SPICE_GST_DEFAULT_BITRATE (8 * 1024 * 1024)
} SpiceGstEncoder;


/* ---------- The SpiceGstVideoBuffer implementation ---------- */

static void spice_gst_video_buffer_free(VideoBuffer *video_buffer)
{
    SpiceGstVideoBuffer *buffer = (SpiceGstVideoBuffer*)video_buffer;
    if (buffer->gst_buffer) {
        gst_buffer_unmap(buffer->gst_buffer, &buffer->map);
        gst_buffer_unref(buffer->gst_buffer);
    }
    free(buffer);
}

static SpiceGstVideoBuffer* create_gst_video_buffer(void)
{
    SpiceGstVideoBuffer *buffer = spice_new0(SpiceGstVideoBuffer, 1);
    buffer->base.free = spice_gst_video_buffer_free;
    return buffer;
}


/* ---------- Miscellaneous SpiceGstEncoder helpers ---------- */

static inline double get_mbps(uint64_t bit_rate)
{
    return (double)bit_rate / 1024 / 1024;
}

/* Returns the source frame rate which may change at any time so don't store
 * the result.
 */
static uint32_t get_source_fps(SpiceGstEncoder *encoder)
{
    return encoder->cbs.get_source_fps ?
        encoder->cbs.get_source_fps(encoder->cbs.opaque) : SPICE_GST_DEFAULT_FPS;
}

static void set_pipeline_changes(SpiceGstEncoder *encoder, uint32_t flags)
{
    encoder->set_pipeline |= flags;
}

static void free_pipeline(SpiceGstEncoder *encoder)
{
    if (encoder->src_caps) {
        gst_caps_unref(encoder->src_caps);
        encoder->src_caps = NULL;
    }
    if (encoder->pipeline) {
        gst_element_set_state(encoder->pipeline, GST_STATE_NULL);
        gst_object_unref(encoder->appsrc);
        gst_object_unref(encoder->gstenc);
        gst_object_unref(encoder->appsink);
        gst_object_unref(encoder->pipeline);
        encoder->pipeline = NULL;
    }
}

/* The maximum bit rate we will use for the current video.
 *
 * This is based on a 10x compression ratio which should be more than enough
 * for even MJPEG to provide good quality.
 */
static uint64_t get_bit_rate_cap(SpiceGstEncoder *encoder)
{
    uint32_t raw_frame_bits = encoder->width * encoder->height * encoder->format->bpp;
    return raw_frame_bits * get_source_fps(encoder) / 10;
}

static void adjust_bit_rate(SpiceGstEncoder *encoder)
{
    if (encoder->bit_rate == 0) {
        /* Use the default value, */
        encoder->bit_rate = SPICE_GST_DEFAULT_BITRATE;
    } else if (encoder->bit_rate < SPICE_GST_MIN_BITRATE) {
        /* don't let the bit rate go too low */
        encoder->bit_rate = SPICE_GST_MIN_BITRATE;
    } else {
        /* or too high */
        encoder->bit_rate = MIN(encoder->bit_rate, get_bit_rate_cap(encoder));
    }
    spice_debug("adjust_bit_rate(%.3fMbps)", get_mbps(encoder->bit_rate));
}


/* ---------- GStreamer pipeline ---------- */

/* A helper for spice_gst_encoder_encode_frame() */
static const SpiceFormatForGStreamer *map_format(SpiceBitmapFmt format)
{
    /* See GStreamer's part-mediatype-video-raw.txt and
     * section-types-definitions.html documents.
     */
    static const SpiceFormatForGStreamer format_map[] =  {
        {SPICE_BITMAP_FMT_RGBA, "BGRA", 32},
        {SPICE_BITMAP_FMT_16BIT, "RGB15", 16},
        /* TODO: Test the other formats */
        {SPICE_BITMAP_FMT_32BIT, "BGRx", 32},
        {SPICE_BITMAP_FMT_24BIT, "BGR", 24},
    };

    int i;
    for (i = 0; i < G_N_ELEMENTS(format_map); i++) {
        if (format_map[i].spice_format == format) {
            if (i > 1) {
                spice_warning("The %d format has not been tested yet", format);
            }
            return &format_map[i];
        }
    }

    return NULL;
}

static void set_appsrc_caps(SpiceGstEncoder *encoder)
{
    if (encoder->src_caps) {
        gst_caps_unref(encoder->src_caps);
    }
    encoder->src_caps = gst_caps_new_simple(
        "video/x-raw",
        "format", G_TYPE_STRING, encoder->format->format,
        "width", G_TYPE_INT, encoder->width,
        "height", G_TYPE_INT, encoder->height,
        "framerate", GST_TYPE_FRACTION, get_source_fps(encoder), 1,
        NULL);
    gst_app_src_set_caps(encoder->appsrc, encoder->src_caps);
}

static GstBusSyncReply handle_pipeline_message(GstBus *bus, GstMessage *msg, gpointer video_encoder)
{
    SpiceGstEncoder *encoder = video_encoder;

    if (GST_MESSAGE_TYPE(msg) == GST_MESSAGE_ERROR) {
        GError *err = NULL;
        gchar *debug_info = NULL;
        gst_message_parse_error(msg, &err, &debug_info);
        spice_warning("GStreamer error from element %s: %s",
                      GST_OBJECT_NAME(msg->src), err->message);
        if (debug_info) {
            spice_debug("debug details: %s", debug_info);
            g_free(debug_info);
        }
        g_clear_error(&err);

        /* Unblock the main thread */
        g_mutex_lock(&encoder->outbuf_mutex);
        encoder->outbuf = (VideoBuffer*)create_gst_video_buffer();
        g_cond_signal(&encoder->outbuf_cond);
        g_mutex_unlock(&encoder->outbuf_mutex);
    }
    return GST_BUS_PASS;
}

static GstFlowReturn new_sample(GstAppSink *gstappsink, gpointer video_encoder)
{
    SpiceGstEncoder *encoder = (SpiceGstEncoder*)video_encoder;
    SpiceGstVideoBuffer *outbuf = create_gst_video_buffer();

    GstSample *sample = gst_app_sink_pull_sample(encoder->appsink);
    if (sample) {
        outbuf->gst_buffer = gst_sample_get_buffer(sample);
        gst_buffer_ref(outbuf->gst_buffer);
        gst_sample_unref(sample);
        if (gst_buffer_map(outbuf->gst_buffer, &outbuf->map, GST_MAP_READ)) {
            outbuf->base.data = outbuf->map.data;
            outbuf->base.size = gst_buffer_get_size(outbuf->gst_buffer);
        }
    }

    /* Notify the main thread that the output buffer is ready */
    g_mutex_lock(&encoder->outbuf_mutex);
    encoder->outbuf = (VideoBuffer*)outbuf;
    g_cond_signal(&encoder->outbuf_cond);
    g_mutex_unlock(&encoder->outbuf_mutex);

    return GST_FLOW_OK;
}

static gboolean create_pipeline(SpiceGstEncoder *encoder)
{
    gchar *gstenc;
    switch (encoder->base.codec_type)
    {
    case SPICE_VIDEO_CODEC_TYPE_MJPEG:
        /* Set max-threads to ensure zero-frame latency */
        gstenc = g_strdup("avenc_mjpeg max-threads=1");
        break;
    case SPICE_VIDEO_CODEC_TYPE_VP8: {
        /* See http://www.webmproject.org/docs/encoder-parameters/
         * - Set end-usage to get a constant bitrate to help with streaming.
         * - min-quantizer ensures the bitrate does not get needlessly high.
         * - resize-allowed would be useful for low bitrate situations but
         *   the decoder does not return a frame of the expected size so
         *   avoid it.
         * - error-resilient minimises artifacts in case the client drops a
         *   frame.
         * - Set lag-in-frames, deadline and cpu-used to match
         *   "Profile Realtime". lag-in-frames ensures zero-frame latency,
         *   deadline turns on realtime behavior, and cpu-used targets a 75%
         *   CPU usage.
         * - deadline is supposed to be set in microseconds but in practice
         *   it behaves like a boolean.
         */
        gstenc = g_strdup_printf("vp8enc end-usage=cbr min-quantizer=10 error-resilient=default lag-in-frames=0 deadline=1 cpu-used=4");
        break;
        }
    default:
        /* gstreamer_encoder_new() should have rejected this codec type */
        spice_warning("unsupported codec type %d", encoder->base.codec_type);
        return FALSE;
    }

    GError *err = NULL;
    gchar *desc = g_strdup_printf("appsrc is-live=true format=time do-timestamp=true name=src ! videoconvert ! %s name=encoder ! appsink name=sink", gstenc);
    spice_debug("GStreamer pipeline: %s", desc);
    encoder->pipeline = gst_parse_launch_full(desc, NULL, GST_PARSE_FLAG_FATAL_ERRORS, &err);
    g_free(gstenc);
    g_free(desc);
    if (!encoder->pipeline || err) {
        spice_warning("GStreamer error: %s", err->message);
        g_clear_error(&err);
        if (encoder->pipeline) {
            gst_object_unref(encoder->pipeline);
            encoder->pipeline = NULL;
        }
        return FALSE;
    }
    encoder->appsrc = GST_APP_SRC(gst_bin_get_by_name(GST_BIN(encoder->pipeline), "src"));
    encoder->gstenc = gst_bin_get_by_name(GST_BIN(encoder->pipeline), "encoder");
    encoder->appsink = GST_APP_SINK(gst_bin_get_by_name(GST_BIN(encoder->pipeline), "sink"));

#ifdef HAVE_GSTREAMER_0_10
    GstAppSinkCallbacks appsink_cbs = {NULL, NULL, &new_sample, NULL, {NULL}};
#else
    GstAppSinkCallbacks appsink_cbs = {NULL, NULL, &new_sample, {NULL}};
#endif
    gst_app_sink_set_callbacks(encoder->appsink, &appsink_cbs, encoder, NULL);

    /* Hook into the bus so we can handle errors */
    GstBus *bus = gst_element_get_bus(encoder->pipeline);
#ifdef HAVE_GSTREAMER_0_10
    gst_bus_set_sync_handler(bus, handle_pipeline_message, encoder);
#else
    gst_bus_set_sync_handler(bus, handle_pipeline_message, encoder, NULL);
#endif
    gst_object_unref(bus);

    if (encoder->base.codec_type == SPICE_VIDEO_CODEC_TYPE_MJPEG) {
        /* See https://bugzilla.gnome.org/show_bug.cgi?id=753257 */
        spice_debug("removing the pipeline clock");
        gst_pipeline_use_clock(GST_PIPELINE(encoder->pipeline), NULL);
    }

    set_pipeline_changes(encoder, SPICE_GST_VIDEO_PIPELINE_STATE |
                                  SPICE_GST_VIDEO_PIPELINE_BITRATE |
                                  SPICE_GST_VIDEO_PIPELINE_CAPS);

    return TRUE;
}

/* A helper for configure_pipeline() */
static void set_gstenc_bitrate(SpiceGstEncoder *encoder)
{
    adjust_bit_rate(encoder);
    switch (encoder->base.codec_type)
    {
    case SPICE_VIDEO_CODEC_TYPE_MJPEG:
        g_object_set(G_OBJECT(encoder->gstenc),
                     "bitrate", (gint)encoder->bit_rate,
                     NULL);
        break;
    case SPICE_VIDEO_CODEC_TYPE_VP8:
        g_object_set(G_OBJECT(encoder->gstenc),
                     "target-bitrate", (gint)encoder->bit_rate,
                     NULL);
        break;
    default:
        /* gstreamer_encoder_new() should have rejected this codec type */
        spice_warning("unsupported codec type %d", encoder->base.codec_type);
        free_pipeline(encoder);
    }
}

/* A helper for spice_gst_encoder_encode_frame() */
static gboolean configure_pipeline(SpiceGstEncoder *encoder)
{
    if (!encoder->pipeline && !create_pipeline(encoder)) {
        return FALSE;
    }
    if (!encoder->set_pipeline) {
        return TRUE;
    }

    /* If the pipeline state does not need to be changed it's because it is
     * already in the PLAYING state. So first set it to the NULL state so it
     * can be (re)configured.
     */
    if (!(encoder->set_pipeline & SPICE_GST_VIDEO_PIPELINE_STATE) &&
        gst_element_set_state(encoder->pipeline, GST_STATE_NULL) == GST_STATE_CHANGE_FAILURE) {
        spice_debug("GStreamer error: could not stop the pipeline");
        free_pipeline(encoder);
        return FALSE;
    }

    /* Configure the encoder bitrate */
    if (encoder->set_pipeline & SPICE_GST_VIDEO_PIPELINE_BITRATE) {
        set_gstenc_bitrate(encoder);
    }

    /* Set the source caps */
    if (encoder->set_pipeline & SPICE_GST_VIDEO_PIPELINE_CAPS) {
        set_appsrc_caps(encoder);
    }

    /* Start playing */
    if (gst_element_set_state(encoder->pipeline, GST_STATE_PLAYING) == GST_STATE_CHANGE_FAILURE) {
        spice_warning("GStreamer error: unable to set the pipeline to the playing state");
        free_pipeline(encoder);
        return FALSE;
    }

    encoder->set_pipeline = 0;
    return TRUE;
}

/* A helper for the *_copy() functions */
static int is_chunk_stride_aligned(const SpiceBitmap *bitmap, uint32_t index)
{
    SpiceChunks *chunks = bitmap->data;
    if (chunks->chunk[index].len % bitmap->stride != 0) {
        /* A line straddles two chunks. This is not supported */
        spice_warning("chunk %d/%d contains an incomplete line, cannot copy",
                      index, chunks->num_chunks);
        return FALSE;
    }
    return TRUE;
}

/* A helper for push_raw_frame() */
static inline int line_copy(SpiceGstEncoder *encoder, const SpiceBitmap *bitmap,
                            uint32_t chunk_offset, uint32_t stream_stride,
                            uint32_t height, uint8_t *buffer)
{
     uint8_t *dst = buffer;
     SpiceChunks *chunks = bitmap->data;
     uint32_t chunk_index = 0;
     for (int l = 0; l < height; l++) {
         /* We may have to move forward by more than one chunk the first
          * time around. This also protects us against 0-byte chunks.
          */
         while (chunk_offset >= chunks->chunk[chunk_index].len) {
             if (!is_chunk_stride_aligned(bitmap, chunk_index)) {
                 return FALSE;
             }
             chunk_offset -= chunks->chunk[chunk_index].len;
             chunk_index++;
         }

         /* Copy the line */
         uint8_t *src = chunks->chunk[chunk_index].data + chunk_offset;
         memcpy(dst, src, stream_stride);
         dst += stream_stride;
         chunk_offset += bitmap->stride;
     }
     spice_return_val_if_fail(dst - buffer == stream_stride * height, FALSE);
     return TRUE;
}

/* A helper for push_raw_frame() */
static inline int chunk_copy(SpiceGstEncoder *encoder, const SpiceBitmap *bitmap,
                             uint32_t chunk_offset, uint32_t len, uint8_t *dst)
{
    SpiceChunks *chunks = bitmap->data;
    uint32_t chunk_index = 0;
    /* Skip chunks until we find the start of the frame */
    while (chunk_index < chunks->num_chunks &&
           chunk_offset >= chunks->chunk[chunk_index].len) {
        if (!is_chunk_stride_aligned(bitmap, chunk_index)) {
            return FALSE;
        }
        chunk_offset -= chunks->chunk[chunk_index].len;
        chunk_index++;
    }

    /* We can copy the frame chunk by chunk */
    while (len && chunk_index < chunks->num_chunks) {
        if (!is_chunk_stride_aligned(bitmap, chunk_index)) {
            return FALSE;
        }
        uint8_t *src = chunks->chunk[chunk_index].data + chunk_offset;
        uint32_t thislen = MIN(chunks->chunk[chunk_index].len - chunk_offset, len);
        memcpy(dst, src, thislen);
        dst += thislen;
        len -= thislen;
        chunk_offset = 0;
        chunk_index++;
    }
    spice_return_val_if_fail(len == 0, FALSE);
    return TRUE;
}

/* A helper for spice_gst_encoder_encode_frame() */
static int push_raw_frame(SpiceGstEncoder *encoder, const SpiceBitmap *bitmap,
                          const SpiceRect *src, int top_down)
{
    uint32_t height = src->bottom - src->top;
    uint32_t stream_stride = (src->right - src->left) * encoder->format->bpp / 8;
    uint32_t len = stream_stride * height;
    GstBuffer *buffer = gst_buffer_new_and_alloc(len);
    GstMapInfo map;
    gst_buffer_map(buffer, &map, GST_MAP_WRITE);
    uint8_t *dst = map.data;

    /* Note that we should not reorder the lines, even if top_down is false.
     * It just changes the number of lines to skip at the start of the bitmap.
     */
    uint32_t skip_lines = top_down ? src->top : bitmap->y - (src->bottom - 0);
    uint32_t chunk_offset = bitmap->stride * skip_lines;

    if (stream_stride != bitmap->stride) {
        /* We have to do a line-by-line copy because for each we have to
         * leave out pixels on the left or right.
         */
        chunk_offset += src->left * encoder->format->bpp / 8;
        if (!line_copy(encoder, bitmap, chunk_offset, stream_stride, height, dst)) {
            gst_buffer_unmap(buffer, &map);
            gst_buffer_unref(buffer);
            return VIDEO_ENCODER_FRAME_UNSUPPORTED;
        }
    } else {
        /* We can copy the bitmap chunk by chunk */
        if (!chunk_copy(encoder, bitmap, chunk_offset, len, dst)) {
            gst_buffer_unmap(buffer, &map);
            gst_buffer_unref(buffer);
            return VIDEO_ENCODER_FRAME_UNSUPPORTED;
        }
    }
    gst_buffer_unmap(buffer, &map);

    GstFlowReturn ret = gst_app_src_push_buffer(encoder->appsrc, buffer);
    if (ret != GST_FLOW_OK) {
        spice_warning("GStreamer error: unable to push source buffer (%d)", ret);
        return VIDEO_ENCODER_FRAME_UNSUPPORTED;
    }

    return VIDEO_ENCODER_FRAME_ENCODE_DONE;
}

/* A helper for spice_gst_encoder_encode_frame() */
static int pull_compressed_buffer(SpiceGstEncoder *encoder,
                                  VideoBuffer **outbuf)
{
    g_mutex_lock(&encoder->outbuf_mutex);
    while (!encoder->outbuf) {
        g_cond_wait(&encoder->outbuf_cond, &encoder->outbuf_mutex);
    }
    *outbuf = encoder->outbuf;
    encoder->outbuf = NULL;
    g_mutex_unlock(&encoder->outbuf_mutex);

    if ((*outbuf)->data) {
        return VIDEO_ENCODER_FRAME_ENCODE_DONE;
    }

    spice_debug("failed to pull the compressed buffer");
    (*outbuf)->free(*outbuf);
    *outbuf = NULL;
    return VIDEO_ENCODER_FRAME_UNSUPPORTED;
}


/* ---------- VideoEncoder's public API ---------- */

static void spice_gst_encoder_destroy(VideoEncoder *video_encoder)
{
    SpiceGstEncoder *encoder = (SpiceGstEncoder*)video_encoder;

    free_pipeline(encoder);
    g_mutex_clear(&encoder->outbuf_mutex);
    g_cond_clear(&encoder->outbuf_cond);

    free(encoder);
}

static int spice_gst_encoder_encode_frame(VideoEncoder *video_encoder,
                                          uint32_t frame_mm_time,
                                          const SpiceBitmap *bitmap,
                                          const SpiceRect *src, int top_down,
                                          VideoBuffer **outbuf)
{
    SpiceGstEncoder *encoder = (SpiceGstEncoder*)video_encoder;
    g_return_val_if_fail(outbuf != NULL, VIDEO_ENCODER_FRAME_UNSUPPORTED);
    *outbuf = NULL;

    uint32_t width = src->right - src->left;
    uint32_t height = src->bottom - src->top;
    if (width != encoder->width || height != encoder->height ||
        encoder->spice_format != bitmap->format) {
        spice_debug("video format change: width %d -> %d, height %d -> %d, format %d -> %d",
                    encoder->width, width, encoder->height, height,
                    encoder->spice_format, bitmap->format);
        encoder->format = map_format(bitmap->format);
        if (!encoder->format) {
            spice_warning("unable to map format type %d", bitmap->format);
            return VIDEO_ENCODER_FRAME_UNSUPPORTED;
        }
        encoder->spice_format = bitmap->format;
        encoder->width = width;
        encoder->height = height;
        if (encoder->pipeline) {
            set_pipeline_changes(encoder, SPICE_GST_VIDEO_PIPELINE_CAPS);
        }
    }

    if (!configure_pipeline(encoder)) {
        return VIDEO_ENCODER_FRAME_UNSUPPORTED;
    }

    int rc = push_raw_frame(encoder, bitmap, src, top_down);
    if (rc == VIDEO_ENCODER_FRAME_ENCODE_DONE) {
        rc = pull_compressed_buffer(encoder, outbuf);
        if (rc != VIDEO_ENCODER_FRAME_ENCODE_DONE) {
            /* The input buffer will be stuck in the pipeline, preventing
             * later ones from being processed. Furthermore something went
             * wrong with this pipeline, so it may be safer to rebuild it
             * from scratch.
             */
            free_pipeline(encoder);
        }
    }
    return rc;
}

static void spice_gst_encoder_client_stream_report(VideoEncoder *video_encoder,
                                             uint32_t num_frames,
                                             uint32_t num_drops,
                                             uint32_t start_frame_mm_time,
                                             uint32_t end_frame_mm_time,
                                             int32_t end_frame_delay,
                                             uint32_t audio_delay)
{
    spice_debug("client report: #frames %u, #drops %d, duration %u video-delay %d audio-delay %u",
                num_frames, num_drops,
                end_frame_mm_time - start_frame_mm_time,
                end_frame_delay, audio_delay);
}

static void spice_gst_encoder_notify_server_frame_drop(VideoEncoder *video_encoder)
{
    spice_debug("server report: getting frame drops...");
}

static uint64_t spice_gst_encoder_get_bit_rate(VideoEncoder *video_encoder)
{
    SpiceGstEncoder *encoder = (SpiceGstEncoder*)video_encoder;
    return encoder->bit_rate;
}

static void spice_gst_encoder_get_stats(VideoEncoder *video_encoder,
                                        VideoEncoderStats *stats)
{
    SpiceGstEncoder *encoder = (SpiceGstEncoder*)video_encoder;
    uint64_t raw_bit_rate = encoder->width * encoder->height * (encoder->format ? encoder->format->bpp : 0) * get_source_fps(encoder);

    spice_return_if_fail(stats != NULL);
    stats->starting_bit_rate = encoder->starting_bit_rate;
    stats->cur_bit_rate = encoder->bit_rate;

    /* Use the compression level as a proxy for the quality */
    stats->avg_quality = stats->cur_bit_rate ? 100.0 - raw_bit_rate / stats->cur_bit_rate : 0;
    if (stats->avg_quality < 0) {
        stats->avg_quality = 0;
    }
}

VideoEncoder *gstreamer_encoder_new(SpiceVideoCodecType codec_type,
                                    uint64_t starting_bit_rate,
                                    VideoEncoderRateControlCbs *cbs)
{
    spice_return_val_if_fail(codec_type == SPICE_VIDEO_CODEC_TYPE_MJPEG ||
                             codec_type == SPICE_VIDEO_CODEC_TYPE_VP8, NULL);

    GError *err = NULL;
    if (!gst_init_check(NULL, NULL, &err)) {
        spice_warning("GStreamer error: %s", err->message);
        g_clear_error(&err);
        return NULL;
    }

    SpiceGstEncoder *encoder = spice_new0(SpiceGstEncoder, 1);
    encoder->base.destroy = spice_gst_encoder_destroy;
    encoder->base.encode_frame = spice_gst_encoder_encode_frame;
    encoder->base.client_stream_report = spice_gst_encoder_client_stream_report;
    encoder->base.notify_server_frame_drop = spice_gst_encoder_notify_server_frame_drop;
    encoder->base.get_bit_rate = spice_gst_encoder_get_bit_rate;
    encoder->base.get_stats = spice_gst_encoder_get_stats;
    encoder->base.codec_type = codec_type;

    if (cbs) {
        encoder->cbs = *cbs;
    }
    encoder->starting_bit_rate = starting_bit_rate;
    g_mutex_init(&encoder->outbuf_mutex);
    g_cond_init(&encoder->outbuf_cond);

    /* All the other fields are initialized to zero by spice_new0(). */

    if (!create_pipeline(encoder)) {
        /* Some GStreamer dependency is probably missing */
        free(encoder);
        encoder = NULL;
    }
    return (VideoEncoder*)encoder;
}
