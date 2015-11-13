/*
   Copyright (C) 2009-2015 Red Hat, Inc.

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

#include "stream.h"
#include "display-channel.h"

void stream_agent_stats_print(StreamAgent *agent)
{
#ifdef STREAM_STATS
    StreamStats *stats = &agent->stats;
    double passed_mm_time = (stats->end - stats->start) / 1000.0;
    MJpegEncoderStats encoder_stats = {0};

    if (agent->mjpeg_encoder) {
        mjpeg_encoder_get_stats(agent->mjpeg_encoder, &encoder_stats);
    }

    spice_debug("stream=%p dim=(%dx%d) #in-frames=%lu #in-avg-fps=%.2f #out-frames=%lu "
                "out/in=%.2f #drops=%lu (#pipe=%lu #fps=%lu) out-avg-fps=%.2f "
                "passed-mm-time(sec)=%.2f size-total(MB)=%.2f size-per-sec(Mbps)=%.2f "
                "size-per-frame(KBpf)=%.2f avg-quality=%.2f "
                "start-bit-rate(Mbps)=%.2f end-bit-rate(Mbps)=%.2f",
                agent, agent->stream->width, agent->stream->height,
                stats->num_input_frames,
                stats->num_input_frames / passed_mm_time,
                stats->num_frames_sent,
                (stats->num_frames_sent + 0.0) / stats->num_input_frames,
                stats->num_drops_pipe +
                stats->num_drops_fps,
                stats->num_drops_pipe,
                stats->num_drops_fps,
                stats->num_frames_sent / passed_mm_time,
                passed_mm_time,
                stats->size_sent / 1024.0 / 1024.0,
                ((stats->size_sent * 8.0) / (1024.0 * 1024)) / passed_mm_time,
                stats->size_sent / 1000.0 / stats->num_frames_sent,
                encoder_stats.avg_quality,
                encoder_stats.starting_bit_rate / (1024.0 * 1024),
                encoder_stats.cur_bit_rate / (1024.0 * 1024));
#endif
}

void stream_stop(DisplayChannel *display, Stream *stream)
{
    DisplayChannelClient *dcc;
    RingItem *item, *next;

    spice_return_if_fail(ring_item_is_linked(&stream->link));
    spice_return_if_fail(!stream->current);

    spice_debug("stream %d", get_stream_id(display, stream));
    FOREACH_DCC(display, item, next, dcc) {
        StreamAgent *stream_agent;

        stream_agent = &dcc->stream_agents[get_stream_id(display, stream)];
        region_clear(&stream_agent->vis_region);
        region_clear(&stream_agent->clip);
        spice_assert(!pipe_item_is_linked(&stream_agent->destroy_item));
        if (stream_agent->mjpeg_encoder && dcc->use_mjpeg_encoder_rate_control) {
            uint64_t stream_bit_rate = mjpeg_encoder_get_bit_rate(stream_agent->mjpeg_encoder);

            if (stream_bit_rate > dcc->streams_max_bit_rate) {
                spice_debug("old max-bit-rate=%.2f new=%.2f",
                dcc->streams_max_bit_rate / 8.0 / 1024.0 / 1024.0,
                stream_bit_rate / 8.0 / 1024.0 / 1024.0);
                dcc->streams_max_bit_rate = stream_bit_rate;
            }
        }
        stream->refs++;
        red_channel_client_pipe_add(RED_CHANNEL_CLIENT(dcc), &stream_agent->destroy_item);
        stream_agent_stats_print(stream_agent);
    }
    display->streams_size_total -= stream->width * stream->height;
    ring_remove(&stream->link);
    stream_unref(display, stream);
}

static void stream_free(DisplayChannel *display, Stream *stream)
{
    stream->next = display->free_streams;
    display->free_streams = stream;
}

void display_channel_init_streams(DisplayChannel *display)
{
    int i;

    ring_init(&display->streams);
    display->free_streams = NULL;
    for (i = 0; i < NUM_STREAMS; i++) {
        Stream *stream = &display->streams_buf[i];
        ring_item_init(&stream->link);
        stream_free(display, stream);
    }
}

void stream_unref(DisplayChannel *display, Stream *stream)
{
    if (--stream->refs != 0)
        return;

    spice_warn_if_fail(!ring_item_is_linked(&stream->link));

    stream_free(display, stream);
    display->stream_count--;
}

void stream_agent_unref(DisplayChannel *display, StreamAgent *agent)
{
    stream_unref(display, agent->stream);
}

StreamClipItem *stream_clip_item_new(DisplayChannelClient* dcc, StreamAgent *agent)
{
    StreamClipItem *item = spice_new(StreamClipItem, 1);
    red_channel_pipe_item_init(RED_CHANNEL_CLIENT(dcc)->channel,
                               (PipeItem *)item, PIPE_ITEM_TYPE_STREAM_CLIP);

    item->stream_agent = agent;
    agent->stream->refs++;
    item->refs = 1;
    return item;
}

static void dcc_update_streams_max_latency(DisplayChannelClient *dcc, StreamAgent *remove_agent)
{
    uint32_t new_max_latency = 0;
    int i;

    if (dcc->streams_max_latency != remove_agent->client_required_latency) {
        return;
    }

    dcc->streams_max_latency = 0;
    if (DCC_TO_DC(dcc)->stream_count == 1) {
        return;
    }
    for (i = 0; i < NUM_STREAMS; i++) {
        StreamAgent *other_agent = &dcc->stream_agents[i];
        if (other_agent == remove_agent || !other_agent->mjpeg_encoder) {
            continue;
        }
        if (other_agent->client_required_latency > new_max_latency) {
            new_max_latency = other_agent->client_required_latency;
        }
    }
    dcc->streams_max_latency = new_max_latency;
}

static uint64_t red_stream_get_initial_bit_rate(DisplayChannelClient *dcc,
                                                Stream *stream)
{
    char *env_bit_rate_str;
    uint64_t bit_rate = 0;

    env_bit_rate_str = getenv("SPICE_BIT_RATE");
    if (env_bit_rate_str != NULL) {
        double env_bit_rate;

        errno = 0;
        env_bit_rate = strtod(env_bit_rate_str, NULL);
        if (errno == 0) {
            bit_rate = env_bit_rate * 1024 * 1024;
        } else {
            spice_warning("error parsing SPICE_BIT_RATE: %s", strerror(errno));
        }
    }

    if (!bit_rate) {
        MainChannelClient *mcc;
        uint64_t net_test_bit_rate;

        mcc = red_client_get_main(RED_CHANNEL_CLIENT(dcc)->client);
        net_test_bit_rate = main_channel_client_is_network_info_initialized(mcc) ?
                                main_channel_client_get_bitrate_per_sec(mcc) :
                                0;
        bit_rate = MAX(dcc->streams_max_bit_rate, net_test_bit_rate);
        if (bit_rate == 0) {
            /*
             * In case we are after a spice session migration,
             * the low_bandwidth flag is retrieved from migration data.
             * If the network info is not initialized due to another reason,
             * the low_bandwidth flag is FALSE.
             */
            bit_rate = dcc->common.is_low_bandwidth ?
                RED_STREAM_DEFAULT_LOW_START_BIT_RATE :
                RED_STREAM_DEFAULT_HIGH_START_BIT_RATE;
        }
    }

    spice_debug("base-bit-rate %.2f (Mbps)", bit_rate / 1024.0 / 1024.0);
    /* dividing the available bandwidth among the active streams, and saving
     * (1-RED_STREAM_CHANNEL_CAPACITY) of it for other messages */
    return (RED_STREAM_CHANNEL_CAPACITY * bit_rate *
            stream->width * stream->height) / DCC_TO_DC(dcc)->streams_size_total;
}

static uint32_t red_stream_mjpeg_encoder_get_roundtrip(void *opaque)
{
    StreamAgent *agent = opaque;
    int roundtrip;

    roundtrip = red_channel_client_get_roundtrip_ms(RED_CHANNEL_CLIENT(agent->dcc));
    if (roundtrip < 0) {
        MainChannelClient *mcc = red_client_get_main(RED_CHANNEL_CLIENT(agent->dcc)->client);

        /*
         * the main channel client roundtrip might not have been
         * calculated (e.g., after migration). In such case,
         * main_channel_client_get_roundtrip_ms returns 0.
         */
        roundtrip = main_channel_client_get_roundtrip_ms(mcc);
    }

    return roundtrip;
}

static uint32_t red_stream_mjpeg_encoder_get_source_fps(void *opaque)
{
    StreamAgent *agent = opaque;

    return agent->stream->input_fps;
}

static void red_stream_update_client_playback_latency(void *opaque, uint32_t delay_ms)
{
    StreamAgent *agent = opaque;
    DisplayChannelClient *dcc = agent->dcc;

    dcc_update_streams_max_latency(dcc, agent);

    agent->client_required_latency = delay_ms;
    if (delay_ms > agent->dcc->streams_max_latency) {
        agent->dcc->streams_max_latency = delay_ms;
    }
    spice_debug("resetting client latency: %u", agent->dcc->streams_max_latency);
    main_dispatcher_set_mm_time_latency(RED_CHANNEL_CLIENT(agent->dcc)->client, agent->dcc->streams_max_latency);
}

void dcc_create_stream(DisplayChannelClient *dcc, Stream *stream)
{
    StreamAgent *agent = &dcc->stream_agents[get_stream_id(DCC_TO_DC(dcc), stream)];

    spice_return_if_fail(region_is_empty(&agent->vis_region));

    stream->refs++;
    if (stream->current) {
        agent->frames = 1;
        region_clone(&agent->vis_region, &stream->current->tree_item.base.rgn);
        region_clone(&agent->clip, &agent->vis_region);
    } else {
        agent->frames = 0;
    }
    agent->drops = 0;
    agent->fps = MAX_FPS;
    agent->dcc = dcc;

    if (dcc->use_mjpeg_encoder_rate_control) {
        MJpegEncoderRateControlCbs mjpeg_cbs;
        uint64_t initial_bit_rate;

        mjpeg_cbs.get_roundtrip_ms = red_stream_mjpeg_encoder_get_roundtrip;
        mjpeg_cbs.get_source_fps = red_stream_mjpeg_encoder_get_source_fps;
        mjpeg_cbs.update_client_playback_delay = red_stream_update_client_playback_latency;

        initial_bit_rate = red_stream_get_initial_bit_rate(dcc, stream);
        agent->mjpeg_encoder = mjpeg_encoder_new(initial_bit_rate, &mjpeg_cbs, agent);
    } else {
        agent->mjpeg_encoder = mjpeg_encoder_new(0, NULL, NULL);
    }
    red_channel_client_pipe_add(RED_CHANNEL_CLIENT(dcc), &agent->create_item);

    if (red_channel_client_test_remote_cap(RED_CHANNEL_CLIENT(dcc), SPICE_DISPLAY_CAP_STREAM_REPORT)) {
        StreamActivateReportItem *report_pipe_item = spice_malloc0(sizeof(*report_pipe_item));

        agent->report_id = rand();
        red_channel_pipe_item_init(RED_CHANNEL_CLIENT(dcc)->channel, &report_pipe_item->pipe_item,
                                   PIPE_ITEM_TYPE_STREAM_ACTIVATE_REPORT);
        report_pipe_item->stream_id = get_stream_id(DCC_TO_DC(dcc), stream);
        red_channel_client_pipe_add(RED_CHANNEL_CLIENT(dcc), &report_pipe_item->pipe_item);
    }
#ifdef STREAM_STATS
    memset(&agent->stats, 0, sizeof(StreamStats));
    if (stream->current) {
        agent->stats.start = stream->current->red_drawable->mm_time;
    }
#endif
}

void stream_agent_stop(DisplayChannelClient *dcc, StreamAgent *agent)
{
    dcc_update_streams_max_latency(dcc, agent);
    if (agent->mjpeg_encoder) {
        mjpeg_encoder_destroy(agent->mjpeg_encoder);
        agent->mjpeg_encoder = NULL;
    }
}
