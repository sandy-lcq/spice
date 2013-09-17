/* -*- Mode: C; c-basic-offset: 4; indent-tabs-mode: nil -*- */
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

#include <glib.h>

#include "dcc-encoders.h"
#include "display-channel.h"

#define ZLIB_DEFAULT_COMPRESSION_LEVEL 3

static SPICE_GNUC_NORETURN SPICE_GNUC_PRINTF(2, 3) void
quic_usr_error(QuicUsrContext *usr, const char *fmt, ...)
{
    EncoderData *usr_data = &(((QuicData *)usr)->data);
    va_list ap;

    va_start(ap, fmt);
    vsnprintf(usr_data->message_buf, sizeof(usr_data->message_buf), fmt, ap);
    va_end(ap);
    spice_critical("%s", usr_data->message_buf);

    longjmp(usr_data->jmp_env, 1);
}

static SPICE_GNUC_NORETURN SPICE_GNUC_PRINTF(2, 3) void
lz_usr_error(LzUsrContext *usr, const char *fmt, ...)
{
    EncoderData *usr_data = &(((LzData *)usr)->data);
    va_list ap;

    va_start(ap, fmt);
    vsnprintf(usr_data->message_buf, sizeof(usr_data->message_buf), fmt, ap);
    va_end(ap);
    spice_critical("%s", usr_data->message_buf);

    longjmp(usr_data->jmp_env, 1);
}

static SPICE_GNUC_PRINTF(2, 3) void
glz_usr_error(GlzEncoderUsrContext *usr, const char *fmt, ...)
{
    EncoderData *usr_data = &(((GlzData *)usr)->data);
    va_list ap;

    va_start(ap, fmt);
    vsnprintf(usr_data->message_buf, sizeof(usr_data->message_buf), fmt, ap);
    va_end(ap);

    spice_critical("%s", usr_data->message_buf); // if global lz fails in the middle
                                        // the consequences are not predictable since the window
                                        // can turn to be unsynchronized between the server and
                                        // and the client
}

static SPICE_GNUC_PRINTF(2, 3) void
quic_usr_warn(QuicUsrContext *usr, const char *fmt, ...)
{
    EncoderData *usr_data = &(((QuicData *)usr)->data);
    va_list ap;

    va_start(ap, fmt);
    vsnprintf(usr_data->message_buf, sizeof(usr_data->message_buf), fmt, ap);
    va_end(ap);
    spice_warning("%s", usr_data->message_buf);
}

static SPICE_GNUC_PRINTF(2, 3) void
lz_usr_warn(LzUsrContext *usr, const char *fmt, ...)
{
    EncoderData *usr_data = &(((LzData *)usr)->data);
    va_list ap;

    va_start(ap, fmt);
    vsnprintf(usr_data->message_buf, sizeof(usr_data->message_buf), fmt, ap);
    va_end(ap);
    spice_warning("%s", usr_data->message_buf);
}

static SPICE_GNUC_PRINTF(2, 3) void
glz_usr_warn(GlzEncoderUsrContext *usr, const char *fmt, ...)
{
    EncoderData *usr_data = &(((GlzData *)usr)->data);
    va_list ap;

    va_start(ap, fmt);
    vsnprintf(usr_data->message_buf, sizeof(usr_data->message_buf), fmt, ap);
    va_end(ap);
    spice_warning("%s", usr_data->message_buf);
}

static void *quic_usr_malloc(QuicUsrContext *usr, int size)
{
    return spice_malloc(size);
}

static void *lz_usr_malloc(LzUsrContext *usr, int size)
{
    return spice_malloc(size);
}

static void *glz_usr_malloc(GlzEncoderUsrContext *usr, int size)
{
    return spice_malloc(size);
}

static void quic_usr_free(QuicUsrContext *usr, void *ptr)
{
    free(ptr);
}

static void lz_usr_free(LzUsrContext *usr, void *ptr)
{
    free(ptr);
}

static void glz_usr_free(GlzEncoderUsrContext *usr, void *ptr)
{
    free(ptr);
}

RedCompressBuf* compress_buf_new(void)
{
    RedCompressBuf *buf = g_slice_new(RedCompressBuf);

    buf->send_next = NULL;

    return buf;
}

void compress_buf_free(RedCompressBuf *buf)
{
    g_slice_free(RedCompressBuf, buf);
}

/* Allocate more space for compressed buffer.
 * The pointer returned in io_ptr is garanteed to be aligned to 4 bytes.
 */
static int encoder_usr_more_space(EncoderData *enc_data, uint8_t **io_ptr)
{
    RedCompressBuf *buf;

    buf = compress_buf_new();
    enc_data->bufs_tail->send_next = buf;
    enc_data->bufs_tail = buf;
    buf->send_next = NULL;
    *io_ptr = buf->buf.bytes;
    return sizeof(buf->buf);
}

static int quic_usr_more_space(QuicUsrContext *usr, uint32_t **io_ptr, int rows_completed)
{
    EncoderData *usr_data = &(((QuicData *)usr)->data);
    return encoder_usr_more_space(usr_data, (uint8_t **)io_ptr) / sizeof(uint32_t);
}

static int lz_usr_more_space(LzUsrContext *usr, uint8_t **io_ptr)
{
    EncoderData *usr_data = &(((LzData *)usr)->data);
    return encoder_usr_more_space(usr_data, io_ptr);
}

static int glz_usr_more_space(GlzEncoderUsrContext *usr, uint8_t **io_ptr)
{
    EncoderData *usr_data = &(((GlzData *)usr)->data);
    return encoder_usr_more_space(usr_data, io_ptr);
}

static int jpeg_usr_more_space(JpegEncoderUsrContext *usr, uint8_t **io_ptr)
{
    EncoderData *usr_data = &(((JpegData *)usr)->data);
    return encoder_usr_more_space(usr_data, io_ptr);
}

#ifdef USE_LZ4
static int lz4_usr_more_space(Lz4EncoderUsrContext *usr, uint8_t **io_ptr)
{
    EncoderData *usr_data = &(((Lz4Data *)usr)->data);
    return encoder_usr_more_space(usr_data, io_ptr);
}
#endif

static int zlib_usr_more_space(ZlibEncoderUsrContext *usr, uint8_t **io_ptr)
{
    EncoderData *usr_data = &(((ZlibData *)usr)->data);
    return encoder_usr_more_space(usr_data, io_ptr);
}

static inline int encoder_usr_more_lines(EncoderData *enc_data, uint8_t **lines)
{
    struct SpiceChunk *chunk;

    if (enc_data->u.lines_data.reverse) {
        if (!(enc_data->u.lines_data.next >= 0)) {
            return 0;
        }
    } else {
        if (!(enc_data->u.lines_data.next < enc_data->u.lines_data.chunks->num_chunks)) {
            return 0;
        }
    }

    chunk = &enc_data->u.lines_data.chunks->chunk[enc_data->u.lines_data.next];
    if (chunk->len % enc_data->u.lines_data.stride) {
        return 0;
    }

    if (enc_data->u.lines_data.reverse) {
        enc_data->u.lines_data.next--;
        *lines = chunk->data + chunk->len - enc_data->u.lines_data.stride;
    } else {
        enc_data->u.lines_data.next++;
        *lines = chunk->data;
    }

    return chunk->len / enc_data->u.lines_data.stride;
}

static int quic_usr_more_lines(QuicUsrContext *usr, uint8_t **lines)
{
    EncoderData *usr_data = &(((QuicData *)usr)->data);
    return encoder_usr_more_lines(usr_data, lines);
}

static int lz_usr_more_lines(LzUsrContext *usr, uint8_t **lines)
{
    EncoderData *usr_data = &(((LzData *)usr)->data);
    return encoder_usr_more_lines(usr_data, lines);
}

static int glz_usr_more_lines(GlzEncoderUsrContext *usr, uint8_t **lines)
{
    EncoderData *usr_data = &(((GlzData *)usr)->data);
    return encoder_usr_more_lines(usr_data, lines);
}

static int jpeg_usr_more_lines(JpegEncoderUsrContext *usr, uint8_t **lines)
{
    EncoderData *usr_data = &(((JpegData *)usr)->data);
    return encoder_usr_more_lines(usr_data, lines);
}

#ifdef USE_LZ4
static int lz4_usr_more_lines(Lz4EncoderUsrContext *usr, uint8_t **lines)
{
    EncoderData *usr_data = &(((Lz4Data *)usr)->data);
    return encoder_usr_more_lines(usr_data, lines);
}
#endif

static int zlib_usr_more_input(ZlibEncoderUsrContext *usr, uint8_t** input)
{
    EncoderData *usr_data = &(((ZlibData *)usr)->data);
    int buf_size;

    if (!usr_data->u.compressed_data.next) {
        spice_assert(usr_data->u.compressed_data.size_left == 0);
        return 0;
    }

    *input = usr_data->u.compressed_data.next->buf.bytes;
    buf_size = MIN(sizeof(usr_data->u.compressed_data.next->buf),
                   usr_data->u.compressed_data.size_left);

    usr_data->u.compressed_data.next = usr_data->u.compressed_data.next->send_next;
    usr_data->u.compressed_data.size_left -= buf_size;
    return buf_size;
}

static void dcc_init_quic(DisplayChannelClient *dcc)
{
    dcc->quic_data.usr.error = quic_usr_error;
    dcc->quic_data.usr.warn = quic_usr_warn;
    dcc->quic_data.usr.info = quic_usr_warn;
    dcc->quic_data.usr.malloc = quic_usr_malloc;
    dcc->quic_data.usr.free = quic_usr_free;
    dcc->quic_data.usr.more_space = quic_usr_more_space;
    dcc->quic_data.usr.more_lines = quic_usr_more_lines;

    dcc->quic = quic_create(&dcc->quic_data.usr);

    if (!dcc->quic) {
        spice_critical("create quic failed");
    }
}

static void dcc_init_lz(DisplayChannelClient *dcc)
{
    dcc->lz_data.usr.error = lz_usr_error;
    dcc->lz_data.usr.warn = lz_usr_warn;
    dcc->lz_data.usr.info = lz_usr_warn;
    dcc->lz_data.usr.malloc = lz_usr_malloc;
    dcc->lz_data.usr.free = lz_usr_free;
    dcc->lz_data.usr.more_space = lz_usr_more_space;
    dcc->lz_data.usr.more_lines = lz_usr_more_lines;

    dcc->lz = lz_create(&dcc->lz_data.usr);

    if (!dcc->lz) {
        spice_critical("create lz failed");
    }
}

static void glz_usr_free_image(GlzEncoderUsrContext *usr, GlzUsrImageContext *image)
{
    GlzData *lz_data = (GlzData *)usr;
    GlzDrawableInstanceItem *glz_drawable_instance = (GlzDrawableInstanceItem *)image;
    DisplayChannelClient *drawable_cc = glz_drawable_instance->glz_drawable->dcc;
    DisplayChannelClient *this_cc = SPICE_CONTAINEROF(lz_data, DisplayChannelClient, glz_data);
    if (this_cc == drawable_cc) {
        dcc_free_glz_drawable_instance(drawable_cc, glz_drawable_instance);
    } else {
        /* The glz dictionary is shared between all DisplayChannelClient
         * instances that belong to the same client, and glz_usr_free_image
         * can be called by the dictionary code
         * (glz_dictionary_window_remove_head). Thus this function can be
         * called from any DisplayChannelClient thread, hence the need for
         * this check.
         */
        pthread_mutex_lock(&drawable_cc->glz_drawables_inst_to_free_lock);
        ring_add_before(&glz_drawable_instance->free_link,
                        &drawable_cc->glz_drawables_inst_to_free);
        pthread_mutex_unlock(&drawable_cc->glz_drawables_inst_to_free_lock);
    }
}

static void dcc_init_glz_data(DisplayChannelClient *dcc)
{
    dcc->glz_data.usr.error = glz_usr_error;
    dcc->glz_data.usr.warn = glz_usr_warn;
    dcc->glz_data.usr.info = glz_usr_warn;
    dcc->glz_data.usr.malloc = glz_usr_malloc;
    dcc->glz_data.usr.free = glz_usr_free;
    dcc->glz_data.usr.more_space = glz_usr_more_space;
    dcc->glz_data.usr.more_lines = glz_usr_more_lines;
    dcc->glz_data.usr.free_image = glz_usr_free_image;
}

static void dcc_init_jpeg(DisplayChannelClient *dcc)
{
    dcc->jpeg_data.usr.more_space = jpeg_usr_more_space;
    dcc->jpeg_data.usr.more_lines = jpeg_usr_more_lines;

    dcc->jpeg = jpeg_encoder_create(&dcc->jpeg_data.usr);

    if (!dcc->jpeg) {
        spice_critical("create jpeg encoder failed");
    }
}

#ifdef USE_LZ4
static inline void dcc_init_lz4(DisplayChannelClient *dcc)
{
    dcc->lz4_data.usr.more_space = lz4_usr_more_space;
    dcc->lz4_data.usr.more_lines = lz4_usr_more_lines;

    dcc->lz4 = lz4_encoder_create(&dcc->lz4_data.usr);

    if (!dcc->lz4) {
        spice_critical("create lz4 encoder failed");
    }
}
#endif

static void dcc_init_zlib(DisplayChannelClient *dcc)
{
    dcc->zlib_data.usr.more_space = zlib_usr_more_space;
    dcc->zlib_data.usr.more_input = zlib_usr_more_input;

    dcc->zlib = zlib_encoder_create(&dcc->zlib_data.usr, ZLIB_DEFAULT_COMPRESSION_LEVEL);

    if (!dcc->zlib) {
        spice_critical("create zlib encoder failed");
    }
}

void dcc_encoders_init(DisplayChannelClient *dcc)
{
    dcc_init_glz_data(dcc);
    dcc_init_quic(dcc);
    dcc_init_lz(dcc);
    dcc_init_jpeg(dcc);
#ifdef USE_LZ4
    dcc_init_lz4(dcc);
#endif
    dcc_init_zlib(dcc);

    // todo: tune level according to bandwidth
    dcc->zlib_level = ZLIB_DEFAULT_COMPRESSION_LEVEL;
}

static void marshaller_compress_buf_free(uint8_t *data, void *opaque)
{
    compress_buf_free((RedCompressBuf *) opaque);
}

void marshaller_add_compressed(SpiceMarshaller *m,
                               RedCompressBuf *comp_buf, size_t size)
{
    size_t max = size;
    size_t now;
    do {
        spice_return_if_fail(comp_buf);
        now = MIN(sizeof(comp_buf->buf), max);
        max -= now;
        spice_marshaller_add_ref_full(m, comp_buf->buf.bytes, now,
                                      marshaller_compress_buf_free, comp_buf);
        comp_buf = comp_buf->send_next;
    } while (max);
}
