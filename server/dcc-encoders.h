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
#ifndef DCC_ENCODERS_H_
#define DCC_ENCODERS_H_

#include <setjmp.h>
#include <common/quic.h>

#include "red-channel.h"
#include "red-parse-qxl.h"
#include "image-cache.h"
#include "glz-encoder.h"
#include "jpeg-encoder.h"
#ifdef USE_LZ4
#include "lz4-encoder.h"
#endif
#include "zlib-encoder.h"

typedef struct RedCompressBuf RedCompressBuf;
typedef struct GlzDrawableInstanceItem GlzDrawableInstanceItem;
typedef struct RedGlzDrawable RedGlzDrawable;
typedef struct ImageEncoders ImageEncoders;
typedef struct ImageEncoderSharedData ImageEncoderSharedData;

void image_encoder_shared_init(ImageEncoderSharedData *shared_data);
void image_encoder_shared_stat_reset(ImageEncoderSharedData *shared_data);
void image_encoder_shared_stat_print(const ImageEncoderSharedData *shared_data);

void image_encoders_init(ImageEncoders *enc, ImageEncoderSharedData *shared_data);
void image_encoders_free(ImageEncoders *enc);
void image_encoders_free_glz_drawable(ImageEncoders *enc, RedGlzDrawable *drawable);
int image_encoders_free_some_independent_glz_drawables(ImageEncoders *enc);
void image_encoders_free_glz_drawables(ImageEncoders *enc);
void image_encoders_free_glz_drawables_to_free(ImageEncoders* enc);
gboolean image_encoders_glz_create(ImageEncoders *enc, uint8_t id);
void image_encoders_glz_get_restore_data(ImageEncoders *enc,
                                         uint8_t *out_id, GlzEncDictRestoreData *out_data);

#define RED_COMPRESS_BUF_SIZE (1024 * 64)
struct RedCompressBuf {
    /* This buffer provide space for compression algorithms.
     * Some algorithms access the buffer as an array of 32 bit words
     * so is defined to make sure is always aligned that way.
     */
    union {
        uint8_t  bytes[RED_COMPRESS_BUF_SIZE];
        uint32_t words[RED_COMPRESS_BUF_SIZE / 4];
    } buf;
    RedCompressBuf *send_next;
};

static inline void compress_buf_free(RedCompressBuf *buf)
{
    g_free(buf);
}

typedef struct GlzSharedDictionary {
    RingItem base;
    GlzEncDictContext *dict;
    uint32_t refs;
    uint8_t id;
    pthread_rwlock_t encode_lock;
    int migrate_freeze;
    RedClient *client; // channel clients of the same client share the dict
} GlzSharedDictionary;

gboolean image_encoders_get_glz_dictionary(ImageEncoders *enc,
                                           struct RedClient *client,
                                           uint8_t id, int window_size);
gboolean image_encoders_restore_glz_dictionary(ImageEncoders *enc,
                                               struct RedClient *client,
                                               uint8_t id,
                                               GlzEncDictRestoreData *restore_data);

typedef struct  {
    RedCompressBuf *bufs_head;
    RedCompressBuf *bufs_tail;
    jmp_buf jmp_env;
    union {
        struct {
            SpiceChunks *chunks;
            int next;
            int stride;
            int reverse;
        } lines_data;
        struct {
            RedCompressBuf* next;
            int size_left;
        } compressed_data; // for encoding data that was already compressed by another method
    } u;
    char message_buf[512];
} EncoderData;

void encoder_data_init(EncoderData *data);
void encoder_data_reset(EncoderData *data);

typedef struct {
    QuicUsrContext usr;
    EncoderData data;
} QuicData;

typedef struct {
    LzUsrContext usr;
    EncoderData data;
} LzData;

typedef struct {
    JpegEncoderUsrContext usr;
    EncoderData data;
} JpegData;

#ifdef USE_LZ4
typedef struct {
    Lz4EncoderUsrContext usr;
    EncoderData data;
} Lz4Data;
#endif

typedef struct {
    ZlibEncoderUsrContext usr;
    EncoderData data;
} ZlibData;

typedef struct {
    GlzEncoderUsrContext usr;
    EncoderData data;
} GlzData;

#define MAX_GLZ_DRAWABLE_INSTANCES 2

/* for each qxl drawable, there may be several instances of lz drawables */
/* TODO - reuse this stuff for the top level. I just added a second level of multiplicity
 * at the Drawable by keeping a ring, so:
 * Drawable -> (ring of) RedGlzDrawable -> (up to 2) GlzDrawableInstanceItem
 * and it should probably (but need to be sure...) be
 * Drawable -> ring of GlzDrawableInstanceItem.
 */
struct GlzDrawableInstanceItem {
    RingItem glz_link;
    RingItem free_link;
    GlzEncDictImageContext *context;
    RedGlzDrawable         *glz_drawable;
};

struct RedGlzDrawable {
    RingItem link;    // ordered by the time it was encoded
    RingItem drawable_link;
    RedDrawable *red_drawable;
    GlzDrawableInstanceItem instances_pool[MAX_GLZ_DRAWABLE_INSTANCES];
    Ring instances;
    uint8_t instances_count;
    gboolean has_drawable;
    ImageEncoders *encoders;
};

struct ImageEncoderSharedData {
    uint32_t glz_drawable_count;

    stat_info_t off_stat;
    stat_info_t lz_stat;
    stat_info_t glz_stat;
    stat_info_t quic_stat;
    stat_info_t jpeg_stat;
    stat_info_t zlib_glz_stat;
    stat_info_t jpeg_alpha_stat;
    stat_info_t lz4_stat;
};

struct ImageEncoders {
    ImageEncoderSharedData *shared_data;

    QuicData quic_data;
    QuicContext *quic;

    LzData lz_data;
    LzContext  *lz;

    int jpeg_quality;

    JpegData jpeg_data;
    JpegEncoderContext *jpeg;

#ifdef USE_LZ4
    Lz4Data lz4_data;
    Lz4EncoderContext *lz4;
#endif

    int zlib_level;

    ZlibData zlib_data;
    ZlibEncoder *zlib;

    /* global lz encoding entities */
    GlzSharedDictionary *glz_dict;
    GlzEncoderContext *glz;
    GlzData glz_data;

    Ring glz_drawables;               // all the living lz drawable, ordered by encoding time
    Ring glz_drawables_inst_to_free;               // list of instances to be freed
    pthread_mutex_t glz_drawables_inst_to_free_lock;
};

typedef struct compress_send_data_t {
    void* comp_buf;
    uint32_t comp_buf_size;
    SpicePalette *lzplt_palette;
    int is_lossy;
} compress_send_data_t;

int image_encoders_compress_quic(ImageEncoders *enc, SpiceImage *dest,
                                 SpiceBitmap *src, compress_send_data_t* o_comp_data);
int image_encoders_compress_lz(ImageEncoders *enc,
                               SpiceImage *dest, SpiceBitmap *src,
                               compress_send_data_t* o_comp_data);
int image_encoders_compress_jpeg(ImageEncoders *enc, SpiceImage *dest,
                                 SpiceBitmap *src, compress_send_data_t* o_comp_data);
int image_encoders_compress_lz4(ImageEncoders *enc, SpiceImage *dest,
                                SpiceBitmap *src, compress_send_data_t* o_comp_data);
int image_encoders_compress_glz(ImageEncoders *enc,
                                SpiceImage *dest, SpiceBitmap *src, struct Drawable *drawable,
                                compress_send_data_t* o_comp_data,
                                gboolean enable_zlib_glz_wrap);

#define RED_RELEASE_BUNCH_SIZE 64

#endif /* DCC_ENCODERS_H_ */
