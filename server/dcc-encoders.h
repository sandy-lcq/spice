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
#include "common/marshaller.h"
#include "common/quic.h"
#include "red_channel.h"
#include "red_parse_qxl.h"
#include "spice_image_cache.h"
#include "glz_encoder_dictionary.h"
#include "glz_encoder.h"
#include "jpeg_encoder.h"
#ifdef USE_LZ4
#include "lz4_encoder.h"
#endif
#include "zlib_encoder.h"

typedef struct RedCompressBuf RedCompressBuf;
typedef struct GlzDrawableInstanceItem GlzDrawableInstanceItem;
typedef struct RedGlzDrawable RedGlzDrawable;


void             dcc_encoders_init                           (DisplayChannelClient *dcc);
void             dcc_encoders_free                           (DisplayChannelClient *dcc);
void             dcc_free_glz_drawable_instance              (DisplayChannelClient *dcc,
                                                              GlzDrawableInstanceItem *item);
void             dcc_free_glz_drawable                       (DisplayChannelClient *dcc,
                                                              RedGlzDrawable *drawable);
void             dcc_free_glz_drawables                      (DisplayChannelClient *dcc);
void             dcc_free_glz_drawables_to_free              (DisplayChannelClient* dcc);
void             dcc_freeze_glz                              (DisplayChannelClient *dcc);

void             marshaller_add_compressed                   (SpiceMarshaller *m,
                                                              RedCompressBuf *comp_buf,
                                                              size_t size);

RedCompressBuf*  compress_buf_new                            (void);
void             compress_buf_free                           (RedCompressBuf *buf);

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

typedef struct GlzSharedDictionary {
    RingItem base;
    GlzEncDictContext *dict;
    uint32_t refs;
    uint8_t id;
    pthread_rwlock_t encode_lock;
    int migrate_freeze;
    RedClient *client; // channel clients of the same client share the dict
} GlzSharedDictionary;

typedef struct  {
    DisplayChannelClient *dcc;
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
    Drawable    *drawable;
    uint32_t     group_id;
    GlzDrawableInstanceItem instances_pool[MAX_GLZ_DRAWABLE_INSTANCES];
    Ring instances;
    uint8_t instances_count;
    DisplayChannelClient *dcc;
};


#endif /* DCC_ENCODERS_H_ */
