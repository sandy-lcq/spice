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
#ifndef DCC_H_
# define DCC_H_

#include "red-worker.h"
#include "pixmap-cache.h"
#include "cache-item.h"
#include "dcc-encoders.h"
#include "stream.h"
#include "display-limits.h"

#define PALETTE_CACHE_HASH_SHIFT 8
#define PALETTE_CACHE_HASH_SIZE (1 << PALETTE_CACHE_HASH_SHIFT)
#define PALETTE_CACHE_HASH_MASK (PALETTE_CACHE_HASH_SIZE - 1)
#define PALETTE_CACHE_HASH_KEY(id) ((id) & PALETTE_CACHE_HASH_MASK)
#define CLIENT_PALETTE_CACHE_SIZE 128

#define DISPLAY_CLIENT_MIGRATE_DATA_TIMEOUT (NSEC_PER_SEC * 10)
#define DISPLAY_CLIENT_RETRY_INTERVAL 10000 //micro

/* Each drawable can refer to at most 3 images: src, brush and mask */
#define MAX_DRAWABLE_PIXMAP_CACHE_ITEMS 3

#define WIDE_CLIENT_ACK_WINDOW 40
#define NARROW_CLIENT_ACK_WINDOW 20

#define MAX_PIPE_SIZE 50

typedef struct WaitForChannels {
    SpiceMsgWaitForChannels header;
    SpiceWaitForChannel buf[MAX_CACHE_CLIENTS];
} WaitForChannels;

typedef struct FreeList {
    int res_size;
    SpiceResourceList *res;
    uint64_t sync[MAX_CACHE_CLIENTS];
    WaitForChannels wait;
} FreeList;

struct DisplayChannelClient {
    CommonGraphicsChannelClient common;
    uint32_t id;
    SpiceImageCompression image_compression;
    spice_wan_compression_t jpeg_state;
    spice_wan_compression_t zlib_glz_state;
    int jpeg_quality;
    int zlib_level;

    QuicData quic_data;
    QuicContext *quic;
    LzData lz_data;
    LzContext  *lz;
    JpegData jpeg_data;
    JpegEncoderContext *jpeg;
#ifdef USE_LZ4
    Lz4Data lz4_data;
    Lz4EncoderContext *lz4;
#endif
    ZlibData zlib_data;
    ZlibEncoder *zlib;

    int expect_init;

    PixmapCache *pixmap_cache;
    uint32_t pixmap_cache_generation;
    int pending_pixmaps_sync;

    RedCacheItem *palette_cache[PALETTE_CACHE_HASH_SIZE];
    Ring palette_cache_lru;
    long palette_cache_available;
    uint32_t palette_cache_items;

    struct {
        uint32_t stream_outbuf_size;
        uint8_t *stream_outbuf; // caution stream buffer is also used as compress bufs!!!

        FreeList free_list;
        uint64_t pixmap_cache_items[MAX_DRAWABLE_PIXMAP_CACHE_ITEMS];
        int num_pixmap_cache_items;
    } send_data;

    /* global lz encoding entities */
    GlzSharedDictionary *glz_dict;
    GlzEncoderContext   *glz;
    GlzData glz_data;

    Ring glz_drawables;               // all the living lz drawable, ordered by encoding time
    Ring glz_drawables_inst_to_free;               // list of instances to be freed
    pthread_mutex_t glz_drawables_inst_to_free_lock;

    uint8_t surface_client_created[NUM_SURFACES];
    QRegion surface_client_lossy_region[NUM_SURFACES];

    StreamAgent stream_agents[NUM_STREAMS];
    int use_video_encoder_rate_control;
    uint32_t streams_max_latency;
    uint64_t streams_max_bit_rate;
    bool gl_draw_ongoing;
};

#define DCC_TO_WORKER(dcc)                                              \
    (SPICE_CONTAINEROF((dcc)->common.base.channel, CommonGraphicsChannel, base)->worker)
#define DCC_TO_DC(dcc)                                                  \
     SPICE_CONTAINEROF((dcc)->common.base.channel, DisplayChannel, common.base)
#define RCC_TO_DCC(rcc) SPICE_CONTAINEROF((rcc), DisplayChannelClient, common.base)

typedef struct RedSurfaceCreateItem {
    SpiceMsgSurfaceCreate surface_create;
    RedPipeItem pipe_item;
} RedSurfaceCreateItem;

typedef struct RedGlScanoutUnixItem {
    RedPipeItem base;
} RedGlScanoutUnixItem;

typedef struct RedGlDrawItem {
    RedPipeItem base;
    SpiceMsgDisplayGlDraw draw;
} RedGlDrawItem;

typedef struct RedImageItem {
    RedPipeItem base;
    SpicePoint pos;
    int width;
    int height;
    int stride;
    int top_down;
    int surface_id;
    int image_format;
    uint32_t image_flags;
    int can_lossy;
    uint8_t data[0];
} RedImageItem;

typedef struct RedDrawablePipeItem {
    RingItem base;  /* link for a list of pipe items held by Drawable */
    RedPipeItem dpi_pipe_item; /* link for the client's pipe itself */
    Drawable *drawable;
    DisplayChannelClient *dcc;
    uint8_t refs;
} RedDrawablePipeItem;

DisplayChannelClient*      dcc_new                                   (DisplayChannel *display,
                                                                      RedClient *client,
                                                                      RedsStream *stream,
                                                                      int mig_target,
                                                                      uint32_t *common_caps,
                                                                      int num_common_caps,
                                                                      uint32_t *caps,
                                                                      int num_caps,
                                                                      SpiceImageCompression image_compression,
                                                                      spice_wan_compression_t jpeg_state,
                                                                      spice_wan_compression_t zlib_glz_state);
void                       dcc_start                                 (DisplayChannelClient *dcc);
void                       dcc_stop                                  (DisplayChannelClient *dcc);
int                        dcc_handle_message                        (RedChannelClient *rcc,
                                                                      uint32_t size,
                                                                      uint16_t type, void *msg);
int                        dcc_handle_migrate_data                   (DisplayChannelClient *dcc,
                                                                      uint32_t size, void *message);
void                       dcc_push_monitors_config                  (DisplayChannelClient *dcc);
void                       dcc_destroy_surface                       (DisplayChannelClient *dcc,
                                                                      uint32_t surface_id);
void                       dcc_stream_agent_clip                     (DisplayChannelClient* dcc,
                                                                      StreamAgent *agent);
void                       dcc_create_stream                         (DisplayChannelClient *dcc,
                                                                      Stream *stream);
void                       dcc_create_surface                        (DisplayChannelClient *dcc,
                                                                      int surface_id);
void                       dcc_push_surface_image                    (DisplayChannelClient *dcc,
                                                                      int surface_id);
RedImageItem *             dcc_add_surface_area_image                (DisplayChannelClient *dcc,
                                                                      int surface_id,
                                                                      SpiceRect *area,
                                                                      RedPipeItem *pos,
                                                                      int can_lossy);
void                       dcc_palette_cache_reset                   (DisplayChannelClient *dcc);
void                       dcc_palette_cache_palette                 (DisplayChannelClient *dcc,
                                                                      SpicePalette *palette,
                                                                      uint8_t *flags);
int                        dcc_pixmap_cache_unlocked_add             (DisplayChannelClient *dcc,
                                                                      uint64_t id, uint32_t size, int lossy);
void                       dcc_prepend_drawable                      (DisplayChannelClient *dcc,
                                                                      Drawable *drawable);
void                       dcc_append_drawable                       (DisplayChannelClient *dcc,
                                                                      Drawable *drawable);
void                       dcc_add_drawable_after                    (DisplayChannelClient *dcc,
                                                                      Drawable *drawable,
                                                                      RedPipeItem *pos);
void                       dcc_release_item                          (DisplayChannelClient *dcc,
                                                                      RedPipeItem *item,
                                                                      int item_pushed);
void                       dcc_send_item                             (DisplayChannelClient *dcc,
                                                                      RedPipeItem *item);
int                        dcc_clear_surface_drawables_from_pipe     (DisplayChannelClient *dcc,
                                                                      int surface_id,
                                                                      int wait_if_used);
int                        dcc_drawable_is_in_pipe                   (DisplayChannelClient *dcc,
                                                                      Drawable *drawable);
RedPipeItem *              dcc_gl_scanout_item_new                   (RedChannelClient *rcc,
                                                                      void *data, int num);
RedPipeItem *              dcc_gl_draw_item_new                      (RedChannelClient *rcc,
                                                                      void *data, int num);

typedef struct compress_send_data_t {
    void*    comp_buf;
    uint32_t comp_buf_size;
    SpicePalette *lzplt_palette;
    int is_lossy;
} compress_send_data_t;

int                        dcc_compress_image                        (DisplayChannelClient *dcc,
                                                                      SpiceImage *dest, SpiceBitmap *src, Drawable *drawable,
                                                                      int can_lossy,
                                                                      compress_send_data_t* o_comp_data);

#endif /* DCC_H_ */
