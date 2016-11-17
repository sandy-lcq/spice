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

#ifndef DISPLAY_CHANNEL_PRIVATE_H_
#define DISPLAY_CHANNEL_PRIVATE_H_

#include "display-channel.h"

#define NUM_DRAWABLES 1000
typedef struct _Drawable _Drawable;
struct _Drawable {
    union {
        Drawable drawable;
        _Drawable *next;
    } u;
};

struct DisplayChannelPrivate
{
    DisplayChannel *pub;

    uint32_t bits_unique;

    MonitorsConfig *monitors_config;

    uint32_t renderer;
    int enable_jpeg;
    int enable_zlib_glz_wrap;

    /* A ring of pending drawables for this DisplayChannel, regardless of which
     * surface they're associated with. This list is mainly used to flush older
     * drawables when we need to make room for new drawables.  The ring is
     * maintained in order of age, the tail being the oldest drawable */
    Ring current_list;

    uint32_t drawable_count;
    _Drawable drawables[NUM_DRAWABLES];
    _Drawable *free_drawables;

    int stream_video;
    GArray *video_codecs;
    uint32_t stream_count;
    Stream streams_buf[NUM_STREAMS];
    Stream *free_streams;
    Ring streams;
    ItemTrace items_trace[NUM_TRACE_ITEMS];
    uint32_t next_item_trace;
    uint64_t streams_size_total;

    RedSurface surfaces[NUM_SURFACES];
    uint32_t n_surfaces;
    SpiceImageSurfaces image_surfaces;

    ImageCache image_cache;

    int gl_draw_async_count;

/* TODO: some day unify this, make it more runtime.. */
    stat_info_t add_stat;
    stat_info_t exclude_stat;
    stat_info_t __exclude_stat;
#ifdef RED_WORKER_STAT
    uint32_t add_count;
    uint32_t add_with_shadow_count;
#endif
    RedStatCounter cache_hits_counter;
    RedStatCounter add_to_cache_counter;
    RedStatCounter non_cache_counter;
    ImageEncoderSharedData encoder_shared_data;
};

#endif /* DISPLAY_CHANNEL_PRIVATE_H_ */
