/* -*- Mode: C; c-basic-offset: 4; indent-tabs-mode: nil -*- */
/*
   Copyright (C) 2009 Red Hat, Inc.

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
#ifndef CURSOR_CHANNEL_H_
# define CURSOR_CHANNEL_H_

#include "spice.h"
#include "reds.h"
#include "red_worker.h"
#include "red_parse_qxl.h"
#include "cache-item.h"
#include "stat.h"

#define CLIENT_CURSOR_CACHE_SIZE 256

#define CURSOR_CACHE_HASH_SHIFT 8
#define CURSOR_CACHE_HASH_SIZE (1 << CURSOR_CACHE_HASH_SHIFT)
#define CURSOR_CACHE_HASH_MASK (CURSOR_CACHE_HASH_SIZE - 1)
#define CURSOR_CACHE_HASH_KEY(id) ((id) & CURSOR_CACHE_HASH_MASK)

typedef struct CursorItem {
    uint32_t group_id;
    int refs;
    RedCursorCmd *red_cursor;
} CursorItem;

typedef struct CursorPipeItem {
    PipeItem base;
    CursorItem *cursor_item;
    int refs;
} CursorPipeItem;

typedef struct LocalCursor {
    CursorItem base;
    SpicePoint16 position;
    uint32_t data_size;
    SpiceCursor red_cursor;
} LocalCursor;

typedef struct CursorChannelClient {
    CommonChannelClient common;

    CacheItem *cursor_cache[CURSOR_CACHE_HASH_SIZE];
    Ring cursor_cache_lru;
    long cursor_cache_available;
    uint32_t cursor_cache_items;
} CursorChannelClient;

typedef struct CursorChannel {
    CommonChannel common; // Must be the first thing

    CursorItem *item;
    int cursor_visible;
    SpicePoint16 cursor_position;
    uint16_t cursor_trail_length;
    uint16_t cursor_trail_frequency;
    uint32_t mouse_mode;

#ifdef RED_STATISTICS
    StatNodeRef stat;
#endif
} CursorChannel;

G_STATIC_ASSERT(sizeof(CursorItem) <= QXL_CURSUR_DEVICE_DATA_SIZE);

CursorChannel*       cursor_channel_new         (RedWorker *worker, int migrate);
void                 cursor_channel_disconnect  (RedChannel *channel);
void                 cursor_channel_reset       (CursorChannel *cursor);
void                 cursor_channel_process_cmd (CursorChannel *cursor, RedCursorCmd *cursor_cmd,
                                                 uint32_t group_id);

CursorItem*          cursor_item_new            (RedCursorCmd *cmd, uint32_t group_id);
void                 cursor_item_unref          (QXLInstance *qxl, CursorItem *cursor);


CursorChannelClient *cursor_channel_client_new(CommonChannel *common,
                                               RedClient *client, RedsStream *stream,
                                               int mig_target,
                                               uint32_t *common_caps, int num_common_caps,
                                               uint32_t *caps, int num_caps);

#endif /* CURSOR_CHANNEL_H_ */
