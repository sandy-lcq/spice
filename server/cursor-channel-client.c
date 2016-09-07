/* -*- Mode: C; c-basic-offset: 4; indent-tabs-mode: nil -*- */
/*
   Copyright (C) 2009-2016 Red Hat, Inc.

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

#include <common/generated_server_marshallers.h>

#include "red-channel-client.h"
#include "cache-item.h"
#include "cursor-channel.h"
#include "cursor-channel-client.h"

#define CLIENT_CURSOR_CACHE_SIZE 256

#define CURSOR_CACHE_HASH_SHIFT 8
#define CURSOR_CACHE_HASH_SIZE (1 << CURSOR_CACHE_HASH_SHIFT)
#define CURSOR_CACHE_HASH_MASK (CURSOR_CACHE_HASH_SIZE - 1)
#define CURSOR_CACHE_HASH_KEY(id) ((id) & CURSOR_CACHE_HASH_MASK)
#define CURSOR_CLIENT_TIMEOUT 30000000000ULL //nano

enum {
    RED_PIPE_ITEM_TYPE_CURSOR = RED_PIPE_ITEM_TYPE_COMMON_LAST,
    RED_PIPE_ITEM_TYPE_CURSOR_INIT,
    RED_PIPE_ITEM_TYPE_INVAL_CURSOR_CACHE,
};

struct CursorChannelClient {
    RedChannelClient base;

    RedCacheItem *cursor_cache[CURSOR_CACHE_HASH_SIZE];
    Ring cursor_cache_lru;
    long cursor_cache_available;
    uint32_t cursor_cache_items;
};


#define CLIENT_CURSOR_CACHE
#include "cache-item.tmpl.c"
#undef CLIENT_CURSOR_CACHE

#ifdef DEBUG_CURSORS
static int _cursor_count = 0;
#endif

void cursor_channel_client_reset_cursor_cache(RedChannelClient *rcc)
{
    red_cursor_cache_reset(CURSOR_CHANNEL_CLIENT(rcc), CLIENT_CURSOR_CACHE_SIZE);
}

void cursor_channel_client_on_disconnect(RedChannelClient *rcc)
{
    if (!rcc) {
        return;
    }
    cursor_channel_client_reset_cursor_cache(rcc);
}

void cursor_channel_client_migrate(RedChannelClient *rcc)
{
    spice_return_if_fail(rcc);

    red_channel_client_pipe_add_type(rcc, RED_PIPE_ITEM_TYPE_INVAL_CURSOR_CACHE);
    red_channel_client_default_migrate(rcc);
}

CursorChannelClient* cursor_channel_client_new(CursorChannel *cursor, RedClient *client, RedsStream *stream,
                                               int mig_target,
                                               uint32_t *common_caps, int num_common_caps,
                                               uint32_t *caps, int num_caps)
{
    spice_return_val_if_fail(cursor, NULL);
    spice_return_val_if_fail(client, NULL);
    spice_return_val_if_fail(stream, NULL);
    spice_return_val_if_fail(!num_common_caps || common_caps, NULL);
    spice_return_val_if_fail(!num_caps || caps, NULL);

    CursorChannelClient *ccc =
        (CursorChannelClient*)red_channel_client_create(sizeof(CursorChannelClient),
                                                        RED_CHANNEL(cursor),
                                                        client, stream,
                                                        FALSE,
                                                        num_common_caps,
                                                        common_caps,
                                                        num_caps,
                                                        caps);
    spice_return_val_if_fail(ccc != NULL, NULL);
    COMMON_GRAPHICS_CHANNEL(cursor)->during_target_migrate = mig_target;

    ring_init(&ccc->cursor_cache_lru);
    ccc->cursor_cache_available = CLIENT_CURSOR_CACHE_SIZE;

    return ccc;
}

RedCacheItem* cursor_channel_client_cache_find(CursorChannelClient *ccc, uint64_t id)
{
    return red_cursor_cache_find(ccc, id);
}

int cursor_channel_client_cache_add(CursorChannelClient *ccc, uint64_t id, size_t size)
{
    return red_cursor_cache_add(ccc, id, size);
}
