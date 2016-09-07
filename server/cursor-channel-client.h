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
#ifndef CURSOR_CHANNEL_CLIENT_H_
# define CURSOR_CHANNEL_CLIENT_H_

#include "cache-item.h"
#include "red-common.h"
#include "red-channel.h"
#include "reds-stream.h"

typedef struct CursorChannel CursorChannel;
typedef struct CursorChannelClient CursorChannelClient;

#define CURSOR_CHANNEL_CLIENT(Client) ((CursorChannelClient*)(Client))

CursorChannelClient* cursor_channel_client_new(CursorChannel *cursor,
                                               RedClient *client,
                                               RedsStream *stream,
                                               int mig_target,
                                               uint32_t *common_caps,
                                               int num_common_caps,
                                               uint32_t *caps, int num_caps);

void cursor_channel_client_reset_cursor_cache(RedChannelClient *rcc);
void cursor_channel_client_on_disconnect(RedChannelClient *rcc);
RedCacheItem* cursor_channel_client_cache_find(CursorChannelClient *ccc, uint64_t id);
int cursor_channel_client_cache_add(CursorChannelClient *ccc, uint64_t id, size_t size);

#endif /* CURSOR_CHANNEL_CLIENT_H_ */
