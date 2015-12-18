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

#ifndef _H_REDWORKER
#define _H_REDWORKER

#include "red-common.h"
#include "red-qxl.h"
#include "red-parse-qxl.h"

typedef struct RedWorker RedWorker;

typedef struct CommonGraphicsChannelClient {
    RedChannelClient base;

    int is_low_bandwidth;
} CommonGraphicsChannelClient;

#define COMMON_GRAPHICS_CHANNEL_CLIENT(Client) ((CommonGraphicsChannelClient*)(Client))
#define COMMON_CLIENT_TIMEOUT (NSEC_PER_SEC * 30)

#define CHANNEL_RECEIVE_BUF_SIZE 1024
typedef struct CommonGraphicsChannel {
    RedChannel base; // Must be the first thing

    QXLInstance *qxl;
    uint8_t recv_buf[CHANNEL_RECEIVE_BUF_SIZE];
    uint32_t id_alloc; // bitfield. TODO - use this instead of shift scheme.
    int during_target_migrate; /* TRUE when the client that is associated with the channel
                                  is during migration. Turned off when the vm is started.
                                  The flag is used to avoid sending messages that are artifacts
                                  of the transition from stopped vm to loaded vm (e.g., recreation
                                  of the primary surface) */
} CommonGraphicsChannel;

#define COMMON_GRAPHICS_CHANNEL(Channel) ((CommonGraphicsChannel*)(Channel))

enum {
    PIPE_ITEM_TYPE_VERB = PIPE_ITEM_TYPE_CHANNEL_BASE,
    PIPE_ITEM_TYPE_INVAL_ONE,

    PIPE_ITEM_TYPE_COMMON_LAST
};

typedef struct VerbItem {
    PipeItem base;
    uint16_t verb;
} VerbItem;

static inline void red_marshall_verb(RedChannelClient *rcc, VerbItem *item)
{
    red_channel_client_init_send_data(rcc, item->verb, NULL);
}

static inline void red_pipe_add_verb(RedChannelClient* rcc, uint16_t verb)
{
    VerbItem *item = spice_new(VerbItem, 1);

    pipe_item_init(&item->base, PIPE_ITEM_TYPE_VERB);
    item->verb = verb;
    red_channel_client_pipe_add(rcc, &item->base);
}

#define LINK_TO_RCC(ptr) SPICE_CONTAINEROF(ptr, RedChannelClient, channel_link)
#define RCC_FOREACH_SAFE(link, next, rcc, channel) \
    SAFE_FOREACH(link, next, channel,  &(channel)->clients, rcc, LINK_TO_RCC(link))


static inline void red_pipes_add_verb(RedChannel *channel, uint16_t verb)
{
    RedChannelClient *rcc;
    RingItem *link, *next;

    RCC_FOREACH_SAFE(link, next, rcc, channel) {
        red_pipe_add_verb(rcc, verb);
    }
}

RedWorker* red_worker_new(QXLInstance *qxl,
                          const ClientCbs *client_cursor_cbs,
                          const ClientCbs *client_display_cbs);
bool       red_worker_run(RedWorker *worker);

void red_drawable_unref(RedDrawable *red_drawable);

CommonGraphicsChannel *red_worker_new_channel(RedWorker *worker, int size,
                                              const char *name,
                                              uint32_t channel_type, int migration_flags,
                                              ChannelCbs *channel_cbs,
                                              channel_handle_parsed_proc handle_parsed);

CommonGraphicsChannelClient *common_graphics_channel_new_client(CommonGraphicsChannel *common,
                                                                int size,
                                                                RedClient *client,
                                                                RedsStream *stream,
                                                                int mig_target,
                                                                int monitor_latency,
                                                                uint32_t *common_caps,
                                                                int num_common_caps,
                                                                uint32_t *caps,
                                                                int num_caps);

#endif
