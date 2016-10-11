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

#include <fcntl.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>

#include "common-graphics-channel.h"
#include "dcc.h"
#include "main-channel-client.h"

static uint8_t *common_alloc_recv_buf(RedChannelClient *rcc, uint16_t type, uint32_t size)
{
    RedChannel *channel = red_channel_client_get_channel(rcc);
    CommonGraphicsChannel *common = SPICE_CONTAINEROF(channel, CommonGraphicsChannel, base);

    /* SPICE_MSGC_MIGRATE_DATA is the only client message whose size is dynamic */
    if (type == SPICE_MSGC_MIGRATE_DATA) {
        return spice_malloc(size);
    }

    if (size > CHANNEL_RECEIVE_BUF_SIZE) {
        spice_critical("unexpected message size %u (max is %d)", size, CHANNEL_RECEIVE_BUF_SIZE);
        return NULL;
    }
    return common->recv_buf;
}

static void common_release_recv_buf(RedChannelClient *rcc, uint16_t type, uint32_t size,
                                    uint8_t* msg)
{
    if (type == SPICE_MSGC_MIGRATE_DATA) {
        free(msg);
    }
}

int common_channel_config_socket(RedChannelClient *rcc)
{
    RedClient *client = red_channel_client_get_client(rcc);
    MainChannelClient *mcc = red_client_get_main(client);
    RedsStream *stream = red_channel_client_get_stream(rcc);
    int flags;
    int delay_val;
    gboolean is_low_bandwidth;

    if ((flags = fcntl(stream->socket, F_GETFL)) == -1) {
        spice_warning("accept failed, %s", strerror(errno));
        return FALSE;
    }

    if (fcntl(stream->socket, F_SETFL, flags | O_NONBLOCK) == -1) {
        spice_warning("accept failed, %s", strerror(errno));
        return FALSE;
    }

    // TODO - this should be dynamic, not one time at channel creation
    is_low_bandwidth = main_channel_client_is_low_bandwidth(mcc);
    delay_val = is_low_bandwidth ? 0 : 1;
    /* FIXME: Using Nagle's Algorithm can lead to apparent delays, depending
     * on the delayed ack timeout on the other side.
     * Instead of using Nagle's, we need to implement message buffering on
     * the application level.
     * see: http://www.stuartcheshire.org/papers/NagleDelayedAck/
     */
    if (setsockopt(stream->socket, IPPROTO_TCP, TCP_NODELAY, &delay_val,
                   sizeof(delay_val)) == -1) {
        if (errno != ENOTSUP) {
            spice_warning("setsockopt failed, %s", strerror(errno));
        }
    }
    // TODO: move wide/narrow ack setting to red_channel.
    red_channel_client_ack_set_client_window(rcc,
        is_low_bandwidth ?
        WIDE_CLIENT_ACK_WINDOW : NARROW_CLIENT_ACK_WINDOW);
    return TRUE;
}

CommonGraphicsChannel* common_graphics_channel_new(RedsState *server,
                                                   QXLInstance *qxl,
                                                   const SpiceCoreInterfaceInternal *core,
                                                   int size, uint32_t channel_type,
                                                   int migration_flags,
                                                   ChannelCbs *channel_cbs,
                                                   channel_handle_parsed_proc handle_parsed)
{
    RedChannel *channel = NULL;
    CommonGraphicsChannel *common;

    spice_return_val_if_fail(channel_cbs, NULL);
    spice_return_val_if_fail(!channel_cbs->alloc_recv_buf, NULL);
    spice_return_val_if_fail(!channel_cbs->release_recv_buf, NULL);

    if (!channel_cbs->config_socket)
        channel_cbs->config_socket = common_channel_config_socket;
    channel_cbs->alloc_recv_buf = common_alloc_recv_buf;
    channel_cbs->release_recv_buf = common_release_recv_buf;

    channel = red_channel_create_parser(size, server,
                                        core, channel_type,
                                        qxl->id, TRUE /* handle_acks */,
                                        spice_get_client_channel_parser(channel_type, NULL),
                                        handle_parsed,
                                        channel_cbs,
                                        migration_flags);
    spice_return_val_if_fail(channel, NULL);

    common = COMMON_GRAPHICS_CHANNEL(channel);
    common->qxl = qxl;
    return common;
}

