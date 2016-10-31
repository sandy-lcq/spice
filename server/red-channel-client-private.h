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

#ifndef _H_RED_CHANNEL_CLIENT_PRIVATE
#define _H_RED_CHANNEL_CLIENT_PRIVATE

#include "red-channel.h"
#include "red-channel-client.h"

typedef struct RedChannelClientLatencyMonitor {
    int state;
    uint64_t last_pong_time;
    SpiceTimer *timer;
    uint32_t id;
    int tcp_nodelay;
    int warmup_was_sent;

    int64_t roundtrip;
} RedChannelClientLatencyMonitor;

typedef struct RedChannelClientConnectivityMonitor {
    int state;
    uint32_t out_bytes;
    uint32_t in_bytes;
    uint32_t timeout;
    SpiceTimer *timer;
} RedChannelClientConnectivityMonitor;

typedef struct OutgoingHandler {
    OutgoingHandlerInterface *cb;
    void *opaque;
    struct iovec vec_buf[IOV_MAX];
    int vec_size;
    struct iovec *vec;
    int pos;
    int size;
} OutgoingHandler;

struct RedChannelClientPrivate
{
    RedChannel *channel;
    RedClient  *client;
    RedsStream *stream;
    gboolean monitor_latency;

    uint32_t refs;

    struct {
        uint32_t generation;
        uint32_t client_generation;
        uint32_t messages_window;
        uint32_t client_window;
    } ack_data;

    struct {
        /* this can be either main.marshaller or urgent.marshaller */
        SpiceMarshaller *marshaller;
        SpiceDataHeaderOpaque header;
        uint32_t size;
        RedPipeItem *item;
        int blocked;
        uint64_t last_sent_serial;

        struct {
            SpiceMarshaller *marshaller;
            uint8_t *header_data;
            RedPipeItem *item;
        } main;

        struct {
            SpiceMarshaller *marshaller;
        } urgent;
    } send_data;

    int during_send;
    GQueue pipe;

    RedChannelCapabilities remote_caps;
    int is_mini_header;
    gboolean destroying;

    int wait_migrate_data;
    int wait_migrate_flush_mark;

    RedChannelClientLatencyMonitor latency_monitor;
    RedChannelClientConnectivityMonitor connectivity_monitor;

    OutgoingHandler outgoing;
};

#endif /* _H_RED_CHANNEL_CLIENT_PRIVATE */
