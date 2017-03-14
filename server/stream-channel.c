/* -*- Mode: C; c-basic-offset: 4; indent-tabs-mode: nil -*- */
/*
   Copyright (C) 2017 Red Hat, Inc.

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

#include "red-channel-client.h"
#include "stream-channel.h"
#include "reds.h"
#include "common-graphics-channel.h"

#define TYPE_STREAM_CHANNEL_CLIENT stream_channel_client_get_type()

#define STREAM_CHANNEL_CLIENT(obj) \
    (G_TYPE_CHECK_INSTANCE_CAST((obj), TYPE_STREAM_CHANNEL_CLIENT, StreamChannelClient))
#define STREAM_CHANNEL_CLIENT_CLASS(klass) \
    (G_TYPE_CHECK_CLASS_CAST((klass), TYPE_STREAM_CHANNEL_CLIENT, StreamChannelClientClass))
#define IS_STREAM_CHANNEL_CLIENT(obj) \
    (G_TYPE_CHECK_INSTANCE_TYPE((obj), TYPE_STREAM_CHANNEL_CLIENT))
#define IS_STREAM_CHANNEL_CLIENT_CLASS(klass) \
    (G_TYPE_CHECK_CLASS_TYPE((klass), TYPE_STREAM_CHANNEL_CLIENT))
#define STREAM_CHANNEL_CLIENT_GET_CLASS(obj) \
    (G_TYPE_INSTANCE_GET_CLASS((obj), TYPE_STREAM_CHANNEL_CLIENT, StreamChannelClientClass))

typedef struct StreamChannelClient StreamChannelClient;
typedef struct StreamChannelClientClass StreamChannelClientClass;

/* we need to inherit from CommonGraphicsChannelClient
 * to get buffer handling */
struct StreamChannelClient {
    CommonGraphicsChannelClient parent;
};

struct StreamChannelClientClass {
    CommonGraphicsChannelClientClass parent_class;
};

GType stream_channel_client_get_type(void) G_GNUC_CONST;

G_DEFINE_TYPE(StreamChannelClient, stream_channel_client, TYPE_COMMON_GRAPHICS_CHANNEL_CLIENT)

struct StreamChannel {
    RedChannel parent;
};

struct StreamChannelClass {
    RedChannelClass parent_class;
};

G_DEFINE_TYPE(StreamChannel, stream_channel, RED_TYPE_CHANNEL)

static void stream_channel_client_on_disconnect(RedChannelClient *rcc);

static void
stream_channel_client_class_init(StreamChannelClientClass *klass)
{
    RedChannelClientClass *channel_class = RED_CHANNEL_CLIENT_CLASS(klass);

    channel_class->on_disconnect = stream_channel_client_on_disconnect;
}

static void
stream_channel_client_init(StreamChannelClient *client)
{
}

static void
stream_channel_client_on_disconnect(RedChannelClient *rcc)
{
}

static StreamChannelClient*
stream_channel_client_new(StreamChannel *channel, RedClient *client, RedsStream *stream,
                          int mig_target, RedChannelCapabilities *caps)
{
    StreamChannelClient *rcc;

    rcc = g_initable_new(TYPE_STREAM_CHANNEL_CLIENT,
                         NULL, NULL,
                         "channel", channel,
                         "client", client,
                         "stream", stream,
                         "monitor-latency", FALSE,
                         "caps", caps,
                         NULL);

    return rcc;
}

static void
stream_channel_send_item(RedChannelClient *rcc, RedPipeItem *pipe_item)
{
    switch (pipe_item->type) {
    default:
        spice_error("invalid pipe item type");
    }

    red_channel_client_begin_send_message(rcc);
}

static bool
handle_message(RedChannelClient *rcc, uint16_t type, uint32_t size, void *msg)
{
    switch (type) {
    case SPICE_MSGC_DISPLAY_INIT:
    case SPICE_MSGC_DISPLAY_PREFERRED_COMPRESSION:
        return true;
    case SPICE_MSGC_DISPLAY_STREAM_REPORT:
        /* TODO these will help tune the streaming reducing/increasing quality */
        return true;
    case SPICE_MSGC_DISPLAY_GL_DRAW_DONE:
        /* client should not send this message */
        return false;
    default:
        return red_channel_client_handle_message(rcc, type, size, msg);
    }
}


StreamChannel*
stream_channel_new(RedsState *server, uint32_t id)
{
    return g_object_new(TYPE_STREAM_CHANNEL,
                        "spice-server", server,
                        "core-interface", reds_get_core_interface(server),
                        "channel-type", SPICE_CHANNEL_DISPLAY,
                        // TODO this id should be after all qxl devices
                        "id", id,
                        "migration-flags", 0,
                        "handle-acks", TRUE, // TODO sure ??
                        NULL);
}

static void
stream_channel_connect(RedChannel *red_channel, RedClient *red_client, RedsStream *stream,
                       int migration, RedChannelCapabilities *caps)
{
    StreamChannel *channel = STREAM_CHANNEL(red_channel);
    StreamChannelClient *client;

    spice_return_if_fail(stream != NULL);

    client = stream_channel_client_new(channel, red_client, stream, migration, caps);
    spice_return_if_fail(client != NULL);

    // TODO set capabilities like  SPICE_DISPLAY_CAP_MONITORS_CONFIG
    // see guest_set_client_capabilities
    RedChannelClient *rcc = RED_CHANNEL_CLIENT(client);
    red_channel_client_push_set_ack(rcc);

    // TODO what should happen on migration, dcc return if on migration wait ??
    red_channel_client_ack_zero_messages_window(rcc);

    // "emulate" dcc_start
    // TODO only if "surface"
    red_channel_client_pipe_add_empty_msg(rcc, SPICE_MSG_DISPLAY_INVAL_ALL_PALETTES);
    // TODO red_surface_create_item_new
    // TODO surface data ??
    // TODO monitor configs ??
    red_channel_client_pipe_add_empty_msg(rcc, SPICE_MSG_DISPLAY_MARK);
}

static void
stream_channel_constructed(GObject *object)
{
    ClientCbs client_cbs = { NULL, };
    RedChannel *red_channel = RED_CHANNEL(object);
    RedsState *reds = red_channel_get_server(red_channel);

    G_OBJECT_CLASS(stream_channel_parent_class)->constructed(object);

    client_cbs.connect = stream_channel_connect;
    red_channel_register_client_cbs(red_channel, &client_cbs, NULL);

    // TODO, send monitor to support multiple monitors in the future
//    red_channel_set_cap(red_channel, SPICE_DISPLAY_CAP_MONITORS_CONFIG);
    red_channel_set_cap(red_channel, SPICE_DISPLAY_CAP_STREAM_REPORT);

    reds_register_channel(reds, red_channel);
}

static void
stream_channel_class_init(StreamChannelClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS(klass);
    RedChannelClass *channel_class = RED_CHANNEL_CLASS(klass);

    object_class->constructed = stream_channel_constructed;

    channel_class->parser = spice_get_client_channel_parser(SPICE_CHANNEL_DISPLAY, NULL);
    channel_class->handle_message = handle_message;

    channel_class->send_item = stream_channel_send_item;
}

static void
stream_channel_init(StreamChannel *channel)
{
}
