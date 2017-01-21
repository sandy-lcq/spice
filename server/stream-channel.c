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

#include <common/generated_server_marshallers.h>
#include <spice/stream-device.h>

#include "red-channel-client.h"
#include "stream-channel.h"
#include "reds.h"
#include "common-graphics-channel.h"
#include "display-limits.h"

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

    /* current video stream id, <0 if not initialized or
     * we are not sending a stream */
    int stream_id;
};

struct StreamChannelClientClass {
    CommonGraphicsChannelClientClass parent_class;
};

GType stream_channel_client_get_type(void) G_GNUC_CONST;

G_DEFINE_TYPE(StreamChannelClient, stream_channel_client, TYPE_COMMON_GRAPHICS_CHANNEL_CLIENT)

struct StreamChannel {
    RedChannel parent;

    /* current video stream id, <0 if not initialized or
     * we are not sending a stream */
    int stream_id;
    /* size of the current video stream */
    unsigned width, height;
};

struct StreamChannelClass {
    RedChannelClass parent_class;
};

G_DEFINE_TYPE(StreamChannel, stream_channel, RED_TYPE_CHANNEL)

enum {
    RED_PIPE_ITEM_TYPE_SURFACE_CREATE = RED_PIPE_ITEM_TYPE_COMMON_LAST,
    RED_PIPE_ITEM_TYPE_SURFACE_DESTROY,
    RED_PIPE_ITEM_TYPE_FILL_SURFACE,
    RED_PIPE_ITEM_TYPE_STREAM_CREATE,
    RED_PIPE_ITEM_TYPE_STREAM_DATA,
    RED_PIPE_ITEM_TYPE_STREAM_DESTROY,
};

typedef struct StreamCreateItem {
    RedPipeItem base;
    SpiceMsgDisplayStreamCreate stream_create;
} StreamCreateItem;

typedef struct StreamDataItem {
    RedPipeItem base;
    // NOTE: this must be the last field in the structure
    SpiceMsgDisplayStreamData data;
} StreamDataItem;

#define PRIMARY_SURFACE_ID 0

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
    client->stream_id = -1;
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
fill_base(SpiceMarshaller *m, const StreamChannel *channel)
{
    SpiceMsgDisplayBase base;

    base.surface_id = PRIMARY_SURFACE_ID;
    base.box = (SpiceRect) { 0, 0, channel->width, channel->height };
    base.clip = (SpiceClip) { SPICE_CLIP_TYPE_NONE, NULL };

    spice_marshall_DisplayBase(m, &base);
}

static void
stream_channel_send_item(RedChannelClient *rcc, RedPipeItem *pipe_item)
{
    SpiceMarshaller *m = red_channel_client_get_marshaller(rcc);
    StreamChannelClient *client = STREAM_CHANNEL_CLIENT(rcc);
    StreamChannel *channel = STREAM_CHANNEL(red_channel_client_get_channel(rcc));

    switch (pipe_item->type) {
    case RED_PIPE_ITEM_TYPE_SURFACE_CREATE: {
        red_channel_client_init_send_data(rcc, SPICE_MSG_DISPLAY_SURFACE_CREATE);
        SpiceMsgSurfaceCreate surface_create = {
            PRIMARY_SURFACE_ID,
            channel->width, channel->height,
            SPICE_SURFACE_FMT_32_xRGB, SPICE_SURFACE_FLAGS_PRIMARY
        };
        spice_marshall_msg_display_surface_create(m, &surface_create);
        break;
    }
    case RED_PIPE_ITEM_TYPE_SURFACE_DESTROY: {
        red_channel_client_init_send_data(rcc, SPICE_MSG_DISPLAY_SURFACE_DESTROY);
        SpiceMsgSurfaceDestroy surface_destroy = { PRIMARY_SURFACE_ID };
        spice_marshall_msg_display_surface_destroy(m, &surface_destroy);
        break;
    }
    case RED_PIPE_ITEM_TYPE_FILL_SURFACE: {
        red_channel_client_init_send_data(rcc, SPICE_MSG_DISPLAY_DRAW_FILL);

        fill_base(m, channel);

        SpiceFill fill;
        fill.brush = (SpiceBrush) { SPICE_BRUSH_TYPE_SOLID, { .color = 0 } };
        fill.rop_descriptor = SPICE_ROPD_OP_PUT;
        fill.mask = (SpiceQMask) { 0, { 0, 0 }, NULL };
        SpiceMarshaller *brush_pat_out, *mask_bitmap_out;
        spice_marshall_Fill(m, &fill, &brush_pat_out, &mask_bitmap_out);
        break;
    }
    case RED_PIPE_ITEM_TYPE_STREAM_CREATE: {
        StreamCreateItem *item = SPICE_UPCAST(StreamCreateItem, pipe_item);
        client->stream_id = item->stream_create.id;
        red_channel_client_init_send_data(rcc, SPICE_MSG_DISPLAY_STREAM_CREATE);
        spice_marshall_msg_display_stream_create(m, &item->stream_create);
        break;
    }
    case RED_PIPE_ITEM_TYPE_STREAM_DATA: {
        StreamDataItem *item = SPICE_UPCAST(StreamDataItem, pipe_item);
        red_channel_client_init_send_data(rcc, SPICE_MSG_DISPLAY_STREAM_DATA);
        spice_marshall_msg_display_stream_data(m, &item->data);
        red_pipe_item_ref(pipe_item);
        spice_marshaller_add_by_ref_full(m, item->data.data, item->data.data_size,
                                         marshaller_unref_pipe_item, pipe_item);
        break;
    }
    case RED_PIPE_ITEM_TYPE_STREAM_DESTROY: {
        if (client->stream_id < 0) {
            return;
        }
        SpiceMsgDisplayStreamDestroy stream_destroy = { client->stream_id };
        red_channel_client_init_send_data(rcc, SPICE_MSG_DISPLAY_STREAM_DESTROY);
        spice_marshall_msg_display_stream_destroy(m, &stream_destroy);
        client->stream_id = -1;
        break;
    }
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
    // pass proper data
    red_channel_client_pipe_add_type(rcc, RED_PIPE_ITEM_TYPE_SURFACE_CREATE);
    // surface data
    red_channel_client_pipe_add_type(rcc, RED_PIPE_ITEM_TYPE_FILL_SURFACE);
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
    channel->stream_id = -1;
    channel->width = 1024;
    channel->height = 768;
}

void
stream_channel_change_format(StreamChannel *channel, const StreamMsgFormat *fmt)
{
    RedChannel *red_channel = RED_CHANNEL(channel);

    // send destroy old stream
    red_channel_pipes_add_type(red_channel, RED_PIPE_ITEM_TYPE_STREAM_DESTROY);

    // send new create surface if required
    if (channel->width != fmt->width || channel->height != fmt->height) {
        channel->width = fmt->width;
        channel->height = fmt->height;
        red_channel_pipes_add_type(red_channel, RED_PIPE_ITEM_TYPE_SURFACE_DESTROY);
        red_channel_pipes_add_type(red_channel, RED_PIPE_ITEM_TYPE_SURFACE_CREATE);
        // TODO monitors config ??
    }

    // allocate a new stream id
    channel->stream_id = (channel->stream_id + 1) % NUM_STREAMS;

    // send create stream
    StreamCreateItem *item = g_new0(StreamCreateItem, 1);
    red_pipe_item_init(&item->base, RED_PIPE_ITEM_TYPE_STREAM_CREATE);
    item->stream_create.id = channel->stream_id;
    item->stream_create.flags = SPICE_STREAM_FLAGS_TOP_DOWN;
    item->stream_create.codec_type = fmt->codec;
    item->stream_create.stream_width = fmt->width;
    item->stream_create.stream_height = fmt->height;
    item->stream_create.src_width = fmt->width;
    item->stream_create.src_height = fmt->height;
    item->stream_create.dest = (SpiceRect) { 0, 0, fmt->width, fmt->height };
    item->stream_create.clip = (SpiceClip) { SPICE_CLIP_TYPE_NONE, NULL };
    red_channel_pipes_add(red_channel, &item->base);
}

void
stream_channel_send_data(StreamChannel *channel, const void *data, size_t size, uint32_t mm_time)
{
    if (channel->stream_id < 0) {
        // this condition can happen if the guest didn't handle
        // the format stop that we send so think the stream is still
        // started
        return;
    }

    RedChannel *red_channel = RED_CHANNEL(channel);

    StreamDataItem *item = g_malloc(sizeof(*item) + size);
    red_pipe_item_init(&item->base, RED_PIPE_ITEM_TYPE_STREAM_DATA);
    item->data.base.id = channel->stream_id;
    item->data.base.multi_media_time = mm_time;
    item->data.data_size = size;
    // TODO try to optimize avoiding the copy
    memcpy(item->data.data, data, size);
    red_channel_pipes_add(red_channel, &item->base);
}
