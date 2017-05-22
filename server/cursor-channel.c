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
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <glib.h>
#include <common/generated_server_marshallers.h>

#include "common-graphics-channel.h"
#include "cursor-channel.h"
#include "cursor-channel-client.h"
#include "reds.h"
#include "red-qxl.h"

typedef struct CursorItem {
    QXLInstance *qxl;
    int refs;
    RedCursorCmd *red_cursor;
} CursorItem;

struct CursorChannel
{
    CommonGraphicsChannel parent;

    CursorItem *item;
    bool cursor_visible;
    SpicePoint16 cursor_position;
    uint16_t cursor_trail_length;
    uint16_t cursor_trail_frequency;
    uint32_t mouse_mode;
};

struct CursorChannelClass
{
    CommonGraphicsChannelClass parent_class;
};

typedef struct RedCursorPipeItem {
    RedPipeItem base;
    CursorItem *cursor_item;
} RedCursorPipeItem;

G_DEFINE_TYPE(CursorChannel, cursor_channel, TYPE_COMMON_GRAPHICS_CHANNEL)

static void cursor_pipe_item_free(RedPipeItem *pipe_item);

static CursorItem *cursor_item_new(QXLInstance *qxl, RedCursorCmd *cmd)
{
    CursorItem *cursor_item;

    spice_return_val_if_fail(cmd != NULL, NULL);

    cursor_item = g_new0(CursorItem, 1);
    cursor_item->qxl = qxl;
    cursor_item->refs = 1;
    cursor_item->red_cursor = cmd;

    return cursor_item;
}

static CursorItem *cursor_item_ref(CursorItem *item)
{
    spice_return_val_if_fail(item != NULL, NULL);
    spice_return_val_if_fail(item->refs > 0, NULL);

    item->refs++;

    return item;
}

static void cursor_item_unref(CursorItem *item)
{
    RedCursorCmd *cursor_cmd;

    spice_return_if_fail(item != NULL);

    if (--item->refs)
        return;

    cursor_cmd = item->red_cursor;
    red_qxl_release_resource(item->qxl, cursor_cmd->release_info_ext);
    red_put_cursor_cmd(cursor_cmd);
    free(cursor_cmd);

    g_free(item);

}

static void cursor_channel_set_item(CursorChannel *cursor, CursorItem *item)
{
    if (cursor->item)
        cursor_item_unref(cursor->item);

    cursor->item = item ? cursor_item_ref(item) : NULL;
}

static RedPipeItem *new_cursor_pipe_item(RedChannelClient *rcc, void *data, int num)
{
    RedCursorPipeItem *item = spice_malloc0(sizeof(RedCursorPipeItem));

    red_pipe_item_init_full(&item->base, RED_PIPE_ITEM_TYPE_CURSOR,
                            cursor_pipe_item_free);
    item->cursor_item = data;
    item->cursor_item->refs++;
    return &item->base;
}

static void marshaller_unref_cursor_item(uint8_t *data, void *opaque)
{
    CursorItem *item = opaque;
    cursor_item_unref(item);
}

static void cursor_fill(CursorChannelClient *ccc, CursorItem *cursor,
                        SpiceCursor *red_cursor, SpiceMarshaller *m)
{
    RedCursorCmd *cursor_cmd;

    if (!cursor) {
        red_cursor->flags = SPICE_CURSOR_FLAGS_NONE;
        return;
    }

    cursor_cmd = cursor->red_cursor;
    *red_cursor = cursor_cmd->u.set.shape;

    if (red_cursor->header.unique) {
        if (cursor_channel_client_cache_find(ccc, red_cursor->header.unique)) {
            red_cursor->flags |= SPICE_CURSOR_FLAGS_FROM_CACHE;
            return;
        }
        if (cursor_channel_client_cache_add(ccc, red_cursor->header.unique, 1)) {
            red_cursor->flags |= SPICE_CURSOR_FLAGS_CACHE_ME;
        }
    }

    if (red_cursor->data_size) {
        SpiceMarshaller *m2 = spice_marshaller_get_submarshaller(m);
        cursor_item_ref(cursor);
        spice_marshaller_add_by_ref_full(m2, red_cursor->data, red_cursor->data_size,
                                         marshaller_unref_cursor_item, cursor);
    }
}

void cursor_channel_disconnect(CursorChannel *cursor_channel)
{
    RedChannel *channel = RED_CHANNEL(cursor_channel);

    if (!channel || !red_channel_is_connected(channel)) {
        return;
    }
    red_channel_apply_clients(channel, cursor_channel_client_reset_cursor_cache);
    red_channel_disconnect(channel);
}

static void cursor_pipe_item_free(RedPipeItem *base)
{
    spice_return_if_fail(base);

    RedCursorPipeItem *pipe_item = SPICE_UPCAST(RedCursorPipeItem, base);

    cursor_item_unref(pipe_item->cursor_item);
    free(pipe_item);
}

static void red_marshall_cursor_init(CursorChannelClient *ccc, SpiceMarshaller *base_marshaller,
                                     RedPipeItem *pipe_item)
{
    CursorChannel *cursor_channel;
    RedChannelClient *rcc = RED_CHANNEL_CLIENT(ccc);
    SpiceMsgCursorInit msg;

    spice_assert(rcc);
    cursor_channel = CURSOR_CHANNEL(red_channel_client_get_channel(rcc));

    red_channel_client_init_send_data(rcc, SPICE_MSG_CURSOR_INIT);
    msg.visible = cursor_channel->cursor_visible;
    msg.position = cursor_channel->cursor_position;
    msg.trail_length = cursor_channel->cursor_trail_length;
    msg.trail_frequency = cursor_channel->cursor_trail_frequency;

    cursor_fill(ccc, cursor_channel->item, &msg.cursor, base_marshaller);
    spice_marshall_msg_cursor_init(base_marshaller, &msg);
}

static void cursor_marshall(CursorChannelClient *ccc,
                            SpiceMarshaller *m,
                            RedCursorPipeItem *cursor_pipe_item)
{
    RedChannelClient *rcc = RED_CHANNEL_CLIENT(ccc);
    CursorChannel *cursor_channel = CURSOR_CHANNEL(red_channel_client_get_channel(rcc));
    CursorItem *item = cursor_pipe_item->cursor_item;
    RedCursorCmd *cmd;

    spice_return_if_fail(cursor_channel);

    cmd = item->red_cursor;
    switch (cmd->type) {
    case QXL_CURSOR_MOVE:
        {
            SpiceMsgCursorMove cursor_move;
            red_channel_client_init_send_data(rcc, SPICE_MSG_CURSOR_MOVE);
            cursor_move.position = cmd->u.position;
            spice_marshall_msg_cursor_move(m, &cursor_move);
            break;
        }
    case QXL_CURSOR_SET:
        {
            SpiceMsgCursorSet cursor_set;

            red_channel_client_init_send_data(rcc, SPICE_MSG_CURSOR_SET);
            cursor_set.position = cmd->u.set.position;
            cursor_set.visible = cursor_channel->cursor_visible;

            cursor_fill(ccc, item, &cursor_set.cursor, m);
            spice_marshall_msg_cursor_set(m, &cursor_set);
            break;
        }
    case QXL_CURSOR_HIDE:
        red_channel_client_init_send_data(rcc, SPICE_MSG_CURSOR_HIDE);
        break;
    case QXL_CURSOR_TRAIL:
        {
            SpiceMsgCursorTrail cursor_trail;

            red_channel_client_init_send_data(rcc, SPICE_MSG_CURSOR_TRAIL);
            cursor_trail.length = cmd->u.trail.length;
            cursor_trail.frequency = cmd->u.trail.frequency;
            spice_marshall_msg_cursor_trail(m, &cursor_trail);
        }
        break;
    default:
        spice_error("bad cursor command %d", cmd->type);
    }
}

static inline void red_marshall_inval(RedChannelClient *rcc,
                                      SpiceMarshaller *base_marshaller,
                                      RedCacheItem *cach_item)
{
    SpiceMsgDisplayInvalOne inval_one;

    red_channel_client_init_send_data(rcc, SPICE_MSG_CURSOR_INVAL_ONE);
    inval_one.id = cach_item->id;

    spice_marshall_msg_cursor_inval_one(base_marshaller, &inval_one);
}

static void cursor_channel_send_item(RedChannelClient *rcc, RedPipeItem *pipe_item)
{
    SpiceMarshaller *m = red_channel_client_get_marshaller(rcc);
    CursorChannelClient *ccc = CURSOR_CHANNEL_CLIENT(rcc);

    switch (pipe_item->type) {
    case RED_PIPE_ITEM_TYPE_CURSOR:
        cursor_marshall(ccc, m, SPICE_UPCAST(RedCursorPipeItem, pipe_item));
        break;
    case RED_PIPE_ITEM_TYPE_INVAL_ONE:
        red_marshall_inval(rcc, m, SPICE_CONTAINEROF(pipe_item, RedCacheItem, u.pipe_data));
        break;
    case RED_PIPE_ITEM_TYPE_CURSOR_INIT:
        cursor_channel_client_reset_cursor_cache(rcc);
        red_marshall_cursor_init(ccc, m, pipe_item);
        break;
    case RED_PIPE_ITEM_TYPE_INVAL_CURSOR_CACHE:
        cursor_channel_client_reset_cursor_cache(rcc);
        red_channel_client_init_send_data(rcc, SPICE_MSG_CURSOR_INVAL_ALL);
        break;
    default:
        spice_error("invalid pipe item type");
    }

    red_channel_client_begin_send_message(rcc);
}

CursorChannel* cursor_channel_new(RedsState *server, QXLInstance *qxl,
                                  const SpiceCoreInterfaceInternal *core)
{
    spice_debug("create cursor channel");
    return g_object_new(TYPE_CURSOR_CHANNEL,
                        "spice-server", server,
                        "core-interface", core,
                        "channel-type", SPICE_CHANNEL_CURSOR,
                        "id", qxl->id,
                        "migration-flags", 0,
                        "qxl", qxl,
                        "handle-acks", TRUE,
                        NULL);
}

void cursor_channel_process_cmd(CursorChannel *cursor, RedCursorCmd *cursor_cmd)
{
    CursorItem *cursor_item;
    bool cursor_show = false;
    QXLInstance *qxl;

    spice_return_if_fail(cursor);
    spice_return_if_fail(cursor_cmd);

    qxl = common_graphics_channel_get_qxl(COMMON_GRAPHICS_CHANNEL(cursor));
    cursor_item = cursor_item_new(qxl, cursor_cmd);

    switch (cursor_cmd->type) {
    case QXL_CURSOR_SET:
        cursor->cursor_visible = !!cursor_cmd->u.set.visible;
        cursor_channel_set_item(cursor, cursor_item);
        break;
    case QXL_CURSOR_MOVE:
        cursor_show = !cursor->cursor_visible;
        cursor->cursor_visible = true;
        cursor->cursor_position = cursor_cmd->u.position;
        break;
    case QXL_CURSOR_HIDE:
        cursor->cursor_visible = false;
        break;
    case QXL_CURSOR_TRAIL:
        cursor->cursor_trail_length = cursor_cmd->u.trail.length;
        cursor->cursor_trail_frequency = cursor_cmd->u.trail.frequency;
        break;
    default:
        spice_warning("invalid cursor command %u", cursor_cmd->type);
        return;
    }

    if (red_channel_is_connected(RED_CHANNEL(cursor)) &&
        (cursor->mouse_mode == SPICE_MOUSE_MODE_SERVER
         || cursor_cmd->type != QXL_CURSOR_MOVE
         || cursor_show)) {
        red_channel_pipes_new_add(RED_CHANNEL(cursor),
                                  new_cursor_pipe_item, cursor_item);
    }

    cursor_item_unref(cursor_item);
}

void cursor_channel_reset(CursorChannel *cursor)
{
    RedChannel *channel = RED_CHANNEL(cursor);

    spice_return_if_fail(cursor);

    cursor_channel_set_item(cursor, NULL);
    cursor->cursor_visible = true;
    cursor->cursor_position.x = cursor->cursor_position.y = 0;
    cursor->cursor_trail_length = cursor->cursor_trail_frequency = 0;

    if (red_channel_is_connected(channel)) {
        red_channel_pipes_add_type(channel, RED_PIPE_ITEM_TYPE_INVAL_CURSOR_CACHE);
        if (!common_graphics_channel_get_during_target_migrate(COMMON_GRAPHICS_CHANNEL(cursor))) {
            red_channel_pipes_add_empty_msg(channel, SPICE_MSG_CURSOR_RESET);
        }
        if (!red_channel_wait_all_sent(channel,
                                       COMMON_CLIENT_TIMEOUT)) {
            red_channel_apply_clients(channel,
                                      red_channel_client_disconnect_if_pending_send);
        }
    }
}

static void cursor_channel_init_client(CursorChannel *cursor, CursorChannelClient *client)
{
    spice_return_if_fail(cursor);

    if (!red_channel_is_connected(RED_CHANNEL(cursor))
        || common_graphics_channel_get_during_target_migrate(COMMON_GRAPHICS_CHANNEL(cursor))) {
        spice_debug("during_target_migrate: skip init");
        return;
    }

    if (client)
        red_channel_client_pipe_add_type(RED_CHANNEL_CLIENT(client),
                                         RED_PIPE_ITEM_TYPE_CURSOR_INIT);
    else
        red_channel_pipes_add_type(RED_CHANNEL(cursor), RED_PIPE_ITEM_TYPE_CURSOR_INIT);
}

void cursor_channel_do_init(CursorChannel *cursor)
{
    cursor_channel_init_client(cursor, NULL);
}

void cursor_channel_set_mouse_mode(CursorChannel *cursor, uint32_t mode)
{
    spice_return_if_fail(cursor);

    cursor->mouse_mode = mode;
}

void cursor_channel_connect(CursorChannel *cursor, RedClient *client, RedsStream *stream,
                            int migrate,
                            RedChannelCapabilities *caps)
{
    CursorChannelClient *ccc;

    spice_return_if_fail(cursor != NULL);

    spice_debug("add cursor channel client");
    ccc = cursor_channel_client_new(cursor, client, stream,
                                    migrate,
                                    caps);
    spice_return_if_fail(ccc != NULL);

    RedChannelClient *rcc = RED_CHANNEL_CLIENT(ccc);
    red_channel_client_ack_zero_messages_window(rcc);
    red_channel_client_push_set_ack(rcc);

    cursor_channel_init_client(cursor, ccc);
}

static void
cursor_channel_finalize(GObject *object)
{
    CursorChannel *self = CURSOR_CHANNEL(object);

    if (self->item) {
        cursor_item_unref(self->item);
    }

    G_OBJECT_CLASS(cursor_channel_parent_class)->finalize(object);
}

static void
cursor_channel_class_init(CursorChannelClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS(klass);
    RedChannelClass *channel_class = RED_CHANNEL_CLASS(klass);

    object_class->finalize = cursor_channel_finalize;

    channel_class->parser = spice_get_client_channel_parser(SPICE_CHANNEL_CURSOR, NULL);
    channel_class->handle_message = red_channel_client_handle_message;

    channel_class->on_disconnect =  cursor_channel_client_on_disconnect;
    channel_class->send_item = cursor_channel_send_item;
}

static void
cursor_channel_init(CursorChannel *self)
{
    self->cursor_visible = true;
    self->mouse_mode = SPICE_MOUSE_MODE_SERVER;
}
