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

#ifndef _COMMON_GRAPHICS_CHANNEL_H
#define _COMMON_GRAPHICS_CHANNEL_H

#include <glib-object.h>

#include "red-channel.h"
#include "red-channel-client.h"

G_BEGIN_DECLS

int common_channel_config_socket(RedChannelClient *rcc);

#define COMMON_CLIENT_TIMEOUT (NSEC_PER_SEC * 30)

#define TYPE_COMMON_GRAPHICS_CHANNEL common_graphics_channel_get_type()

#define COMMON_GRAPHICS_CHANNEL(obj) \
    (G_TYPE_CHECK_INSTANCE_CAST((obj), TYPE_COMMON_GRAPHICS_CHANNEL, CommonGraphicsChannel))
#define COMMON_GRAPHICS_CHANNEL_CLASS(klass) \
    (G_TYPE_CHECK_CLASS_CAST((klass), TYPE_COMMON_GRAPHICS_CHANNEL, CommonGraphicsChannelClass))
#define COMMON_IS_GRAPHICS_CHANNEL(obj) \
    (G_TYPE_CHECK_INSTANCE_TYPE((obj), TYPE_COMMON_GRAPHICS_CHANNEL))
#define COMMON_IS_GRAPHICS_CHANNEL_CLASS(klass) \
    (G_TYPE_CHECK_CLASS_TYPE((klass), TYPE_COMMON_GRAPHICS_CHANNEL))
#define COMMON_GRAPHICS_CHANNEL_GET_CLASS(obj) \
    (G_TYPE_INSTANCE_GET_CLASS((obj), TYPE_COMMON_GRAPHICS_CHANNEL, CommonGraphicsChannelClass))

typedef struct CommonGraphicsChannel CommonGraphicsChannel;
typedef struct CommonGraphicsChannelClass CommonGraphicsChannelClass;
typedef struct CommonGraphicsChannelPrivate CommonGraphicsChannelPrivate;

struct CommonGraphicsChannel
{
    RedChannel parent;

    CommonGraphicsChannelPrivate *priv;
};

struct CommonGraphicsChannelClass
{
    RedChannelClass parent_class;
};

GType common_graphics_channel_get_type(void) G_GNUC_CONST;

void common_graphics_channel_set_during_target_migrate(CommonGraphicsChannel *self, gboolean value);
gboolean common_graphics_channel_get_during_target_migrate(CommonGraphicsChannel *self);
QXLInstance* common_graphics_channel_get_qxl(CommonGraphicsChannel *self);

enum {
    RED_PIPE_ITEM_TYPE_VERB = RED_PIPE_ITEM_TYPE_CHANNEL_BASE,
    RED_PIPE_ITEM_TYPE_INVAL_ONE,

    RED_PIPE_ITEM_TYPE_COMMON_LAST
};

typedef struct RedVerbItem {
    RedPipeItem base;
    uint16_t verb;
} RedVerbItem;

static inline void red_marshall_verb(RedChannelClient *rcc, RedVerbItem *item)
{
    red_channel_client_init_send_data(rcc, item->verb, NULL);
}

static inline void red_pipe_add_verb(RedChannelClient* rcc, uint16_t verb)
{
    RedVerbItem *item = spice_new(RedVerbItem, 1);

    red_pipe_item_init(&item->base, RED_PIPE_ITEM_TYPE_VERB);
    item->verb = verb;
    red_channel_client_pipe_add(rcc, &item->base);
}

static inline void red_pipe_add_verb_proxy(RedChannelClient *rcc, gpointer data)
{
    uint16_t verb = GPOINTER_TO_UINT(data);
    red_pipe_add_verb(rcc, verb);
}


static inline void red_pipes_add_verb(RedChannel *channel, uint16_t verb)
{
    red_channel_apply_clients_data(channel, red_pipe_add_verb_proxy, GUINT_TO_POINTER(verb));
}

G_END_DECLS

#endif /* _COMMON_GRAPHICS_CHANNEL_H */
