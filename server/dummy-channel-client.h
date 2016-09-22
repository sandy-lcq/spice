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
#ifndef __DUMMY_CHANNEL_CLIENT_H__
#define __DUMMY_CHANNEL_CLIENT_H__

#include <glib-object.h>

#include "red-channel-client.h"

G_BEGIN_DECLS

#define TYPE_DUMMY_CHANNEL_CLIENT dummy_channel_client_get_type()

#define DUMMY_CHANNEL_CLIENT(obj) \
    (G_TYPE_CHECK_INSTANCE_CAST ((obj), TYPE_DUMMY_CHANNEL_CLIENT, DummyChannelClient))
#define DUMMY_CHANNEL_CLIENT_CLASS(klass) \
    (G_TYPE_CHECK_CLASS_CAST ((klass), TYPE_DUMMY_CHANNEL_CLIENT, DummyChannelClientClass))
#define IS_DUMMY_CHANNEL_CLIENT(obj) \
    (G_TYPE_CHECK_INSTANCE_TYPE ((obj), TYPE_DUMMY_CHANNEL_CLIENT))
#define IS_DUMMY_CHANNEL_CLIENT_CLASS(klass) \
    (G_TYPE_CHECK_CLASS_TYPE ((klass), TYPE_DUMMY_CHANNEL_CLIENT))
#define DUMMY_CHANNEL_CLIENT_GET_CLASS(obj) \
    (G_TYPE_INSTANCE_GET_CLASS ((obj), TYPE_DUMMY_CHANNEL_CLIENT, DummyChannelClientClass))

typedef struct DummyChannelClient DummyChannelClient;
typedef struct DummyChannelClientClass DummyChannelClientClass;
typedef struct DummyChannelClientPrivate DummyChannelClientPrivate;

struct DummyChannelClient
{
    RedChannelClient parent;

    DummyChannelClientPrivate *priv;
};

struct DummyChannelClientClass
{
    RedChannelClientClass parent_class;
};

GType dummy_channel_client_get_type(void) G_GNUC_CONST;

RedChannelClient *dummy_channel_client_create(RedChannel *channel,
                                              RedClient  *client,
                                              int num_common_caps, uint32_t *common_caps,
                                              int num_caps, uint32_t *caps);

G_END_DECLS

#endif /* __DUMMY_CHANNEL_CLIENT_H__ */
