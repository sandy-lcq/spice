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
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "dummy-channel-client.h"
#include "red-channel.h"
#include "red-client.h"

static void dummy_channel_client_initable_interface_init(GInitableIface *iface);

G_DEFINE_TYPE_WITH_CODE(DummyChannelClient, dummy_channel_client, RED_TYPE_CHANNEL_CLIENT,
                        G_IMPLEMENT_INTERFACE(G_TYPE_INITABLE,
                                              dummy_channel_client_initable_interface_init))

#define DUMMY_CHANNEL_CLIENT_PRIVATE(o) \
    (G_TYPE_INSTANCE_GET_PRIVATE((o), TYPE_DUMMY_CHANNEL_CLIENT, DummyChannelClientPrivate))

struct DummyChannelClientPrivate
{
    gboolean connected;
};

static gboolean dummy_channel_client_initable_init(GInitable *initable,
                                                   GCancellable *cancellable,
                                                   GError **error)
{
    GError *local_error = NULL;
    RedChannelClient *rcc = RED_CHANNEL_CLIENT(initable);
    RedClient *client = red_channel_client_get_client(rcc);
    RedChannel *channel = red_channel_client_get_channel(rcc);

    red_channel_add_client(channel, rcc);
    if (!red_client_add_channel(client, rcc, &local_error)) {
        red_channel_remove_client(channel, rcc);
    }

    if (local_error) {
        g_warning("Failed to create channel client: %s", local_error->message);
        g_propagate_error(error, local_error);
    }
    return local_error == NULL;
}

static void dummy_channel_client_initable_interface_init(GInitableIface *iface)
{
    iface->init = dummy_channel_client_initable_init;
}

static gboolean dummy_channel_client_is_connected(RedChannelClient *rcc)
{
    return DUMMY_CHANNEL_CLIENT(rcc)->priv->connected;
}

static void dummy_channel_client_disconnect(RedChannelClient *rcc)
{
    DummyChannelClient *self = DUMMY_CHANNEL_CLIENT(rcc);
    RedChannel *channel = red_channel_client_get_channel(rcc);
    GList *link;
    uint32_t type, id;

    if (channel && (link = g_list_find(red_channel_get_clients(channel), rcc))) {
        g_object_get(channel, "channel-type", &type, "id", &id, NULL);
        spice_printerr("rcc=%p (channel=%p type=%d id=%d)", rcc, channel,
                       type, id);
        red_channel_remove_client(channel, link->data);
    }
    self->priv->connected = FALSE;
}

static void
dummy_channel_client_class_init(DummyChannelClientClass *klass)
{
    RedChannelClientClass *cc_class = RED_CHANNEL_CLIENT_CLASS(klass);

    g_type_class_add_private(klass, sizeof(DummyChannelClientPrivate));

    cc_class->is_connected = dummy_channel_client_is_connected;
    cc_class->disconnect = dummy_channel_client_disconnect;
}

static void
dummy_channel_client_init(DummyChannelClient *self)
{
    self->priv = DUMMY_CHANNEL_CLIENT_PRIVATE(self);

    self->priv->connected = TRUE;
}

RedChannelClient* dummy_channel_client_create(RedChannel *channel,
                                              RedClient  *client,
                                              int num_common_caps,
                                              uint32_t *common_caps,
                                              int num_caps, uint32_t *caps)
{
    RedChannelClient *rcc;
    GArray *common_caps_array = NULL, *caps_array = NULL;

    if (common_caps) {
        common_caps_array = g_array_sized_new(FALSE, FALSE, sizeof (*common_caps),
                                              num_common_caps);
        g_array_append_vals(common_caps_array, common_caps, num_common_caps);
    }
    if (caps) {
        caps_array = g_array_sized_new(FALSE, FALSE, sizeof (*caps), num_caps);
        g_array_append_vals(caps_array, caps, num_caps);
    }

    rcc = g_initable_new(TYPE_DUMMY_CHANNEL_CLIENT,
                         NULL, NULL,
                         "channel", channel,
                         "client", client,
                         "caps", caps_array,
                         "common-caps", common_caps_array,
                         NULL);

    if (caps_array)
        g_array_unref(caps_array);
    if (common_caps_array)
        g_array_unref(common_caps_array);

    return rcc;
}
