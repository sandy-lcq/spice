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

#include "inputs-channel-client.h"
#include "migration-protocol.h"
#include "red-channel-client.h"

G_DEFINE_TYPE(InputsChannelClient, inputs_channel_client, RED_TYPE_CHANNEL_CLIENT)

#define INPUTS_CHANNEL_CLIENT_PRIVATE(o) \
    (G_TYPE_INSTANCE_GET_PRIVATE((o), TYPE_INPUTS_CHANNEL_CLIENT, InputsChannelClientPrivate))

struct InputsChannelClientPrivate
{
    uint16_t motion_count;
};

static void
inputs_channel_client_class_init(InputsChannelClientClass *klass)
{
    g_type_class_add_private(klass, sizeof(InputsChannelClientPrivate));
}

static void
inputs_channel_client_init(InputsChannelClient *self)
{
    self->priv = INPUTS_CHANNEL_CLIENT_PRIVATE(self);
}

RedChannelClient* inputs_channel_client_create(RedChannel *channel,
                                               RedClient *client,
                                               RedsStream *stream,
                                               int monitor_latency,
                                               int num_common_caps,
                                               uint32_t *common_caps,
                                               int num_caps,
                                               uint32_t *caps)
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
    rcc = g_initable_new(TYPE_INPUTS_CHANNEL_CLIENT,
                         NULL, NULL,
                         "channel", channel,
                         "client", client,
                         "stream", stream,
                         "monitor-latency", monitor_latency,
                         "caps", caps_array,
                         "common-caps", common_caps_array,
                         NULL);

    if (caps_array)
        g_array_unref(caps_array);
    if (common_caps_array)
        g_array_unref(common_caps_array);

    return rcc;
}

void inputs_channel_client_send_migrate_data(RedChannelClient *rcc,
                                             SpiceMarshaller *m,
                                             RedPipeItem *item)
{
    InputsChannelClient *icc = INPUTS_CHANNEL_CLIENT(rcc);

    red_channel_client_init_send_data(rcc, SPICE_MSG_MIGRATE_DATA, item);

    spice_marshaller_add_uint32(m, SPICE_MIGRATE_DATA_INPUTS_MAGIC);
    spice_marshaller_add_uint32(m, SPICE_MIGRATE_DATA_INPUTS_VERSION);
    spice_marshaller_add_uint16(m, icc->priv->motion_count);
}

void inputs_channel_client_handle_migrate_data(InputsChannelClient *icc,
                                               uint16_t motion_count)
{
    icc->priv->motion_count = motion_count;

    for (; icc->priv->motion_count >= SPICE_INPUT_MOTION_ACK_BUNCH;
           icc->priv->motion_count -= SPICE_INPUT_MOTION_ACK_BUNCH) {
        red_channel_client_pipe_add_type(RED_CHANNEL_CLIENT(icc),
                                         RED_PIPE_ITEM_MOUSE_MOTION_ACK);
    }
}

void inputs_channel_client_on_mouse_motion(InputsChannelClient *icc)
{
    InputsChannel *inputs_channel = INPUTS_CHANNEL(red_channel_client_get_channel(RED_CHANNEL_CLIENT(icc)));

    if (++icc->priv->motion_count % SPICE_INPUT_MOTION_ACK_BUNCH == 0 &&
        !inputs_channel_is_src_during_migrate(inputs_channel)) {
        red_channel_client_pipe_add_type(RED_CHANNEL_CLIENT(icc),
                                         RED_PIPE_ITEM_MOUSE_MOTION_ACK);
        icc->priv->motion_count = 0;
    }
}
