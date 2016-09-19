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
#include "inputs-channel.h"
#include "migration-protocol.h"
#include "red-channel-client.h"

typedef struct InputsChannelClientPrivate InputsChannelClientPrivate;
struct InputsChannelClientPrivate
{
    uint16_t motion_count;
};

struct InputsChannelClient
{
    RedChannelClient base;

    InputsChannelClientPrivate priv[1];
};

RedChannelClient* inputs_channel_client_create(RedChannel *channel,
                                               RedClient *client,
                                               RedsStream *stream,
                                               int monitor_latency,
                                               int num_common_caps,
                                               uint32_t *common_caps,
                                               int num_caps,
                                               uint32_t *caps)
{
    InputsChannelClient* icc =
        INPUTS_CHANNEL_CLIENT(red_channel_client_create(sizeof(InputsChannelClient),
                                                        channel, client,
                                                        stream,
                                                        monitor_latency,
                                                        num_common_caps,
                                                        common_caps, num_caps,
                                                        caps));
    if (icc) {
        icc->priv->motion_count = 0;
    }
    return RED_CHANNEL_CLIENT(icc);
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
    InputsChannel *inputs_channel = (InputsChannel *)red_channel_client_get_channel(RED_CHANNEL_CLIENT(icc));

    if (++icc->priv->motion_count % SPICE_INPUT_MOTION_ACK_BUNCH == 0 &&
        !inputs_channel_is_src_during_migrate(inputs_channel)) {
        red_channel_client_pipe_add_type(RED_CHANNEL_CLIENT(icc),
                                         RED_PIPE_ITEM_MOUSE_MOTION_ACK);
        icc->priv->motion_count = 0;
    }
}
