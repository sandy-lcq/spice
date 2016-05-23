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

#ifndef _INPUTS_CHANNEL_CLIENT_H_
#define _INPUTS_CHANNEL_CLIENT_H_

#include "red-channel.h"

typedef struct InputsChannelClient InputsChannelClient;

RedChannelClient* inputs_channel_client_create(RedChannel *channel,
                                               RedClient *client,
                                               RedsStream *stream,
                                               int monitor_latency,
                                               int num_common_caps,
                                               uint32_t *common_caps,
                                               int num_caps,
                                               uint32_t *caps);

void inputs_channel_client_send_migrate_data(RedChannelClient *rcc,
                                             SpiceMarshaller *m,
                                             RedPipeItem *item);
void inputs_channel_client_handle_migrate_data(InputsChannelClient *icc,
                                               uint16_t motion_count);
void inputs_channel_client_on_mouse_motion(InputsChannelClient *icc);

enum {
    RED_PIPE_ITEM_INPUTS_INIT = RED_PIPE_ITEM_TYPE_CHANNEL_BASE,
    RED_PIPE_ITEM_MOUSE_MOTION_ACK,
    RED_PIPE_ITEM_KEY_MODIFIERS,
    RED_PIPE_ITEM_MIGRATE_DATA,
};

#endif /* _INPUTS_CHANNEL_CLIENT_H_ */
