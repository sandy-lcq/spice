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

#ifndef SMARTCARD_CHANNEL_CLIENT_H__
#define SMARTCARD_CHANNEL_CLIENT_H__

#include "smartcard.h"
#include "red-channel-client.h"

typedef struct SmartCardChannelClientPrivate SmartCardChannelClientPrivate;
struct SmartCardChannelClientPrivate {
    RedCharDeviceSmartcard *smartcard;

    /* read_from_client/write_to_device buffer.
     * The beginning of the buffer should always be VSCMsgHeader*/
    RedCharDeviceWriteBuffer *write_buf;
    int msg_in_write_buf; /* was the client msg received into a RedCharDeviceWriteBuffer
                           * or was it explicitly malloced */
};

typedef struct SmartCardChannelClient {
    RedChannelClient base;

    SmartCardChannelClientPrivate priv[1];
} SmartCardChannelClient;

#define SMARTCARD_CHANNEL_CLIENT(rcc) ((SmartCardChannelClient*)rcc)

SmartCardChannelClient* smartcard_channel_client_create(RedChannel *channel,
                                                        RedClient *client, RedsStream *stream,
                                                        int monitor_latency,
                                                        int num_common_caps, uint32_t *common_caps,
                                                        int num_caps, uint32_t *caps);

uint8_t* smartcard_channel_client_alloc_msg_rcv_buf(RedChannelClient *rcc,
                                                    uint16_t type,
                                                    uint32_t size);

void smartcard_channel_client_release_msg_rcv_buf(RedChannelClient *rcc,
                                                  uint16_t type,
                                                  uint32_t size,
                                                  uint8_t *msg);

int smartcard_channel_client_handle_migrate_flush_mark(RedChannelClient *rcc);

void smartcard_channel_client_on_disconnect(RedChannelClient *rcc);

void smartcard_channel_client_send_data(RedChannelClient *rcc,
                                        SpiceMarshaller *m,
                                        RedPipeItem *item,
                                        VSCMsgHeader *vheader);

void smartcard_channel_client_send_error(RedChannelClient *rcc,
                                         SpiceMarshaller *m,
                                         RedPipeItem *item);

RedCharDeviceSmartcard* smartcard_channel_client_get_device(SmartCardChannelClient *scc);

int smartcard_channel_client_handle_message(RedChannelClient *rcc,
                                            uint16_t type,
                                            uint32_t size,
                                            uint8_t *msg);

int smartcard_channel_client_handle_migrate_data(RedChannelClient *rcc,
                                                 uint32_t size,
                                                 void *message);

void smartcard_channel_client_set_char_device(SmartCardChannelClient *scc,
                                              RedCharDeviceSmartcard *device);

RedCharDeviceSmartcard* smartcard_channel_client_get_char_device(SmartCardChannelClient *scc);

void smartcard_channel_client_release_msg_rcv_buf(RedChannelClient *rcc,
                                                  uint16_t type,
                                                  uint32_t size,
                                                  uint8_t *msg);

uint8_t *smartcard_channel_client_alloc_msg_rcv_buf(RedChannelClient *rcc,
                                                    uint16_t type,
                                                    uint32_t size);

#endif /* SMARTCARD_CHANNEL_CLIENT_H__ */
