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

#include <glib-object.h>

#include "smartcard.h"

G_BEGIN_DECLS

#define TYPE_SMARTCARD_CHANNEL_CLIENT smart_card_channel_client_get_type()

#define SMARTCARD_CHANNEL_CLIENT(obj) \
    (G_TYPE_CHECK_INSTANCE_CAST((obj), TYPE_SMARTCARD_CHANNEL_CLIENT, SmartCardChannelClient))
#define SMARTCARD_CHANNEL_CLIENT_CLASS(klass) \
    (G_TYPE_CHECK_CLASS_CAST((klass), TYPE_SMARTCARD_CHANNEL_CLIENT, SmartCardChannelClientClass))
#define IS_SMARTCARD_CHANNEL_CLIENT(obj) \
    (G_TYPE_CHECK_INSTANCE_TYPE((obj), TYPE_SMARTCARD_CHANNEL_CLIENT))
#define IS_SMARTCARD_CHANNEL_CLIENT_CLASS(klass) \
    (G_TYPE_CHECK_CLASS_TYPE((klass), TYPE_SMARTCARD_CHANNEL_CLIENT))
#define SMARTCARD_CHANNEL_CLIENT_GET_CLASS(obj) \
    (G_TYPE_INSTANCE_GET_CLASS((obj), TYPE_SMARTCARD_CHANNEL_CLIENT, SmartCardChannelClientClass))

typedef struct SmartCardChannelClient SmartCardChannelClient;
typedef struct SmartCardChannelClientClass SmartCardChannelClientClass;
typedef struct SmartCardChannelClientPrivate SmartCardChannelClientPrivate;

struct SmartCardChannelClient
{
    RedChannelClient parent;

    SmartCardChannelClientPrivate *priv;
};

struct SmartCardChannelClientClass
{
    RedChannelClientClass parent_class;
};

GType smart_card_channel_client_get_type(void) G_GNUC_CONST;

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

G_END_DECLS

#endif /* SMARTCARD_CHANNEL_CLIENT_H__ */
