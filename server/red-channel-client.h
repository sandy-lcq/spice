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

#ifndef _H_RED_CHANNEL_CLIENT
#define _H_RED_CHANNEL_CLIENT

#include <common/marshaller.h>

#include "red-pipe-item.h"
#include "reds-stream.h"
#include "red-channel.h"
/* FIXME: remove */
#include "red-channel-client-private.h"

typedef struct RedChannel RedChannel;
typedef struct RedClient RedClient;
typedef struct IncomingHandler IncomingHandler;

typedef struct RedChannelClient RedChannelClient;
typedef struct RedChannelClientPrivate RedChannelClientPrivate;

/*
 * When an error occurs over a channel, we treat it as a warning
 * for spice-server and shutdown the channel.
 */
#define spice_channel_client_error(rcc, format, ...)                                     \
    do {                                                                                 \
        RedChannel *_ch = red_channel_client_get_channel(rcc);                           \
        spice_warning("rcc %p type %u id %u: " format, rcc,                              \
                    _ch->type, _ch->id, ## __VA_ARGS__);                                 \
        red_channel_client_shutdown(rcc);                                                \
    } while (0)

RedChannelClient *red_channel_client_create(int size, RedChannel *channel,
                                            RedClient *client, RedsStream *stream,
                                            int monitor_latency,
                                            int num_common_caps, uint32_t *common_caps,
                                            int num_caps, uint32_t *caps);

RedChannelClient *red_channel_client_create_dummy(int size,
                                                  RedChannel *channel,
                                                  RedClient  *client,
                                                  int num_common_caps, uint32_t *common_caps,
                                                  int num_caps, uint32_t *caps);

void red_channel_client_ref(RedChannelClient *rcc);
void red_channel_client_unref(RedChannelClient *rcc);

int red_channel_client_is_connected(RedChannelClient *rcc);
void red_channel_client_default_migrate(RedChannelClient *rcc);
int red_channel_client_is_waiting_for_migrate_data(RedChannelClient *rcc);
void red_channel_client_destroy(RedChannelClient *rcc);
int red_channel_client_test_remote_common_cap(RedChannelClient *rcc, uint32_t cap);
int red_channel_client_test_remote_cap(RedChannelClient *rcc, uint32_t cap);
/* shutdown is the only safe thing to do out of the client/channel
 * thread. It will not touch the rings, just shutdown the socket.
 * It should be followed by some way to gurantee a disconnection. */
void red_channel_client_shutdown(RedChannelClient *rcc);
/* handles general channel msgs from the client */
int red_channel_client_handle_message(RedChannelClient *rcc, uint32_t size,
                                      uint16_t type, void *message);
/* when preparing send_data: should call init and then use marshaller */
void red_channel_client_init_send_data(RedChannelClient *rcc, uint16_t msg_type, RedPipeItem *item);

uint64_t red_channel_client_get_message_serial(RedChannelClient *channel);
void red_channel_client_set_message_serial(RedChannelClient *channel, uint64_t);

/* When sending a msg. Should first call red_channel_client_begin_send_message.
 * It will first send the pending urgent data, if there is any, and then
 * the rest of the data.
 */
void red_channel_client_begin_send_message(RedChannelClient *rcc);

/*
 * Stores the current send data, and switches to urgent send data.
 * When it begins the actual send, it will send first the urgent data
 * and afterward the rest of the data.
 * Should be called only if during the marshalling of on message,
 * the need to send another message, before, rises.
 * Important: the serial of the non-urgent sent data, will be succeeded.
 * return: the urgent send data marshaller
 */
SpiceMarshaller *red_channel_client_switch_to_urgent_sender(RedChannelClient *rcc);

/* returns -1 if we don't have an estimation */
int red_channel_client_get_roundtrip_ms(RedChannelClient *rcc);

/* Checks periodically if the connection is still alive */
void red_channel_client_start_connectivity_monitoring(RedChannelClient *rcc, uint32_t timeout_ms);

void red_channel_client_pipe_add_push(RedChannelClient *rcc, RedPipeItem *item);
void red_channel_client_pipe_add(RedChannelClient *rcc, RedPipeItem *item);
void red_channel_client_pipe_add_after(RedChannelClient *rcc, RedPipeItem *item, RedPipeItem *pos);
void red_channel_client_pipe_add_after_pos(RedChannelClient *rcc, RedPipeItem *item, GList *pos);
int red_channel_client_pipe_item_is_linked(RedChannelClient *rcc, RedPipeItem *item);
void red_channel_client_pipe_remove_and_release(RedChannelClient *rcc, RedPipeItem *item);
void red_channel_client_pipe_remove_and_release_pos(RedChannelClient *rcc, GList *item_pos);
void red_channel_client_pipe_add_tail(RedChannelClient *rcc, RedPipeItem *item);
void red_channel_client_pipe_add_tail_and_push(RedChannelClient *rcc, RedPipeItem *item);
/* for types that use this routine -> the pipe item should be freed */
void red_channel_client_pipe_add_type(RedChannelClient *rcc, int pipe_item_type);
void red_channel_client_pipe_add_empty_msg(RedChannelClient *rcc, int msg_type);
gboolean red_channel_client_pipe_is_empty(RedChannelClient *rcc);
uint32_t red_channel_client_get_pipe_size(RedChannelClient *rcc);
GQueue* red_channel_client_get_pipe(RedChannelClient *rcc);
gboolean red_channel_client_is_mini_header(RedChannelClient *rcc);

void red_channel_client_ack_zero_messages_window(RedChannelClient *rcc);
void red_channel_client_ack_set_client_window(RedChannelClient *rcc, int client_window);
void red_channel_client_push_set_ack(RedChannelClient *rcc);

gboolean red_channel_client_is_blocked(RedChannelClient *rcc);

/* helper for channels that have complex logic that can possibly ready a send */
int red_channel_client_send_message_pending(RedChannelClient *rcc);

gboolean red_channel_client_no_item_being_sent(RedChannelClient *rcc);
void red_channel_client_push(RedChannelClient *rcc);
void red_channel_client_receive(RedChannelClient *rcc);
void red_channel_client_send(RedChannelClient *rcc);
void red_channel_client_disconnect(RedChannelClient *rcc);

/* Note: the valid times to call red_channel_get_marshaller are just during send_item callback. */
SpiceMarshaller *red_channel_client_get_marshaller(RedChannelClient *rcc);
RedsStream *red_channel_client_get_stream(RedChannelClient *rcc);
RedClient *red_channel_client_get_client(RedChannelClient *rcc);

/* Note that the header is valid only between red_channel_reset_send_data and
 * red_channel_begin_send_message.*/
void red_channel_client_set_header_sub_list(RedChannelClient *rcc, uint32_t sub_list);

/*
 * blocking functions.
 *
 * timeout is in nano sec. -1 for no timeout.
 *
 * Return: TRUE if waiting succeeded. FALSE if timeout expired.
 */

int red_channel_client_wait_pipe_item_sent(RedChannelClient *rcc,
                                           GList *item_pos,
                                           int64_t timeout);
int red_channel_client_wait_outgoing_item(RedChannelClient *rcc,
                                          int64_t timeout);
void red_channel_client_disconnect_if_pending_send(RedChannelClient *rcc);

RedChannel* red_channel_client_get_channel(RedChannelClient *rcc);

void red_channel_client_on_output(void *opaque, int n);
void red_channel_client_on_input(void *opaque, int n);
int red_channel_client_get_out_msg_size(void *opaque);
void red_channel_client_prepare_out_msg(void *opaque, struct iovec *vec,
                                             int *vec_size, int pos);
void red_channel_client_on_out_block(void *opaque);
void red_channel_client_on_out_msg_done(void *opaque);

void red_channel_client_semi_seamless_migration_complete(RedChannelClient *rcc);
void red_channel_client_init_outgoing_messages_window(RedChannelClient *rcc);

gboolean red_channel_client_set_migration_seamless(RedChannelClient *rcc);
void red_channel_client_set_destroying(RedChannelClient *rcc);
gboolean red_channel_client_is_destroying(RedChannelClient *rcc);

#define RED_CHANNEL_CLIENT(Client) ((RedChannelClient *)(Client))

typedef struct OutgoingHandler {
    OutgoingHandlerInterface *cb;
    void *opaque;
    struct iovec vec_buf[IOV_MAX];
    int vec_size;
    struct iovec *vec;
    int pos;
    int size;
} OutgoingHandler;

typedef struct IncomingHandler {
    IncomingHandlerInterface *cb;
    void *opaque;
    uint8_t header_buf[MAX_HEADER_SIZE];
    SpiceDataHeaderOpaque header;
    uint32_t header_pos;
    uint8_t *msg; // data of the msg following the header. allocated by alloc_msg_buf.
    uint32_t msg_pos;
    uint64_t serial;
} IncomingHandler;

struct RedChannelClient {
    /* protected */
    OutgoingHandler outgoing;
    IncomingHandler incoming;

    RedChannelClientPrivate priv[1];
};

#endif /* _H_RED_CHANNEL_CLIENT */
