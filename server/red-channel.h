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


    Author:
        yhalperi@redhat.com
*/

#ifndef _H_RED_CHANNEL
#define _H_RED_CHANNEL

#include <pthread.h>
#include <limits.h>
#include <glib-object.h>
#include <common/ring.h>
#include <common/marshaller.h>

#include "demarshallers.h"
#include "spice.h"
#include "red-common.h"
#include "reds-stream.h"
#include "stat.h"
#include "red-pipe-item.h"

G_BEGIN_DECLS

typedef struct SpiceDataHeaderOpaque SpiceDataHeaderOpaque;

typedef uint16_t (*get_msg_type_proc)(SpiceDataHeaderOpaque *header);
typedef uint32_t (*get_msg_size_proc)(SpiceDataHeaderOpaque *header);
typedef void (*set_msg_type_proc)(SpiceDataHeaderOpaque *header, uint16_t type);
typedef void (*set_msg_size_proc)(SpiceDataHeaderOpaque *header, uint32_t size);
typedef void (*set_msg_serial_proc)(SpiceDataHeaderOpaque *header, uint64_t serial);
typedef void (*set_msg_sub_list_proc)(SpiceDataHeaderOpaque *header, uint32_t sub_list);

struct SpiceDataHeaderOpaque {
    uint8_t *data;
    uint16_t header_size;

    set_msg_type_proc set_msg_type;
    set_msg_size_proc set_msg_size;
    set_msg_serial_proc set_msg_serial;
    set_msg_sub_list_proc set_msg_sub_list;

    get_msg_type_proc get_msg_type;
    get_msg_size_proc get_msg_size;
};

typedef int (*handle_message_proc)(void *opaque,
                                   uint16_t type, uint32_t size, uint8_t *msg);
typedef int (*handle_parsed_proc)(void *opaque, uint32_t size, uint16_t type, void *message);
typedef uint8_t *(*alloc_msg_recv_buf_proc)(void *opaque, uint16_t type, uint32_t size);
typedef void (*release_msg_recv_buf_proc)(void *opaque,
                                          uint16_t type, uint32_t size, uint8_t *msg);
typedef void (*on_incoming_error_proc)(void *opaque);
typedef void (*on_input_proc)(void *opaque, int n);

typedef struct IncomingHandlerInterface {
    handle_message_proc handle_message;
    alloc_msg_recv_buf_proc alloc_msg_buf;
    on_incoming_error_proc on_error; // recv error or handle_message error
    release_msg_recv_buf_proc release_msg_buf; // for errors
    // The following is an optional alternative to handle_message, used if not null
    spice_parse_channel_func_t parser;
    handle_parsed_proc handle_parsed;
    on_input_proc on_input;
} IncomingHandlerInterface;

typedef int (*get_outgoing_msg_size_proc)(void *opaque);
typedef void (*prepare_outgoing_proc)(void *opaque, struct iovec *vec, int *vec_size, int pos);
typedef void (*on_outgoing_error_proc)(void *opaque);
typedef void (*on_outgoing_block_proc)(void *opaque);
typedef void (*on_outgoing_msg_done_proc)(void *opaque);
typedef void (*on_output_proc)(void *opaque, int n);

typedef struct OutgoingHandlerInterface {
    get_outgoing_msg_size_proc get_msg_size;
    prepare_outgoing_proc prepare;
    on_outgoing_error_proc on_error;
    on_outgoing_block_proc on_block;
    on_outgoing_msg_done_proc on_msg_done;
    on_output_proc on_output;
} OutgoingHandlerInterface;
/* Red Channel interface */

typedef struct RedChannel RedChannel;
typedef struct RedChannelClient RedChannelClient;
typedef struct RedClient RedClient;
typedef struct MainChannelClient MainChannelClient;

typedef uint8_t *(*channel_alloc_msg_recv_buf_proc)(RedChannelClient *channel,
                                                    uint16_t type, uint32_t size);
typedef int (*channel_handle_parsed_proc)(RedChannelClient *rcc, uint32_t size, uint16_t type,
                                        void *message);
typedef int (*channel_handle_message_proc)(RedChannelClient *rcc,
                                           uint16_t type, uint32_t size, uint8_t *msg);
typedef void (*channel_release_msg_recv_buf_proc)(RedChannelClient *channel,
                                                  uint16_t type, uint32_t size, uint8_t *msg);
typedef void (*channel_disconnect_proc)(RedChannelClient *rcc);
typedef int (*channel_configure_socket_proc)(RedChannelClient *rcc);
typedef void (*channel_send_pipe_item_proc)(RedChannelClient *rcc, RedPipeItem *item);
typedef void (*channel_on_incoming_error_proc)(RedChannelClient *rcc);
typedef void (*channel_on_outgoing_error_proc)(RedChannelClient *rcc);

typedef int (*channel_handle_migrate_flush_mark_proc)(RedChannelClient *base);
typedef int (*channel_handle_migrate_data_proc)(RedChannelClient *base,
                                                uint32_t size, void *message);
typedef uint64_t (*channel_handle_migrate_data_get_serial_proc)(RedChannelClient *base,
                                            uint32_t size, void *message);


typedef void (*channel_client_connect_proc)(RedChannel *channel, RedClient *client, RedsStream *stream,
                                            int migration, int num_common_caps, uint32_t *common_caps,
                                            int num_caps, uint32_t *caps);
typedef void (*channel_client_disconnect_proc)(RedChannelClient *base);
typedef void (*channel_client_migrate_proc)(RedChannelClient *base);


/*
 * callbacks that are triggered from client events.
 * They should be called from the thread that handles the RedClient
 */
typedef struct {
    channel_client_connect_proc connect;
    channel_client_disconnect_proc disconnect;
    channel_client_migrate_proc migrate;
} ClientCbs;

static inline gboolean test_capability(const uint32_t *caps, int num_caps, uint32_t cap)
{
    return VD_AGENT_HAS_CAPABILITY(caps, num_caps, cap);
}

#define RED_TYPE_CHANNEL red_channel_get_type()

#define RED_CHANNEL(obj) (G_TYPE_CHECK_INSTANCE_CAST((obj), RED_TYPE_CHANNEL, RedChannel))
#define RED_CHANNEL_CLASS(klass) \
    (G_TYPE_CHECK_CLASS_CAST((klass), RED_TYPE_CHANNEL, RedChannelClass))
#define RED_IS_CHANNEL(obj) (G_TYPE_CHECK_INSTANCE_TYPE((obj), RED_TYPE_CHANNEL))
#define RED_IS_CHANNEL_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE((klass), RED_TYPE_CHANNEL))
#define RED_CHANNEL_GET_CLASS(obj) \
    (G_TYPE_INSTANCE_GET_CLASS((obj), RED_TYPE_CHANNEL, RedChannelClass))

typedef struct RedChannelClass RedChannelClass;
typedef struct RedChannelPrivate RedChannelPrivate;

struct RedChannel
{
    GObject parent;

    RedChannelPrivate *priv;
};

struct RedChannelClass
{
    GObjectClass parent_class;

    /* subclasses must implement either handle_message(), or both parser() and
     * handle_parsed() */
    channel_handle_message_proc handle_message;
    spice_parse_channel_func_t parser;
    channel_handle_parsed_proc handle_parsed;

    // TODO: add ASSERTS for thread_id  in client and channel calls
    /*
     * callbacks that are triggered from channel client stream events.
     * They are called from the thread that listen to the stream events.
     */
    channel_configure_socket_proc config_socket;
    channel_disconnect_proc on_disconnect;
    channel_send_pipe_item_proc send_item;
    channel_alloc_msg_recv_buf_proc alloc_recv_buf;
    channel_release_msg_recv_buf_proc release_recv_buf;
    channel_handle_migrate_flush_mark_proc handle_migrate_flush_mark;
    channel_handle_migrate_data_proc handle_migrate_data;
    channel_handle_migrate_data_get_serial_proc handle_migrate_data_get_serial;
};

#define FOREACH_CLIENT(_channel, _iter, _data) \
    GLIST_FOREACH((_channel ? red_channel_get_clients(RED_CHANNEL(_channel)) : NULL), \
                  _iter, RedChannelClient, _data)

/* Red Channel interface */

typedef struct RedChannelCapabilities {
    int num_common_caps;
    uint32_t *common_caps;
    int num_caps;
    uint32_t *caps;
} RedChannelCapabilities;

GType red_channel_get_type(void) G_GNUC_CONST;

void red_channel_add_client(RedChannel *channel, RedChannelClient *rcc);
void red_channel_remove_client(RedChannel *channel, RedChannelClient *rcc);

void red_channel_set_stat_node(RedChannel *channel, StatNodeRef stat);

void red_channel_register_client_cbs(RedChannel *channel, const ClientCbs *client_cbs, gpointer cbs_data);
// caps are freed when the channel is destroyed
void red_channel_set_common_cap(RedChannel *channel, uint32_t cap);
void red_channel_set_cap(RedChannel *channel, uint32_t cap);

int red_channel_is_connected(RedChannel *channel);

/* seamless migration is supported for only one client. This routine
 * checks if the only channel client associated with channel is
 * waiting for migration data */
int red_channel_is_waiting_for_migrate_data(RedChannel *channel);

/*
 * the disconnect callback is called from the channel's thread,
 * i.e., for display channels - red worker thread, for all the other - from the main thread.
 * RedClient is managed from the main thread. red_channel_client_destroy can be called only
 * from red_client_destroy.
 */

void red_channel_destroy(RedChannel *channel);

/* return true if all the channel clients support the cap */
int red_channel_test_remote_common_cap(RedChannel *channel, uint32_t cap);
int red_channel_test_remote_cap(RedChannel *channel, uint32_t cap);

/* should be called when a new channel is ready to send messages */
void red_channel_init_outgoing_messages_window(RedChannel *channel);

// TODO: add back the channel_pipe_add functionality - by adding reference counting
// to the RedPipeItem.

// helper to push a new item to all channels
typedef RedPipeItem *(*new_pipe_item_t)(RedChannelClient *rcc, void *data, int num);
int red_channel_pipes_new_add_push(RedChannel *channel, new_pipe_item_t creator, void *data);
void red_channel_pipes_new_add(RedChannel *channel, new_pipe_item_t creator, void *data);

void red_channel_pipes_add_type(RedChannel *channel, int pipe_item_type);

void red_channel_pipes_add_empty_msg(RedChannel *channel, int msg_type);

/* return TRUE if all of the connected clients to this channel are blocked */
int red_channel_all_blocked(RedChannel *channel);

/* return TRUE if any of the connected clients to this channel are blocked */
int red_channel_any_blocked(RedChannel *channel);

int red_channel_no_item_being_sent(RedChannel *channel);

// TODO: unstaticed for display/cursor channels. they do some specific pushes not through
// adding elements or on events. but not sure if this is actually required (only result
// should be that they ""try"" a little harder, but if the event system is correct it
// should not make any difference.
void red_channel_push(RedChannel *channel);
// Again, used in various places outside of event handler context (or in other event handler
// contexts):
//  flush_display_commands/flush_cursor_commands
//  display_channel_wait_for_init
//  red_wait_outgoing_item
//  red_wait_pipe_item_sent
//  handle_channel_events - this is the only one that was used before, and was in red-channel.c
void red_channel_receive(RedChannel *channel);
// For red_worker
void red_channel_send(RedChannel *channel);
// For red_worker
void red_channel_disconnect(RedChannel *channel);
void red_channel_connect(RedChannel *channel, RedClient *client,
                         RedsStream *stream, int migration, int num_common_caps,
                         uint32_t *common_caps, int num_caps, uint32_t *caps);

/* return the sum of all the rcc pipe size */
uint32_t red_channel_max_pipe_size(RedChannel *channel);
/* return the min size of all the rcc pipe */
uint32_t red_channel_min_pipe_size(RedChannel *channel);
/* return the max size of all the rcc pipe */
uint32_t red_channel_sum_pipes_size(RedChannel *channel);

/* apply given function to all connected clients */
typedef void (*channel_client_callback)(RedChannelClient *rcc);
typedef void (*channel_client_callback_data)(RedChannelClient *rcc, void *data);
void red_channel_apply_clients(RedChannel *channel, channel_client_callback v);
void red_channel_apply_clients_data(RedChannel *channel, channel_client_callback_data v,
                                    void *data);
GList *red_channel_get_clients(RedChannel *channel);
guint red_channel_get_n_clients(RedChannel *channel);
struct RedsState* red_channel_get_server(RedChannel *channel);
SpiceCoreInterfaceInternal* red_channel_get_core_interface(RedChannel *channel);

/* channel callback function */
int red_channel_config_socket(RedChannel *self, RedChannelClient *rcc);
void red_channel_on_disconnect(RedChannel *self, RedChannelClient *rcc);
void red_channel_send_item(RedChannel *self, RedChannelClient *rcc, RedPipeItem *item);
void red_channel_reset_thread_id(RedChannel *self);
StatNodeRef red_channel_get_stat_node(RedChannel *channel);

/* FIXME: do these even need to be in RedChannel? It's really only used in
 * RedChannelClient. Needs refactoring */
IncomingHandlerInterface* red_channel_get_incoming_handler(RedChannel *self);
OutgoingHandlerInterface* red_channel_get_outgoing_handler(RedChannel *self);

const RedChannelCapabilities* red_channel_get_local_capabilities(RedChannel *self);

/*
 * blocking functions.
 *
 * timeout is in nano sec. -1 for no timeout.
 *
 * Return: TRUE if waiting succeeded. FALSE if timeout expired.
 */

int red_channel_wait_all_sent(RedChannel *channel,
                              int64_t timeout);

/* wrappers for client callbacks */
void red_channel_migrate_client(RedChannel *channel, RedChannelClient *rcc);
void red_channel_disconnect_client(RedChannel *channel, RedChannelClient *rcc);

#define CHANNEL_BLOCKED_SLEEP_DURATION 10000 //micro

G_END_DECLS

#endif
