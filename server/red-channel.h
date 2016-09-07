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
#include <common/ring.h>
#include <common/marshaller.h>

#include "spice.h"
#include "red-common.h"
#include "demarshallers.h"
#include "reds-stream.h"
#include "stat.h"
#include "red-pipe-item.h"

#define MAX_SEND_BUFS 1000
#define CLIENT_ACK_WINDOW 20

#ifndef IOV_MAX
#define IOV_MAX 1024
#endif

#define MAX_HEADER_SIZE sizeof(SpiceDataHeader)

/* Basic interface for channels, without using the RedChannel interface.
   The intention is to move towards one channel interface gradually.
   At the final stage, this interface shouldn't be exposed. Only RedChannel will use it. */

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

/* Messages handled by red_channel
 * SET_ACK - sent to client on channel connection
 * Note that the numbers don't have to correspond to spice message types,
 * but we keep the 100 first allocated for base channel approach.
 * */
enum {
    RED_PIPE_ITEM_TYPE_SET_ACK=1,
    RED_PIPE_ITEM_TYPE_MIGRATE,
    RED_PIPE_ITEM_TYPE_EMPTY_MSG,
    RED_PIPE_ITEM_TYPE_PING,
    RED_PIPE_ITEM_TYPE_MARKER,

    RED_PIPE_ITEM_TYPE_CHANNEL_BASE=101,
};

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

// TODO: add ASSERTS for thread_id  in client and channel calls
//
/*
 * callbacks that are triggered from channel client stream events.
 * They are called from the thread that listen to the stream events.
 */
typedef struct {
    channel_configure_socket_proc config_socket;
    channel_disconnect_proc on_disconnect;
    channel_send_pipe_item_proc send_item;
    channel_alloc_msg_recv_buf_proc alloc_recv_buf;
    channel_release_msg_recv_buf_proc release_recv_buf;
    channel_handle_migrate_flush_mark_proc handle_migrate_flush_mark;
    channel_handle_migrate_data_proc handle_migrate_data;
    channel_handle_migrate_data_get_serial_proc handle_migrate_data_get_serial;
} ChannelCbs;


/*
 * callbacks that are triggered from client events.
 * They should be called from the thread that handles the RedClient
 */
typedef struct {
    channel_client_connect_proc connect;
    channel_client_disconnect_proc disconnect;
    channel_client_migrate_proc migrate;
} ClientCbs;

typedef struct RedChannelCapabilities {
    int num_common_caps;
    uint32_t *common_caps;
    int num_caps;
    uint32_t *caps;
} RedChannelCapabilities;

int test_capability(const uint32_t *caps, int num_caps, uint32_t cap);

typedef struct RedChannelClientLatencyMonitor {
    int state;
    uint64_t last_pong_time;
    SpiceTimer *timer;
    uint32_t id;
    int tcp_nodelay;
    int warmup_was_sent;

    int64_t roundtrip;
} RedChannelClientLatencyMonitor;

typedef struct RedChannelClientConnectivityMonitor {
    int state;
    uint32_t out_bytes;
    uint32_t in_bytes;
    uint32_t timeout;
    SpiceTimer *timer;
} RedChannelClientConnectivityMonitor;

struct RedChannel {
    uint32_t type;
    uint32_t id;

    uint32_t refs;

    RingItem link; // channels link for reds

    const SpiceCoreInterfaceInternal *core;
    int handle_acks;

    // RedChannel will hold only connected channel clients (logic - when pushing pipe item to all channel clients, there
    // is no need to go over disconnect clients)
    // . While client will hold the channel clients till it is destroyed
    // and then it will destroy them as well.
    // However RCC still holds a reference to the Channel.
    // Maybe replace these logic with ref count?
    // TODO: rename to 'connected_clients'?
    GList *clients;

    OutgoingHandlerInterface outgoing_cb;
    IncomingHandlerInterface incoming_cb;

    ChannelCbs channel_cbs;
    ClientCbs client_cbs;

    RedChannelCapabilities local_caps;
    uint32_t migration_flags;

    void *data;

    // TODO: when different channel_clients are in different threads from Channel -> need to protect!
    pthread_t thread_id;
    RedsState *reds;
#ifdef RED_STATISTICS
    StatNodeRef stat;
    uint64_t *out_bytes_counter;
#endif
};

#define FOREACH_CLIENT(channel, _link, _next, _data)                   \
    for (_link = (channel ? RED_CHANNEL(channel)->clients : NULL), \
         _next = (_link ? _link->next : NULL), \
         _data = (_link ? _link->data : NULL); \
         _link; \
         _link = _next, \
         _next = (_link ? _link->next : NULL), \
         _data = (_link ? _link->data : NULL))


#define RED_CHANNEL(Channel) ((RedChannel *)(Channel))

/* if one of the callbacks should cause disconnect, use red_channel_shutdown and don't
 * explicitly destroy the channel */
RedChannel *red_channel_create(int size,
                               RedsState *reds,
                               const SpiceCoreInterfaceInternal *core,
                               uint32_t type, uint32_t id,
                               int handle_acks,
                               channel_handle_message_proc handle_message,
                               const ChannelCbs *channel_cbs,
                               uint32_t migration_flags);

/* alternative constructor, meant for marshaller based (inputs,main) channels,
 * will become default eventually */
RedChannel *red_channel_create_parser(int size,
                                      RedsState *reds,
                                      const SpiceCoreInterfaceInternal *core,
                                      uint32_t type, uint32_t id,
                                      int handle_acks,
                                      spice_parse_channel_func_t parser,
                                      channel_handle_parsed_proc handle_parsed,
                                      const ChannelCbs *channel_cbs,
                                      uint32_t migration_flags);
void red_channel_ref(RedChannel *channel);
void red_channel_unref(RedChannel *channel);
void red_channel_add_client(RedChannel *channel, RedChannelClient *rcc);
void red_channel_remove_client(RedChannel *channel, RedChannelClient *rcc);

void red_channel_set_stat_node(RedChannel *channel, StatNodeRef stat);

void red_channel_register_client_cbs(RedChannel *channel, const ClientCbs *client_cbs, gpointer cbs_data);
// caps are freed when the channel is destroyed
void red_channel_set_common_cap(RedChannel *channel, uint32_t cap);
void red_channel_set_cap(RedChannel *channel, uint32_t cap);

// TODO: tmp, for channels that don't use RedChannel yet (e.g., snd channel), but
// do use the client callbacks. So the channel clients are not connected (the channel doesn't
// have list of them, but they do have a link to the channel, and the client has a list of them)
RedChannel *red_channel_create_dummy(int size, RedsState *reds, uint32_t type, uint32_t id);

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
void red_channel_pipes_new_add_tail(RedChannel *channel, new_pipe_item_t creator, void *data);

void red_channel_pipes_add_type(RedChannel *channel, int pipe_item_type);

void red_channel_pipes_add_empty_msg(RedChannel *channel, int msg_type);

int red_channel_get_first_socket(RedChannel *channel);

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
void red_channel_apply_clients_data(RedChannel *channel, channel_client_callback_data v, void * data);
struct RedsState* red_channel_get_server(RedChannel *channel);

struct RedClient {
    RedsState *reds;
    RingItem link;
    GList *channels;
    MainChannelClient *mcc;
    pthread_mutex_t lock; // different channels can be in different threads

    pthread_t thread_id;

    int disconnecting;
    /* Note that while semi-seamless migration is conducted by the main thread, seamless migration
     * involves all channels, and thus the related varaibles can be accessed from different
     * threads */
    int during_target_migrate; /* if seamless=TRUE, migration_target is turned off when all
                                  the clients received their migration data. Otherwise (semi-seamless),
                                  it is turned off, when red_client_semi_seamless_migrate_complete
                                  is called */
    int seamless_migrate;
    int num_migrated_channels; /* for seamless - number of channels that wait for migrate data*/
    int refs;
};

RedClient *red_client_new(RedsState *reds, int migrated);

/*
 * disconnects all the client's channels (should be called from the client's thread)
 */
void red_client_destroy(RedClient *client);

RedClient *red_client_ref(RedClient *client);

/*
 * releases the client resources when refs == 0.
 * We assume the red_client_derstroy was called before
 * we reached refs==0
 */
RedClient *red_client_unref(RedClient *client);

/* client->lock should be locked */
void red_client_add_channel(RedClient *client, RedChannelClient *rcc);
void red_client_remove_channel(RedChannelClient *rcc);
RedChannelClient *red_client_get_channel(RedClient *client, int type, int id);

MainChannelClient *red_client_get_main(RedClient *client);
// main should be set once before all the other channels are created
void red_client_set_main(RedClient *client, MainChannelClient *mcc);

/* called when the migration handshake results in seamless migration (dst side).
 * By default we assume semi-seamless */
void red_client_set_migration_seamless(RedClient *client);
void red_client_semi_seamless_migrate_complete(RedClient *client); /* dst side */
/* TRUE if the migration is seamless and there are still channels that wait from migration data.
 * Or, during semi-seamless migration, and the main channel still waits for MIGRATE_END
 * from the client.
 * Note: Call it only from the main thread */
int red_client_during_migrate_at_target(RedClient *client);

void red_client_migrate(RedClient *client);
gboolean red_client_seamless_migration_done_for_channel(RedClient *client);
/*
 * blocking functions.
 *
 * timeout is in nano sec. -1 for no timeout.
 *
 * Return: TRUE if waiting succeeded. FALSE if timeout expired.
 */

int red_channel_wait_all_sent(RedChannel *channel,
                              int64_t timeout);

#define CHANNEL_BLOCKED_SLEEP_DURATION 10000 //micro

#endif
