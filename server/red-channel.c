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
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <common/ring.h>

#include "red-channel.h"
#include "red-channel-client.h"
#include "reds.h"
#include "reds-stream.h"
#include "main-dispatcher.h"
#include "utils.h"

/*
 * Lifetime of RedChannel, RedChannelClient and RedClient:
 * RedChannel is created and destroyed by the calls to
 * red_channel_create.* and red_channel_destroy. The RedChannel resources
 * are deallocated only after red_channel_destroy is called and no RedChannelClient
 * refers to the channel.
 * RedChannelClient is created and destroyed by the calls to red_channel_client_create
 * and red_channel_client_destroy. RedChannelClient resources are deallocated only when
 * its refs == 0. The reference count of RedChannelClient can be increased by routines
 * that include calls that might destroy the red_channel_client. For example,
 * red_peer_handle_incoming calls the handle_message proc of the channel, which
 * might lead to destroying the client. However, after the call to handle_message,
 * there is a call to the channel's release_msg_buf proc.
 *
 * Once red_channel_client_destroy is called, the RedChannelClient is disconnected and
 * removed from the RedChannel clients list, but if rcc->refs != 0, it will still hold
 * a reference to the Channel. The reason for this is that on the one hand RedChannel holds
 * callbacks that may be still in use by RedChannel, and on the other hand,
 * when an operation is performed on the list of clients that belongs to the channel,
 * we don't want to execute it on the "to be destroyed" channel client.
 *
 * RedClient is created and destroyed by the calls to red_client_new and red_client_destroy.
 * When it is destroyed, it also disconnects and destroys all the RedChannelClients that
 * are associated with it. However, since part of these channel clients may still have
 * other references, they will not be completely released, until they are dereferenced.
 *
 * Note: red_channel_client_destroy is not thread safe, and still it is called from
 * red_client_destroy (from the client's thread). However, since before this call,
 * red_client_destroy calls rcc->channel->client_cbs.disconnect(rcc), which is synchronous,
 * we assume that if the channel is in another thread, it does no longer have references to
 * this channel client.
 * If a call to red_channel_client_destroy is made from another location, it must be called
 * from the channel's thread.
*/

void red_channel_receive(RedChannel *channel)
{
    g_list_foreach(channel->clients, (GFunc)red_channel_client_receive, NULL);
}

static void red_channel_client_default_peer_on_error(RedChannelClient *rcc)
{
    red_channel_client_disconnect(rcc);
}

void red_channel_add_client(RedChannel *channel, RedChannelClient *rcc)
{
    spice_assert(rcc);
    channel->clients = g_list_prepend(channel->clients, rcc);
}

int red_channel_test_remote_common_cap(RedChannel *channel, uint32_t cap)
{
    GList *link, *next;
    RedChannelClient *rcc;

    FOREACH_CLIENT(channel, link, next, rcc) {
        if (!red_channel_client_test_remote_common_cap(rcc, cap)) {
            return FALSE;
        }
    }
    return TRUE;
}

int red_channel_test_remote_cap(RedChannel *channel, uint32_t cap)
{
    GList *link, *next;
    RedChannelClient *rcc;

    FOREACH_CLIENT(channel, link, next, rcc) {
        if (!red_channel_client_test_remote_cap(rcc, cap)) {
            return FALSE;
        }
    }
    return TRUE;
}

/* returns TRUE If all channels are finished migrating, FALSE otherwise */
gboolean red_client_seamless_migration_done_for_channel(RedClient *client)
{
    gboolean ret = FALSE;

    pthread_mutex_lock(&client->lock);
    client->num_migrated_channels--;
    /* we assume we always have at least one channel who has migration data transfer,
     * otherwise, this flag will never be set back to FALSE*/
    if (!client->num_migrated_channels) {
        client->during_target_migrate = FALSE;
        client->seamless_migrate = FALSE;
        /* migration completion might have been triggered from a different thread
         * than the main thread */
        main_dispatcher_seamless_migrate_dst_complete(reds_get_main_dispatcher(client->reds),
                                                      client);
        ret = TRUE;
    }
    pthread_mutex_unlock(&client->lock);

    return ret;
}

int red_channel_is_waiting_for_migrate_data(RedChannel *channel)
{
    RedChannelClient *rcc;
    guint n_clients = g_list_length(channel->clients);

    if (!red_channel_is_connected(channel)) {
        return FALSE;
    }

    if (n_clients > 1) {
        return FALSE;
    }
    spice_assert(n_clients == 1);
    rcc = g_list_nth_data(channel->clients, 0);
    return red_channel_client_is_waiting_for_migrate_data(rcc);
}

static void red_channel_client_default_connect(RedChannel *channel, RedClient *client,
                                               RedsStream *stream,
                                               int migration,
                                               int num_common_caps, uint32_t *common_caps,
                                               int num_caps, uint32_t *caps)
{
    spice_error("not implemented");
}

static void red_channel_client_default_disconnect(RedChannelClient *base)
{
    red_channel_client_disconnect(base);
}

RedChannel *red_channel_create(int size,
                               RedsState *reds,
                               const SpiceCoreInterfaceInternal *core,
                               uint32_t type, uint32_t id,
                               int handle_acks,
                               channel_handle_message_proc handle_message,
                               const ChannelCbs *channel_cbs,
                               uint32_t migration_flags)
{
    RedChannel *channel;
    ClientCbs client_cbs = { NULL, };

    spice_assert(size >= sizeof(*channel));
    spice_assert(channel_cbs->config_socket && channel_cbs->on_disconnect && handle_message &&
           channel_cbs->alloc_recv_buf);
    spice_assert(channel_cbs->handle_migrate_data ||
                 !(migration_flags & SPICE_MIGRATE_NEED_DATA_TRANSFER));
    channel = spice_malloc0(size);
    channel->type = type;
    channel->id = id;
    channel->refs = 1;
    channel->handle_acks = handle_acks;
    channel->migration_flags = migration_flags;
    channel->channel_cbs = *channel_cbs;

    channel->reds = reds;
    channel->core = core;

    // TODO: send incoming_cb as parameters instead of duplicating?
    channel->incoming_cb.alloc_msg_buf = (alloc_msg_recv_buf_proc)channel_cbs->alloc_recv_buf;
    channel->incoming_cb.release_msg_buf = (release_msg_recv_buf_proc)channel_cbs->release_recv_buf;
    channel->incoming_cb.handle_message = (handle_message_proc)handle_message;
    channel->incoming_cb.on_error =
        (on_incoming_error_proc)red_channel_client_default_peer_on_error;
    channel->incoming_cb.on_input = red_channel_client_on_input;
    channel->outgoing_cb.get_msg_size = red_channel_client_get_out_msg_size;
    channel->outgoing_cb.prepare = red_channel_client_prepare_out_msg;
    channel->outgoing_cb.on_block = red_channel_client_on_out_block;
    channel->outgoing_cb.on_error =
        (on_outgoing_error_proc)red_channel_client_default_peer_on_error;
    channel->outgoing_cb.on_msg_done = red_channel_client_on_out_msg_done;
    channel->outgoing_cb.on_output = red_channel_client_on_output;

    client_cbs.connect = red_channel_client_default_connect;
    client_cbs.disconnect = red_channel_client_default_disconnect;
    client_cbs.migrate = red_channel_client_default_migrate;

    red_channel_register_client_cbs(channel, &client_cbs, NULL);
    red_channel_set_common_cap(channel, SPICE_COMMON_CAP_MINI_HEADER);

    channel->thread_id = pthread_self();

    channel->out_bytes_counter = 0;

    spice_debug("channel type %d id %d thread_id 0x%lx",
                channel->type, channel->id, channel->thread_id);
    return channel;
}

// TODO: red_worker can use this one
static void dummy_watch_update_mask(SpiceWatch *watch, int event_mask)
{
}

static SpiceWatch *dummy_watch_add(const SpiceCoreInterfaceInternal *iface,
                                   int fd, int event_mask, SpiceWatchFunc func, void *opaque)
{
    return NULL; // apparently allowed?
}

static void dummy_watch_remove(SpiceWatch *watch)
{
}

// TODO: actually, since I also use channel_client_dummy, no need for core. Can be NULL
static const SpiceCoreInterfaceInternal dummy_core = {
    .watch_update_mask = dummy_watch_update_mask,
    .watch_add = dummy_watch_add,
    .watch_remove = dummy_watch_remove,
};

RedChannel *red_channel_create_dummy(int size, RedsState *reds, uint32_t type, uint32_t id)
{
    RedChannel *channel;
    ClientCbs client_cbs = { NULL, };

    spice_assert(size >= sizeof(*channel));
    channel = spice_malloc0(size);
    channel->type = type;
    channel->id = id;
    channel->refs = 1;
    channel->reds = reds;
    channel->core = &dummy_core;
    client_cbs.connect = red_channel_client_default_connect;
    client_cbs.disconnect = red_channel_client_default_disconnect;
    client_cbs.migrate = red_channel_client_default_migrate;

    red_channel_register_client_cbs(channel, &client_cbs, NULL);
    red_channel_set_common_cap(channel, SPICE_COMMON_CAP_MINI_HEADER);

    channel->thread_id = pthread_self();
    spice_debug("channel type %d id %d thread_id 0x%lx",
                channel->type, channel->id, channel->thread_id);

    channel->out_bytes_counter = 0;

    return channel;
}

static int do_nothing_handle_message(RedChannelClient *rcc,
                                     uint16_t type,
                                     uint32_t size,
                                     uint8_t *msg)
{
    return TRUE;
}

RedChannel *red_channel_create_parser(int size,
                                      RedsState *reds,
                                      const SpiceCoreInterfaceInternal *core,
                                      uint32_t type, uint32_t id,
                                      int handle_acks,
                                      spice_parse_channel_func_t parser,
                                      channel_handle_parsed_proc handle_parsed,
                                      const ChannelCbs *channel_cbs,
                                      uint32_t migration_flags)
{
    RedChannel *channel = red_channel_create(size, reds, core, type, id,
                                             handle_acks,
                                             do_nothing_handle_message,
                                             channel_cbs,
                                             migration_flags);
    channel->incoming_cb.handle_parsed = (handle_parsed_proc)handle_parsed;
    channel->incoming_cb.parser = parser;

    return channel;
}

void red_channel_set_stat_node(RedChannel *channel, StatNodeRef stat)
{
    spice_return_if_fail(channel != NULL);
    spice_return_if_fail(channel->stat == 0);

#ifdef RED_STATISTICS
    channel->stat = stat;
    channel->out_bytes_counter = stat_add_counter(channel->reds, stat, "out_bytes", TRUE);
#endif
}

void red_channel_register_client_cbs(RedChannel *channel, const ClientCbs *client_cbs, gpointer cbs_data)
{
    spice_assert(client_cbs->connect || channel->type == SPICE_CHANNEL_MAIN);
    channel->client_cbs.connect = client_cbs->connect;

    if (client_cbs->disconnect) {
        channel->client_cbs.disconnect = client_cbs->disconnect;
    }

    if (client_cbs->migrate) {
        channel->client_cbs.migrate = client_cbs->migrate;
    }
    channel->data = cbs_data;
}

int test_capability(const uint32_t *caps, int num_caps, uint32_t cap)
{
    uint32_t index = cap / 32;
    if (num_caps < index + 1) {
        return FALSE;
    }

    return (caps[index] & (1 << (cap % 32))) != 0;
}

static void add_capability(uint32_t **caps, int *num_caps, uint32_t cap)
{
    int nbefore, n;

    nbefore = *num_caps;
    n = cap / 32;
    *num_caps = MAX(*num_caps, n + 1);
    *caps = spice_renew(uint32_t, *caps, *num_caps);
    memset(*caps + nbefore, 0, (*num_caps - nbefore) * sizeof(uint32_t));
    (*caps)[n] |= (1 << (cap % 32));
}

void red_channel_set_common_cap(RedChannel *channel, uint32_t cap)
{
    add_capability(&channel->local_caps.common_caps, &channel->local_caps.num_common_caps, cap);
}

void red_channel_set_cap(RedChannel *channel, uint32_t cap)
{
    add_capability(&channel->local_caps.caps, &channel->local_caps.num_caps, cap);
}

void red_channel_ref(RedChannel *channel)
{
    channel->refs++;
}

void red_channel_unref(RedChannel *channel)
{
    if (--channel->refs == 0) {
        if (channel->local_caps.num_common_caps) {
            free(channel->local_caps.common_caps);
        }

        if (channel->local_caps.num_caps) {
            free(channel->local_caps.caps);
        }

        free(channel);
    }
}

void red_channel_destroy(RedChannel *channel)
{
    if (!channel) {
        return;
    }

    g_list_foreach(channel->clients, (GFunc)red_channel_client_destroy, NULL);
    red_channel_unref(channel);
}

void red_channel_send(RedChannel *channel)
{
    g_list_foreach(channel->clients, (GFunc)red_channel_client_send, NULL);
}

void red_channel_push(RedChannel *channel)
{
    if (!channel) {
        return;
    }

    g_list_foreach(channel->clients, (GFunc)red_channel_client_push, NULL);
}

// TODO: this function doesn't make sense because the window should be client (WAN/LAN)
// specific
void red_channel_init_outgoing_messages_window(RedChannel *channel)
{
    g_list_foreach(channel->clients, (GFunc)red_channel_client_init_outgoing_messages_window, NULL);
}

static void red_channel_client_pipe_add_type_proxy(gpointer data, gpointer user_data)
{
    int type = GPOINTER_TO_INT(user_data);
    red_channel_client_pipe_add_type(data, type);
}

void red_channel_pipes_add_type(RedChannel *channel, int pipe_item_type)
{
    g_list_foreach(channel->clients, red_channel_client_pipe_add_type_proxy,
                   GINT_TO_POINTER(pipe_item_type));
}

static void red_channel_client_pipe_add_empty_msg_proxy(gpointer data, gpointer user_data)
{
    int type = GPOINTER_TO_INT(user_data);
    red_channel_client_pipe_add_empty_msg(data, type);
}

void red_channel_pipes_add_empty_msg(RedChannel *channel, int msg_type)
{
    g_list_foreach(channel->clients, red_channel_client_pipe_add_empty_msg_proxy, GINT_TO_POINTER(msg_type));
}

int red_channel_is_connected(RedChannel *channel)
{
    return channel && channel->clients;
}

void red_channel_remove_client(RedChannel *channel, RedChannelClient *rcc)
{
    GList *link;
    g_return_if_fail(channel == red_channel_client_get_channel(rcc));

    if (!pthread_equal(pthread_self(), channel->thread_id)) {
        spice_warning("channel type %d id %d - "
                      "channel->thread_id (0x%lx) != pthread_self (0x%lx)."
                      "If one of the threads is != io-thread && != vcpu-thread, "
                      "this might be a BUG",
                      channel->type, channel->id,
                      channel->thread_id, pthread_self());
    }
    spice_return_if_fail(channel);
    link = g_list_find(channel->clients, rcc);
    spice_return_if_fail(link != NULL);

    channel->clients = g_list_remove_link(channel->clients, link);
    // TODO: should we set rcc->channel to NULL???
}

void red_client_remove_channel(RedChannelClient *rcc)
{
    RedClient *client = red_channel_client_get_client(rcc);
    pthread_mutex_lock(&client->lock);
    client->channels = g_list_remove(client->channels, rcc);
    pthread_mutex_unlock(&client->lock);
}

void red_channel_disconnect(RedChannel *channel)
{
    g_list_foreach(channel->clients, (GFunc)red_channel_client_disconnect, NULL);
}

void red_channel_apply_clients(RedChannel *channel, channel_client_callback cb)
{
    g_list_foreach(channel->clients, (GFunc)cb, NULL);
}

void red_channel_apply_clients_data(RedChannel *channel, channel_client_callback_data cb, void *data)
{
    g_list_foreach(channel->clients, (GFunc)cb, data);
}

int red_channel_all_blocked(RedChannel *channel)
{
    GList *link;
    RedChannelClient *rcc;

    if (!channel || !channel->clients) {
        return FALSE;
    }
    for (link = channel->clients; link != NULL; link = link->next) {
        rcc = link->data;
        if (!red_channel_client_is_blocked(rcc)) {
            return FALSE;
        }
    }
    return TRUE;
}

int red_channel_any_blocked(RedChannel *channel)
{
    GList *link, *next;
    RedChannelClient *rcc;

    FOREACH_CLIENT(channel, link, next, rcc) {
        if (red_channel_client_is_blocked(rcc)) {
            return TRUE;
        }
    }
    return FALSE;
}

int red_channel_get_first_socket(RedChannel *channel)
{
    RedChannelClient *rcc;
    RedsStream *stream;

    if (!channel || !channel->clients) {
        return -1;
    }
    rcc = g_list_nth_data(channel->clients, 0);
    stream = red_channel_client_get_stream(rcc);

    return stream->socket;
}

int red_channel_no_item_being_sent(RedChannel *channel)
{
    GList *link, *next;
    RedChannelClient *rcc;

    FOREACH_CLIENT(channel, link, next, rcc) {
        if (!red_channel_client_no_item_being_sent(rcc)) {
            return FALSE;
        }
    }
    return TRUE;
}

/*
 * RedClient implementation - kept in red-channel.c because they are
 * pretty tied together.
 */

RedClient *red_client_new(RedsState *reds, int migrated)
{
    RedClient *client;

    client = spice_malloc0(sizeof(RedClient));
    client->reds = reds;
    pthread_mutex_init(&client->lock, NULL);
    client->thread_id = pthread_self();
    client->during_target_migrate = migrated;
    client->refs = 1;

    return client;
}

RedClient *red_client_ref(RedClient *client)
{
    spice_assert(client);
    g_atomic_int_inc(&client->refs);
    return client;
}

RedClient *red_client_unref(RedClient *client)
{
    if (g_atomic_int_dec_and_test(&client->refs)) {
        spice_debug("release client=%p", client);
        pthread_mutex_destroy(&client->lock);
        free(client);
        return NULL;
    }
    return client;
}

void red_client_set_migration_seamless(RedClient *client) // dest
{
    GList *link;
    spice_assert(client->during_target_migrate);
    pthread_mutex_lock(&client->lock);
    client->seamless_migrate = TRUE;
    /* update channel clients that got connected before the migration
     * type was set. red_client_add_channel will handle newer channel clients */
    for (link = client->channels; link != NULL; link = link->next) {
        if (red_channel_client_set_migration_seamless(link->data))
            client->num_migrated_channels++;
    }
    pthread_mutex_unlock(&client->lock);
}

void red_client_migrate(RedClient *client)
{
    GList *link, *next;
    RedChannelClient *rcc;
    RedChannel *channel;

    spice_printerr("migrate client with #channels %d", g_list_length(client->channels));
    if (!pthread_equal(pthread_self(), client->thread_id)) {
        spice_warning("client->thread_id (0x%lx) != pthread_self (0x%lx)."
                      "If one of the threads is != io-thread && != vcpu-thread,"
                      " this might be a BUG",
                      client->thread_id, pthread_self());
    }
    link = client->channels;
    while (link) {
        next = link->next;
        rcc = link->data;
        channel = red_channel_client_get_channel(rcc);
        if (red_channel_client_is_connected(rcc)) {
            channel->client_cbs.migrate(rcc);
        }
        link = next;
    }
}

void red_client_destroy(RedClient *client)
{
    GList *link, *next;
    RedChannelClient *rcc;

    spice_printerr("destroy client %p with #channels=%d", client, g_list_length(client->channels));
    if (!pthread_equal(pthread_self(), client->thread_id)) {
        spice_warning("client->thread_id (0x%lx) != pthread_self (0x%lx)."
                      "If one of the threads is != io-thread && != vcpu-thread,"
                      " this might be a BUG",
                      client->thread_id,
                      pthread_self());
    }
    link = client->channels;
    while (link) {
        RedChannel *channel;
        next = link->next;
        // some channels may be in other threads, so disconnection
        // is not synchronous.
        rcc = link->data;
        channel = red_channel_client_get_channel(rcc);
        red_channel_client_set_destroying(rcc);
        // some channels may be in other threads. However we currently
        // assume disconnect is synchronous (we changed the dispatcher
        // to wait for disconnection)
        // TODO: should we go back to async. For this we need to use
        // ref count for channel clients.
        channel->client_cbs.disconnect(rcc);
        spice_assert(red_channel_client_pipe_is_empty(rcc));
        spice_assert(red_channel_client_no_item_being_sent(rcc));
        red_channel_client_destroy(rcc);
        link = next;
    }
    red_client_unref(client);
}

/* client->lock should be locked */
RedChannelClient *red_client_get_channel(RedClient *client, int type, int id)
{
    GList *link;
    RedChannelClient *rcc;
    RedChannelClient *ret = NULL;

    for (link = client->channels; link != NULL; link = link->next) {
        RedChannel *channel;
        rcc = link->data;
        channel = red_channel_client_get_channel(rcc);
        if (channel->type == type && channel->id == id) {
            ret = rcc;
            break;
        }
    }
    return ret;
}

/* client->lock should be locked */
void red_client_add_channel(RedClient *client, RedChannelClient *rcc)
{
    spice_assert(rcc && client);
    client->channels = g_list_prepend(client->channels, rcc);
    if (client->during_target_migrate && client->seamless_migrate) {
        if (red_channel_client_set_migration_seamless(rcc))
            client->num_migrated_channels++;
    }
}

MainChannelClient *red_client_get_main(RedClient *client) {
    return client->mcc;
}

void red_client_set_main(RedClient *client, MainChannelClient *mcc) {
    client->mcc = mcc;
}

void red_client_semi_seamless_migrate_complete(RedClient *client)
{
    GList *link, *next;

    pthread_mutex_lock(&client->lock);
    if (!client->during_target_migrate || client->seamless_migrate) {
        spice_error("unexpected");
        pthread_mutex_unlock(&client->lock);
        return;
    }
    client->during_target_migrate = FALSE;
    link = client->channels;
    while (link) {
        next = link->next;
        red_channel_client_semi_seamless_migration_complete(link->data);
        link = next;
    }
    pthread_mutex_unlock(&client->lock);
    reds_on_client_semi_seamless_migrate_complete(client->reds, client);
}

/* should be called only from the main thread */
int red_client_during_migrate_at_target(RedClient *client)
{
    int ret;
    pthread_mutex_lock(&client->lock);
    ret = client->during_target_migrate;
    pthread_mutex_unlock(&client->lock);
    return ret;
}

/*
 * Functions to push the same item to multiple pipes.
 */

/*
 * TODO: after convinced of correctness, add paths for single client
 * that avoid the whole loop. perhaps even have a function pointer table
 * later.
 * TODO - inline? macro? right now this is the simplest from code amount
 */

typedef void (*rcc_item_t)(RedChannelClient *rcc, RedPipeItem *item);
typedef int (*rcc_item_cond_t)(RedChannelClient *rcc, RedPipeItem *item);

/**
 * red_channel_pipes_create_batch:
 * @channel: a channel
 * @creator: a callback to create pipe item (not null)
 * @data: the data to pass to the creator
 * @pipe_add: a callback to add non-null pipe items (not null)
 *
 * Returns: the number of added items
 **/
static int red_channel_pipes_create_batch(RedChannel *channel,
                                new_pipe_item_t creator, void *data,
                                rcc_item_t pipe_add)
{
    GList *link, *next;
    RedChannelClient *rcc;
    RedPipeItem *item;
    int num = 0, n = 0;

    spice_assert(creator != NULL);
    spice_assert(pipe_add != NULL);

    FOREACH_CLIENT(channel, link, next, rcc) {
        item = (*creator)(rcc, data, num++);
        if (item) {
            (*pipe_add)(rcc, item);
            n++;
        }
    }

    return n;
}

int red_channel_pipes_new_add_push(RedChannel *channel,
                                   new_pipe_item_t creator, void *data)
{
    int n = red_channel_pipes_create_batch(channel, creator, data,
                                           red_channel_client_pipe_add);
    red_channel_push(channel);

    return n;
}

void red_channel_pipes_new_add(RedChannel *channel, new_pipe_item_t creator, void *data)
{
    red_channel_pipes_create_batch(channel, creator, data,
                                     red_channel_client_pipe_add);
}

void red_channel_pipes_new_add_tail(RedChannel *channel, new_pipe_item_t creator, void *data)
{
    red_channel_pipes_create_batch(channel, creator, data,
                                     red_channel_client_pipe_add_tail);
}

uint32_t red_channel_max_pipe_size(RedChannel *channel)
{
    GList *link;
    RedChannelClient *rcc;
    uint32_t pipe_size = 0;

    for (link = channel->clients; link != NULL; link = link->next) {
        uint32_t new_size;
        rcc = link->data;
        new_size = red_channel_client_get_pipe_size(rcc);
        pipe_size = MAX(pipe_size, new_size);
    }
    return pipe_size;
}

uint32_t red_channel_min_pipe_size(RedChannel *channel)
{
    GList *link, *next;
    RedChannelClient *rcc;
    uint32_t pipe_size = ~0;

    FOREACH_CLIENT(channel, link, next, rcc) {
        uint32_t new_size;
        new_size = red_channel_client_get_pipe_size(rcc);
        pipe_size = MIN(pipe_size, new_size);
    }
    return pipe_size == ~0 ? 0 : pipe_size;
}

uint32_t red_channel_sum_pipes_size(RedChannel *channel)
{
    GList *link, *next;
    RedChannelClient *rcc;
    uint32_t sum = 0;

    FOREACH_CLIENT(channel, link, next, rcc) {
        sum += red_channel_client_get_pipe_size(rcc);
    }
    return sum;
}

int red_channel_wait_all_sent(RedChannel *channel,
                              int64_t timeout)
{
    uint64_t end_time;
    uint32_t max_pipe_size;
    int blocked = FALSE;

    if (timeout != -1) {
        end_time = spice_get_monotonic_time_ns() + timeout;
    } else {
        end_time = UINT64_MAX;
    }

    red_channel_push(channel);
    while (((max_pipe_size = red_channel_max_pipe_size(channel)) ||
           (blocked = red_channel_any_blocked(channel))) &&
           (timeout == -1 || spice_get_monotonic_time_ns() < end_time)) {
        spice_debug("pipe-size %u blocked %d", max_pipe_size, blocked);
        usleep(CHANNEL_BLOCKED_SLEEP_DURATION);
        red_channel_receive(channel);
        red_channel_send(channel);
        red_channel_push(channel);
    }

    if (max_pipe_size || blocked) {
        spice_warning("timeout: pending out messages exist (pipe-size %u, blocked %d)",
                      max_pipe_size, blocked);
        return FALSE;
    } else {
        spice_assert(red_channel_no_item_being_sent(channel));
        return TRUE;
    }
}

RedsState* red_channel_get_server(RedChannel *channel)
{
    return channel->reds;
}
