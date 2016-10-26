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
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include "red-channel.h"
#include "red-client.h"
#include "reds.h"

#define FOREACH_CHANNEL_CLIENT(_client, _iter, _data) \
    GLIST_FOREACH((_client ? (_client)->channels : NULL), _iter, RedChannelClient, _data)

struct RedClient {
    RedsState *reds;
    GList *channels;
    MainChannelClient *mcc;
    pthread_mutex_t lock; // different channels can be in different threads

    pthread_t thread_id;

    int disconnecting;
    /* Note that while semi-seamless migration is conducted by the main thread, seamless migration
     * involves all channels, and thus the related variables can be accessed from different
     * threads */
    /* if seamless=TRUE, migration_target is turned off when all
     * the clients received their migration data. Otherwise (semi-seamless),
     * it is turned off, when red_client_semi_seamless_migrate_complete
     * is called */
    int during_target_migrate;
    int seamless_migrate;
    int num_migrated_channels; /* for seamless - number of channels that wait for migrate data*/
    int refs;
};

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

void red_client_set_migration_seamless(RedClient *client) // dest
{
    GListIter iter;
    RedChannelClient *rcc;

    spice_assert(client->during_target_migrate);
    pthread_mutex_lock(&client->lock);
    client->seamless_migrate = TRUE;
    /* update channel clients that got connected before the migration
     * type was set. red_client_add_channel will handle newer channel clients */
    FOREACH_CHANNEL_CLIENT(client, iter, rcc) {
        if (red_channel_client_set_migration_seamless(rcc)) {
            client->num_migrated_channels++;
        }
    }
    pthread_mutex_unlock(&client->lock);
}

void red_client_migrate(RedClient *client)
{
    GListIter iter;
    RedChannelClient *rcc;
    RedChannel *channel;

    spice_printerr("migrate client with #channels %d", g_list_length(client->channels));
    if (!pthread_equal(pthread_self(), client->thread_id)) {
        spice_warning("client->thread_id (0x%lx) != pthread_self (0x%lx)."
                      "If one of the threads is != io-thread && != vcpu-thread,"
                      " this might be a BUG",
                      client->thread_id, pthread_self());
    }
    FOREACH_CHANNEL_CLIENT(client, iter, rcc) {
        channel = red_channel_client_get_channel(rcc);
        if (red_channel_client_is_connected(rcc)) {
            red_channel_migrate_client(channel, rcc);
        }
    }
}

void red_client_destroy(RedClient *client)
{
    GListIter iter;
    RedChannelClient *rcc;

    spice_printerr("destroy client %p with #channels=%d", client, g_list_length(client->channels));
    if (!pthread_equal(pthread_self(), client->thread_id)) {
        spice_warning("client->thread_id (0x%lx) != pthread_self (0x%lx)."
                      "If one of the threads is != io-thread && != vcpu-thread,"
                      " this might be a BUG",
                      client->thread_id,
                      pthread_self());
    }
    FOREACH_CHANNEL_CLIENT(client, iter, rcc) {
        RedChannel *channel;
        // some channels may be in other threads, so disconnection
        // is not synchronous.
        channel = red_channel_client_get_channel(rcc);
        red_channel_client_set_destroying(rcc);
        // some channels may be in other threads. However we currently
        // assume disconnect is synchronous (we changed the dispatcher
        // to wait for disconnection)
        // TODO: should we go back to async. For this we need to use
        // ref count for channel clients.
        red_channel_disconnect_client(channel, rcc);
        spice_assert(red_channel_client_pipe_is_empty(rcc));
        spice_assert(red_channel_client_no_item_being_sent(rcc));
        red_channel_client_destroy(rcc);
    }
    red_client_unref(client);
}

/* client->lock should be locked */
RedChannelClient *red_client_get_channel(RedClient *client, int type, int id)
{
    GListIter iter;
    RedChannelClient *rcc;
    RedChannelClient *ret = NULL;

    FOREACH_CHANNEL_CLIENT(client, iter, rcc) {
        int channel_type, channel_id;
        RedChannel *channel;

        channel = red_channel_client_get_channel(rcc);
        g_object_get(channel, "channel-type", &channel_type, "id", &channel_id, NULL);
        if (channel_type == type && channel_id == id) {
            ret = rcc;
            break;
        }
    }
    return ret;
}

gboolean red_client_add_channel(RedClient *client, RedChannelClient *rcc, GError **error)
{
    uint32_t type, id;
    RedChannel *channel;
    gboolean result = TRUE;

    spice_assert(rcc && client);
    channel = red_channel_client_get_channel(rcc);

    pthread_mutex_lock(&client->lock);

    g_object_get(channel, "channel-type", &type, "id", &id, NULL);
    if (red_client_get_channel(client, type, id)) {
        g_set_error(error,
                    SPICE_SERVER_ERROR,
                    SPICE_SERVER_ERROR_FAILED,
                    "Client %p: duplicate channel type %d id %d",
                    client, type, id);
        result = FALSE;
        goto cleanup;
    }

    client->channels = g_list_prepend(client->channels, rcc);
    if (client->during_target_migrate && client->seamless_migrate) {
        if (red_channel_client_set_migration_seamless(rcc)) {
            client->num_migrated_channels++;
        }
    }

cleanup:
    pthread_mutex_unlock(&client->lock);
    return result;
}

MainChannelClient *red_client_get_main(RedClient *client) {
    return client->mcc;
}

void red_client_set_main(RedClient *client, MainChannelClient *mcc) {
    client->mcc = mcc;
}

void red_client_semi_seamless_migrate_complete(RedClient *client)
{
    GListIter iter;
    RedChannelClient *rcc;

    pthread_mutex_lock(&client->lock);
    if (!client->during_target_migrate || client->seamless_migrate) {
        spice_error("unexpected");
        pthread_mutex_unlock(&client->lock);
        return;
    }
    client->during_target_migrate = FALSE;
    FOREACH_CHANNEL_CLIENT(client, iter, rcc) {
        red_channel_client_semi_seamless_migration_complete(rcc);
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

void red_client_remove_channel(RedChannelClient *rcc)
{
    RedClient *client = red_channel_client_get_client(rcc);
    pthread_mutex_lock(&client->lock);
    client->channels = g_list_remove(client->channels, rcc);
    pthread_mutex_unlock(&client->lock);
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

gboolean red_client_is_disconnecting(RedClient *client)
{
    return client->disconnecting;
}

void red_client_set_disconnecting(RedClient *client)
{
    client->disconnecting = TRUE;
}

RedsState *red_client_get_server(RedClient *client)
{
    return client->reds;
}
