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
#include <config.h>
#endif

#include <glib.h>
#include <stdio.h>
#include <stdint.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <sys/ioctl.h>
#ifdef HAVE_LINUX_SOCKIOS_H
#include <linux/sockios.h> /* SIOCOUTQ */
#endif
#include <common/generated_server_marshallers.h>

#include "red-channel-client.h"
#include "red-channel-client-private.h"
#include "red-client.h"
#include "glib-compat.h"

static const SpiceDataHeaderOpaque full_header_wrapper;
static const SpiceDataHeaderOpaque mini_header_wrapper;
static void red_channel_client_destroy_remote_caps(RedChannelClient* rcc);
static void red_channel_client_initable_interface_init(GInitableIface *iface);

G_DEFINE_TYPE_WITH_CODE(RedChannelClient, red_channel_client, G_TYPE_OBJECT,
                        G_IMPLEMENT_INTERFACE(G_TYPE_INITABLE,
                                              red_channel_client_initable_interface_init))

#define CHANNEL_CLIENT_PRIVATE(o) \
    (G_TYPE_INSTANCE_GET_PRIVATE((o), RED_TYPE_CHANNEL_CLIENT, RedChannelClientPrivate))

static gboolean red_channel_client_initable_init(GInitable *initable,
                                                 GCancellable *cancellable,
                                                 GError **error);

enum {
    PROP0,
    PROP_STREAM,
    PROP_CHANNEL,
    PROP_CLIENT,
    PROP_MONITOR_LATENCY,
    PROP_COMMON_CAPS,
    PROP_CAPS
};

#define PING_TEST_TIMEOUT_MS (MSEC_PER_SEC * 15)
#define PING_TEST_IDLE_NET_TIMEOUT_MS (MSEC_PER_SEC / 10)

enum QosPingState {
    PING_STATE_NONE,
    PING_STATE_TIMER,
    PING_STATE_WARMUP,
    PING_STATE_LATENCY,
};

enum ConnectivityState {
    CONNECTIVITY_STATE_CONNECTED,
    CONNECTIVITY_STATE_BLOCKED,
    CONNECTIVITY_STATE_WAIT_PONG,
    CONNECTIVITY_STATE_DISCONNECTED,
};

typedef struct RedEmptyMsgPipeItem {
    RedPipeItem base;
    int msg;
} RedEmptyMsgPipeItem;

typedef struct MarkerPipeItem {
    RedPipeItem base;
    gboolean *item_in_pipe;
} MarkerPipeItem;

static void red_channel_client_start_ping_timer(RedChannelClient *rcc, uint32_t timeout)
{
    SpiceCoreInterfaceInternal *core;

    if (!rcc->priv->latency_monitor.timer) {
        return;
    }
    if (rcc->priv->latency_monitor.state != PING_STATE_NONE) {
        return;
    }
    rcc->priv->latency_monitor.state = PING_STATE_TIMER;

    core = red_channel_get_core_interface(rcc->priv->channel);
    core->timer_start(core, rcc->priv->latency_monitor.timer, timeout);
}

static void red_channel_client_cancel_ping_timer(RedChannelClient *rcc)
{
    SpiceCoreInterfaceInternal *core;

    if (!rcc->priv->latency_monitor.timer) {
        return;
    }
    if (rcc->priv->latency_monitor.state != PING_STATE_TIMER) {
        return;
    }

    core = red_channel_get_core_interface(rcc->priv->channel);
    core->timer_cancel(core, rcc->priv->latency_monitor.timer);
    rcc->priv->latency_monitor.state = PING_STATE_NONE;
}

static void red_channel_client_restart_ping_timer(RedChannelClient *rcc)
{
    uint64_t passed, timeout;

    passed = (spice_get_monotonic_time_ns() - rcc->priv->latency_monitor.last_pong_time) / NSEC_PER_MILLISEC;
    timeout = PING_TEST_IDLE_NET_TIMEOUT_MS;
    if (passed  < PING_TEST_TIMEOUT_MS) {
        timeout += PING_TEST_TIMEOUT_MS - passed;
    }

    red_channel_client_start_ping_timer(rcc, timeout);
}

static void
red_channel_client_get_property(GObject *object,
                                guint property_id,
                                GValue *value,
                                GParamSpec *pspec)
{
    RedChannelClient *self = RED_CHANNEL_CLIENT(object);

    switch (property_id)
    {
        case PROP_STREAM:
            g_value_set_pointer(value, self->priv->stream);
            break;
        case PROP_CHANNEL:
            g_value_set_object(value, self->priv->channel);
            break;
        case PROP_CLIENT:
            g_value_set_object(value, self->priv->client);
            break;
        case PROP_MONITOR_LATENCY:
            g_value_set_boolean(value, self->priv->monitor_latency);
            break;
        case PROP_COMMON_CAPS:
            {
                GArray *arr = g_array_sized_new(FALSE, FALSE,
                                                sizeof(*self->priv->remote_caps.common_caps),
                                                self->priv->remote_caps.num_common_caps);
                g_value_take_boxed(value, arr);
            }
            break;
        case PROP_CAPS:
            {
                GArray *arr = g_array_sized_new(FALSE, FALSE,
                                                sizeof(*self->priv->remote_caps.caps),
                                                self->priv->remote_caps.num_caps);
                g_value_take_boxed(value, arr);
            }
            break;
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec);
    }
}

static void
red_channel_client_set_property(GObject *object,
                                guint property_id,
                                const GValue *value,
                                GParamSpec *pspec)
{
    RedChannelClient *self = RED_CHANNEL_CLIENT(object);

    switch (property_id)
    {
        case PROP_STREAM:
            self->priv->stream = g_value_get_pointer(value);
            break;
        case PROP_CHANNEL:
            if (self->priv->channel)
                g_object_unref(self->priv->channel);
            self->priv->channel = g_value_dup_object(value);
            break;
        case PROP_CLIENT:
            self->priv->client = g_value_get_object(value);
            break;
        case PROP_MONITOR_LATENCY:
            self->priv->monitor_latency = g_value_get_boolean(value);
            break;
        case PROP_COMMON_CAPS:
            {
                GArray *caps = g_value_get_boxed(value);
                if (caps) {
                    self->priv->remote_caps.num_common_caps = caps->len;
                    free(self->priv->remote_caps.common_caps);
                    self->priv->remote_caps.common_caps =
                        spice_memdup(caps->data, caps->len * sizeof(uint32_t));
                }
            }
            break;
        case PROP_CAPS:
            {
                GArray *caps = g_value_get_boxed(value);
                if (caps) {
                    self->priv->remote_caps.num_caps = caps->len;
                    free(self->priv->remote_caps.caps);
                    self->priv->remote_caps.caps =
                        spice_memdup(caps->data, caps->len * sizeof(uint32_t));
                }
            }
            break;
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec);
    }
}

static void
red_channel_client_finalize(GObject *object)
{
    RedChannelClient *self = RED_CHANNEL_CLIENT(object);

    reds_stream_free(self->priv->stream);
    self->priv->stream = NULL;

    if (self->priv->send_data.main.marshaller) {
        spice_marshaller_destroy(self->priv->send_data.main.marshaller);
    }

    if (self->priv->send_data.urgent.marshaller) {
        spice_marshaller_destroy(self->priv->send_data.urgent.marshaller);
    }

    red_channel_client_destroy_remote_caps(self);
    if (self->priv->channel) {
        g_object_unref(self->priv->channel);
    }

    G_OBJECT_CLASS(red_channel_client_parent_class)->finalize(object);
}

static void red_channel_client_initable_interface_init(GInitableIface *iface)
{
    iface->init = red_channel_client_initable_init;
}

static gboolean red_channel_client_default_is_connected(RedChannelClient *rcc);
static void red_channel_client_default_disconnect(RedChannelClient *rcc);


static void red_channel_client_constructed(GObject *object)
{
    RedChannelClient *self =  RED_CHANNEL_CLIENT(object);

    self->incoming.opaque = self;
    self->incoming.cb = red_channel_get_incoming_handler(self->priv->channel);

    self->priv->outgoing.opaque = self;
    self->priv->outgoing.cb = red_channel_get_outgoing_handler(self->priv->channel);
    self->priv->outgoing.pos = 0;
    self->priv->outgoing.size = 0;

    if (red_channel_client_test_remote_common_cap(self, SPICE_COMMON_CAP_MINI_HEADER)) {
        self->incoming.header = mini_header_wrapper;
        self->priv->send_data.header = mini_header_wrapper;
        self->priv->is_mini_header = TRUE;
    } else {
        self->incoming.header = full_header_wrapper;
        self->priv->send_data.header = full_header_wrapper;
        self->priv->is_mini_header = FALSE;
    }
    self->incoming.header.data = self->incoming.header_buf;
}

static void red_channel_client_class_init(RedChannelClientClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS(klass);
    GParamSpec *spec;

    g_debug("%s", G_STRFUNC);
    g_type_class_add_private(klass, sizeof(RedChannelClientPrivate));

    object_class->get_property = red_channel_client_get_property;
    object_class->set_property = red_channel_client_set_property;
    object_class->finalize = red_channel_client_finalize;
    object_class->constructed = red_channel_client_constructed;

    klass->is_connected = red_channel_client_default_is_connected;
    klass->disconnect = red_channel_client_default_disconnect;

    spec = g_param_spec_pointer("stream", "stream",
                                "Associated RedStream",
                                G_PARAM_STATIC_STRINGS
                                | G_PARAM_READWRITE
                                | G_PARAM_CONSTRUCT_ONLY);
    g_object_class_install_property(object_class, PROP_STREAM, spec);

    spec = g_param_spec_object("channel", "channel",
                               "Associated RedChannel",
                               RED_TYPE_CHANNEL,
                               G_PARAM_STATIC_STRINGS
                               | G_PARAM_READWRITE
                               | G_PARAM_CONSTRUCT_ONLY);
    g_object_class_install_property(object_class, PROP_CHANNEL, spec);

    spec = g_param_spec_object("client", "client",
                               "Associated RedClient",
                               RED_TYPE_CLIENT,
                               G_PARAM_STATIC_STRINGS
                               | G_PARAM_READWRITE
                               | G_PARAM_CONSTRUCT_ONLY);
    g_object_class_install_property(object_class, PROP_CLIENT, spec);

    spec = g_param_spec_boolean("monitor-latency", "monitor-latency",
                                "Whether to monitor latency for this client",
                                FALSE,
                                G_PARAM_STATIC_STRINGS
                                | G_PARAM_READWRITE
                                | G_PARAM_CONSTRUCT_ONLY);
    g_object_class_install_property(object_class, PROP_MONITOR_LATENCY, spec);

    spec = g_param_spec_boxed("common-caps", "common-caps",
                              "Common Capabilities",
                              G_TYPE_ARRAY,
                              G_PARAM_STATIC_STRINGS
                              | G_PARAM_READWRITE
                              | G_PARAM_CONSTRUCT_ONLY);
    g_object_class_install_property(object_class, PROP_COMMON_CAPS, spec);

    spec = g_param_spec_boxed("caps", "caps",
                              "Capabilities",
                              G_TYPE_ARRAY,
                              G_PARAM_STATIC_STRINGS
                              | G_PARAM_READWRITE
                              | G_PARAM_CONSTRUCT_ONLY);
    g_object_class_install_property(object_class, PROP_CAPS, spec);
}

static void
red_channel_client_init(RedChannelClient *self)
{
    self->priv = CHANNEL_CLIENT_PRIVATE(self);
    // blocks send message (maybe use send_data.blocked + block flags)
    self->priv->ack_data.messages_window = ~0;
    self->priv->ack_data.client_generation = ~0;
    self->priv->ack_data.client_window = CLIENT_ACK_WINDOW;
    self->priv->send_data.main.marshaller = spice_marshaller_new();
    self->priv->send_data.urgent.marshaller = spice_marshaller_new();

    self->priv->send_data.marshaller = self->priv->send_data.main.marshaller;

    g_queue_init(&self->priv->pipe);
}

RedChannel* red_channel_client_get_channel(RedChannelClient *rcc)
{
    return rcc->priv->channel;
}

void red_channel_client_on_output(void *opaque, int n)
{
    RedChannelClient *rcc = opaque;

    if (rcc->priv->connectivity_monitor.timer) {
        rcc->priv->connectivity_monitor.out_bytes += n;
    }
}

void red_channel_client_on_input(void *opaque, int n)
{
    RedChannelClient *rcc = opaque;

    if (rcc->priv->connectivity_monitor.timer) {
        rcc->priv->connectivity_monitor.in_bytes += n;
    }
}

int red_channel_client_get_out_msg_size(void *opaque)
{
    RedChannelClient *rcc = RED_CHANNEL_CLIENT(opaque);

    return rcc->priv->send_data.size;
}

void red_channel_client_prepare_out_msg(void *opaque, struct iovec *vec,
                                        int *vec_size, int pos)
{
    RedChannelClient *rcc = RED_CHANNEL_CLIENT(opaque);

    *vec_size = spice_marshaller_fill_iovec(rcc->priv->send_data.marshaller,
                                            vec, IOV_MAX, pos);
}

void red_channel_client_on_out_block(void *opaque)
{
    SpiceCoreInterfaceInternal *core;
    RedChannelClient *rcc = RED_CHANNEL_CLIENT(opaque);

    rcc->priv->send_data.blocked = TRUE;
    core = red_channel_get_core_interface(rcc->priv->channel);
    core->watch_update_mask(core, rcc->priv->stream->watch,
                            SPICE_WATCH_EVENT_READ |
                            SPICE_WATCH_EVENT_WRITE);
}

static inline int red_channel_client_urgent_marshaller_is_active(RedChannelClient *rcc)
{
    return (rcc->priv->send_data.marshaller == rcc->priv->send_data.urgent.marshaller);
}

static void red_channel_client_reset_send_data(RedChannelClient *rcc)
{
    spice_marshaller_reset(rcc->priv->send_data.marshaller);
    rcc->priv->send_data.header.data = spice_marshaller_reserve_space(rcc->priv->send_data.marshaller,
                                                                      rcc->priv->send_data.header.header_size);
    spice_marshaller_set_base(rcc->priv->send_data.marshaller, rcc->priv->send_data.header.header_size);
    rcc->priv->send_data.header.set_msg_type(&rcc->priv->send_data.header, 0);
    rcc->priv->send_data.header.set_msg_size(&rcc->priv->send_data.header, 0);

    if (!rcc->priv->is_mini_header) {
        spice_assert(rcc->priv->send_data.marshaller != rcc->priv->send_data.urgent.marshaller);
        rcc->priv->send_data.header.set_msg_sub_list(&rcc->priv->send_data.header, 0);
    }
}

static void red_channel_client_send_set_ack(RedChannelClient *rcc)
{
    SpiceMsgSetAck ack;

    spice_assert(rcc);
    red_channel_client_init_send_data(rcc, SPICE_MSG_SET_ACK, NULL);
    ack.generation = ++rcc->priv->ack_data.generation;
    ack.window = rcc->priv->ack_data.client_window;
    rcc->priv->ack_data.messages_window = 0;

    spice_marshall_msg_set_ack(rcc->priv->send_data.marshaller, &ack);

    red_channel_client_begin_send_message(rcc);
}

static void red_channel_client_send_migrate(RedChannelClient *rcc)
{
    SpiceMsgMigrate migrate;

    red_channel_client_init_send_data(rcc, SPICE_MSG_MIGRATE, NULL);
    g_object_get(rcc->priv->channel, "migration-flags", &migrate.flags, NULL);
    spice_marshall_msg_migrate(rcc->priv->send_data.marshaller, &migrate);
    if (migrate.flags & SPICE_MIGRATE_NEED_FLUSH) {
        rcc->priv->wait_migrate_flush_mark = TRUE;
    }

    red_channel_client_begin_send_message(rcc);
}

static void red_channel_client_send_ping(RedChannelClient *rcc)
{
    SpiceMsgPing ping;

    if (!rcc->priv->latency_monitor.warmup_was_sent) { // latency test start
        int delay_val;
        socklen_t opt_size = sizeof(delay_val);

        rcc->priv->latency_monitor.warmup_was_sent = TRUE;
        /*
         * When testing latency, TCP_NODELAY must be switched on, otherwise,
         * sending the ping message is delayed by Nagle algorithm, and the
         * roundtrip measurement is less accurate (bigger).
         */
        rcc->priv->latency_monitor.tcp_nodelay = 1;
        if (getsockopt(rcc->priv->stream->socket, IPPROTO_TCP, TCP_NODELAY, &delay_val,
                       &opt_size) == -1) {
            spice_warning("getsockopt failed, %s", strerror(errno));
        }  else {
            rcc->priv->latency_monitor.tcp_nodelay = delay_val;
            if (!delay_val) {
                delay_val = 1;
                if (setsockopt(rcc->priv->stream->socket, IPPROTO_TCP, TCP_NODELAY, &delay_val,
                               sizeof(delay_val)) == -1) {
                   if (errno != ENOTSUP) {
                        spice_warning("setsockopt failed, %s", strerror(errno));
                    }
                }
            }
        }
    }

    red_channel_client_init_send_data(rcc, SPICE_MSG_PING, NULL);
    ping.id = rcc->priv->latency_monitor.id;
    ping.timestamp = spice_get_monotonic_time_ns();
    spice_marshall_msg_ping(rcc->priv->send_data.marshaller, &ping);
    red_channel_client_begin_send_message(rcc);
}

static void red_channel_client_send_empty_msg(RedChannelClient *rcc, RedPipeItem *base)
{
    RedEmptyMsgPipeItem *msg_pipe_item = SPICE_UPCAST(RedEmptyMsgPipeItem, base);

    red_channel_client_init_send_data(rcc, msg_pipe_item->msg, NULL);
    red_channel_client_begin_send_message(rcc);
}

static void red_channel_client_send_item(RedChannelClient *rcc, RedPipeItem *item)
{
    spice_assert(red_channel_client_no_item_being_sent(rcc));
    red_channel_client_reset_send_data(rcc);
    switch (item->type) {
        case RED_PIPE_ITEM_TYPE_SET_ACK:
            red_channel_client_send_set_ack(rcc);
            break;
        case RED_PIPE_ITEM_TYPE_MIGRATE:
            red_channel_client_send_migrate(rcc);
            break;
        case RED_PIPE_ITEM_TYPE_EMPTY_MSG:
            red_channel_client_send_empty_msg(rcc, item);
            break;
        case RED_PIPE_ITEM_TYPE_PING:
            red_channel_client_send_ping(rcc);
            break;
        case RED_PIPE_ITEM_TYPE_MARKER:
            break;
        default:
            red_channel_send_item(rcc->priv->channel, rcc, item);
            break;
    }
    red_pipe_item_unref(item);
}

static inline void red_channel_client_release_sent_item(RedChannelClient *rcc)
{
    if (rcc->priv->send_data.item) {
        red_pipe_item_unref(rcc->priv->send_data.item);
        rcc->priv->send_data.item = NULL;
    }
}

static void red_channel_client_restore_main_sender(RedChannelClient *rcc)
{
    spice_marshaller_reset(rcc->priv->send_data.urgent.marshaller);
    rcc->priv->send_data.marshaller = rcc->priv->send_data.main.marshaller;
    rcc->priv->send_data.header.data = rcc->priv->send_data.main.header_data;
    rcc->priv->send_data.item = rcc->priv->send_data.main.item;
}

void red_channel_client_on_out_msg_done(void *opaque)
{
    RedChannelClient *rcc = RED_CHANNEL_CLIENT(opaque);
    int fd;

    rcc->priv->send_data.size = 0;

    if (spice_marshaller_get_fd(rcc->priv->send_data.marshaller, &fd)) {
        if (reds_stream_send_msgfd(rcc->priv->stream, fd) < 0) {
            perror("sendfd");
            red_channel_client_disconnect(rcc);
            if (fd != -1)
                close(fd);
            return;
        }
        if (fd != -1)
            close(fd);
    }

    red_channel_client_release_sent_item(rcc);
    if (rcc->priv->send_data.blocked) {
        SpiceCoreInterfaceInternal *core = red_channel_get_core_interface(rcc->priv->channel);
        rcc->priv->send_data.blocked = FALSE;
        core->watch_update_mask(core, rcc->priv->stream->watch,
                                SPICE_WATCH_EVENT_READ);
    }

    if (red_channel_client_urgent_marshaller_is_active(rcc)) {
        red_channel_client_restore_main_sender(rcc);
        spice_assert(rcc->priv->send_data.header.data != NULL);
        red_channel_client_begin_send_message(rcc);
    } else {
        if (rcc->priv->latency_monitor.timer
            && !rcc->priv->send_data.blocked
            && g_queue_is_empty(&rcc->priv->pipe)) {
            /* It is possible that the socket will become idle, so we may be able to test latency */
            red_channel_client_restart_ping_timer(rcc);
        }
    }

}

static gboolean red_channel_client_pipe_remove(RedChannelClient *rcc, RedPipeItem *item)
{
    return g_queue_remove(&rcc->priv->pipe, item);
}

static void red_channel_client_destroy_remote_caps(RedChannelClient* rcc)
{
    rcc->priv->remote_caps.num_common_caps = 0;
    free(rcc->priv->remote_caps.common_caps);
    rcc->priv->remote_caps.num_caps = 0;
    free(rcc->priv->remote_caps.caps);
}

int red_channel_client_test_remote_common_cap(RedChannelClient *rcc, uint32_t cap)
{
    return test_capability(rcc->priv->remote_caps.common_caps,
                           rcc->priv->remote_caps.num_common_caps,
                           cap);
}

int red_channel_client_test_remote_cap(RedChannelClient *rcc, uint32_t cap)
{
    return test_capability(rcc->priv->remote_caps.caps,
                           rcc->priv->remote_caps.num_caps,
                           cap);
}

static void red_channel_client_push_ping(RedChannelClient *rcc)
{
    spice_assert(rcc->priv->latency_monitor.state == PING_STATE_NONE);
    rcc->priv->latency_monitor.state = PING_STATE_WARMUP;
    rcc->priv->latency_monitor.warmup_was_sent = FALSE;
    rcc->priv->latency_monitor.id = rand();
    red_channel_client_pipe_add_type(rcc, RED_PIPE_ITEM_TYPE_PING);
    red_channel_client_pipe_add_type(rcc, RED_PIPE_ITEM_TYPE_PING);
}

static void red_channel_client_ping_timer(void *opaque)
{
    RedChannelClient *rcc = opaque;

    spice_assert(rcc->priv->latency_monitor.state == PING_STATE_TIMER);
    red_channel_client_cancel_ping_timer(rcc);

#ifdef HAVE_LINUX_SOCKIOS_H /* SIOCOUTQ is a Linux only ioctl on sockets. */
    {
        int so_unsent_size = 0;

        /* retrieving the occupied size of the socket's tcp snd buffer (unacked + unsent) */
        if (ioctl(rcc->priv->stream->socket, SIOCOUTQ, &so_unsent_size) == -1) {
            spice_printerr("ioctl(SIOCOUTQ) failed, %s", strerror(errno));
        }
        if (so_unsent_size > 0) {
            /* tcp snd buffer is still occupied. rescheduling ping */
            red_channel_client_start_ping_timer(rcc, PING_TEST_IDLE_NET_TIMEOUT_MS);
        } else {
            red_channel_client_push_ping(rcc);
        }
    }
#else /* ifdef HAVE_LINUX_SOCKIOS_H */
    /* More portable alternative code path (less accurate but avoids bogus ioctls)*/
    red_channel_client_push_ping(rcc);
#endif /* ifdef HAVE_LINUX_SOCKIOS_H */
}

static inline int red_channel_client_waiting_for_ack(RedChannelClient *rcc)
{
    gboolean handle_acks;
    g_object_get(rcc->priv->channel,
                 "handle-acks", &handle_acks,
                 NULL);

    return (handle_acks && (rcc->priv->ack_data.messages_window >
                            rcc->priv->ack_data.client_window * 2));
}

/*
 * When a connection is not alive (and we can't detect it via a socket error), we
 * reach one of these 2 states:
 * (1) Sending msgs is blocked: either writes return EAGAIN
 *     or we are missing MSGC_ACK from the client.
 * (2) MSG_PING was sent without receiving a MSGC_PONG in reply.
 *
 * The connectivity_timer callback tests if the channel's state matches one of the above.
 * In case it does, on the next time the timer is called, it checks if the connection has
 * been idle during the time that passed since the previous timer call. If the connection
 * has been idle, we consider the client as disconnected.
 */
static void red_channel_client_connectivity_timer(void *opaque)
{
    RedChannelClient *rcc = opaque;
    RedChannelClientConnectivityMonitor *monitor = &rcc->priv->connectivity_monitor;
    int is_alive = TRUE;

    if (monitor->state == CONNECTIVITY_STATE_BLOCKED) {
        if (monitor->in_bytes == 0 && monitor->out_bytes == 0) {
            if (!rcc->priv->send_data.blocked && !red_channel_client_waiting_for_ack(rcc)) {
                spice_error("mismatch between rcc-state and connectivity-state");
            }
            spice_debug("rcc is blocked; connection is idle");
            is_alive = FALSE;
        }
    } else if (monitor->state == CONNECTIVITY_STATE_WAIT_PONG) {
        if (monitor->in_bytes == 0) {
            if (rcc->priv->latency_monitor.state != PING_STATE_WARMUP &&
                rcc->priv->latency_monitor.state != PING_STATE_LATENCY) {
                spice_error("mismatch between rcc-state and connectivity-state");
            }
            spice_debug("rcc waits for pong; connection is idle");
            is_alive = FALSE;
        }
    }

    if (is_alive) {
        SpiceCoreInterfaceInternal *core = red_channel_get_core_interface(rcc->priv->channel);
        monitor->in_bytes = 0;
        monitor->out_bytes = 0;
        if (rcc->priv->send_data.blocked || red_channel_client_waiting_for_ack(rcc)) {
            monitor->state = CONNECTIVITY_STATE_BLOCKED;
        } else if (rcc->priv->latency_monitor.state == PING_STATE_WARMUP ||
                   rcc->priv->latency_monitor.state == PING_STATE_LATENCY) {
            monitor->state = CONNECTIVITY_STATE_WAIT_PONG;
        } else {
             monitor->state = CONNECTIVITY_STATE_CONNECTED;
        }
        core->timer_start(core, rcc->priv->connectivity_monitor.timer,
                          rcc->priv->connectivity_monitor.timeout);
    } else {
        uint32_t type, id;
        g_object_get(rcc->priv->channel,
                     "channel-type", &type,
                     "id", &id,
                     NULL);
        monitor->state = CONNECTIVITY_STATE_DISCONNECTED;
        spice_warning("rcc %p on channel %d:%d has been unresponsive for more than %u ms, disconnecting",
                      rcc, type, id, monitor->timeout);
        red_channel_client_disconnect(rcc);
    }
}

void red_channel_client_start_connectivity_monitoring(RedChannelClient *rcc, uint32_t timeout_ms)
{
    SpiceCoreInterfaceInternal *core = red_channel_get_core_interface(rcc->priv->channel);
    if (!red_channel_client_is_connected(rcc)) {
        return;
    }
    spice_debug(NULL);
    spice_assert(timeout_ms > 0);
    /*
     * If latency_monitor is not active, we activate it in order to enable
     * periodic ping messages so that we will be be able to identify a disconnected
     * channel-client even if there are no ongoing channel specific messages
     * on this channel.
     */
    if (rcc->priv->latency_monitor.timer == NULL) {
        rcc->priv->latency_monitor.timer = core->timer_add(
            core, red_channel_client_ping_timer, rcc);
        if (!red_client_during_migrate_at_target(rcc->priv->client)) {
            red_channel_client_start_ping_timer(rcc, PING_TEST_IDLE_NET_TIMEOUT_MS);
        }
        rcc->priv->latency_monitor.roundtrip = -1;
    }
    if (rcc->priv->connectivity_monitor.timer == NULL) {
        rcc->priv->connectivity_monitor.state = CONNECTIVITY_STATE_CONNECTED;
        rcc->priv->connectivity_monitor.timer = core->timer_add(
            core, red_channel_client_connectivity_timer, rcc);
        rcc->priv->connectivity_monitor.timeout = timeout_ms;
        if (!red_client_during_migrate_at_target(rcc->priv->client)) {
            core->timer_start(core, rcc->priv->connectivity_monitor.timer,
                              rcc->priv->connectivity_monitor.timeout);
        }
    }
}

static void red_channel_client_event(int fd, int event, void *data)
{
    RedChannelClient *rcc = RED_CHANNEL_CLIENT(data);

    g_object_ref(rcc);
    if (event & SPICE_WATCH_EVENT_READ) {
        red_channel_client_receive(rcc);
    }
    if (event & SPICE_WATCH_EVENT_WRITE) {
        red_channel_client_push(rcc);
    }
    g_object_unref(rcc);
}

static uint32_t full_header_get_msg_size(SpiceDataHeaderOpaque *header)
{
    return GUINT32_FROM_LE(((SpiceDataHeader *)header->data)->size);
}

static uint32_t mini_header_get_msg_size(SpiceDataHeaderOpaque *header)
{
    return GUINT32_FROM_LE(((SpiceMiniDataHeader *)header->data)->size);
}

static uint16_t full_header_get_msg_type(SpiceDataHeaderOpaque *header)
{
    return GUINT16_FROM_LE(((SpiceDataHeader *)header->data)->type);
}

static uint16_t mini_header_get_msg_type(SpiceDataHeaderOpaque *header)
{
    return GUINT16_FROM_LE(((SpiceMiniDataHeader *)header->data)->type);
}

static void full_header_set_msg_type(SpiceDataHeaderOpaque *header, uint16_t type)
{
    ((SpiceDataHeader *)header->data)->type = GUINT16_TO_LE(type);
}

static void mini_header_set_msg_type(SpiceDataHeaderOpaque *header, uint16_t type)
{
    ((SpiceMiniDataHeader *)header->data)->type = GUINT16_TO_LE(type);
}

static void full_header_set_msg_size(SpiceDataHeaderOpaque *header, uint32_t size)
{
    ((SpiceDataHeader *)header->data)->size = GUINT32_TO_LE(size);
}

static void mini_header_set_msg_size(SpiceDataHeaderOpaque *header, uint32_t size)
{
    ((SpiceMiniDataHeader *)header->data)->size = GUINT32_TO_LE(size);
}

static void full_header_set_msg_serial(SpiceDataHeaderOpaque *header, uint64_t serial)
{
    ((SpiceDataHeader *)header->data)->serial = GUINT64_TO_LE(serial);
}

static void mini_header_set_msg_serial(SpiceDataHeaderOpaque *header, uint64_t serial)
{
    /* ignore serial, not supported by mini header */
}

static void full_header_set_msg_sub_list(SpiceDataHeaderOpaque *header, uint32_t sub_list)
{
    ((SpiceDataHeader *)header->data)->sub_list = GUINT32_TO_LE(sub_list);
}

static void mini_header_set_msg_sub_list(SpiceDataHeaderOpaque *header, uint32_t sub_list)
{
    spice_error("attempt to set header sub list on mini header");
}

static const SpiceDataHeaderOpaque full_header_wrapper = {NULL, sizeof(SpiceDataHeader),
                                                          full_header_set_msg_type,
                                                          full_header_set_msg_size,
                                                          full_header_set_msg_serial,
                                                          full_header_set_msg_sub_list,
                                                          full_header_get_msg_type,
                                                          full_header_get_msg_size};

static const SpiceDataHeaderOpaque mini_header_wrapper = {NULL, sizeof(SpiceMiniDataHeader),
                                                          mini_header_set_msg_type,
                                                          mini_header_set_msg_size,
                                                          mini_header_set_msg_serial,
                                                          mini_header_set_msg_sub_list,
                                                          mini_header_get_msg_type,
                                                          mini_header_get_msg_size};

static gboolean red_channel_client_initable_init(GInitable *initable,
                                                 GCancellable *cancellable,
                                                 GError **error)
{
    GError *local_error = NULL;
    SpiceCoreInterfaceInternal *core;
    RedChannelClient *self = RED_CHANNEL_CLIENT(initable);

    if (!red_channel_config_socket(self->priv->channel, self)) {
        g_set_error_literal(&local_error,
                            SPICE_SERVER_ERROR,
                            SPICE_SERVER_ERROR_FAILED,
                            "Unable to configure socket");
        goto cleanup;
    }

    core = red_channel_get_core_interface(self->priv->channel);
    if (self->priv->stream)
        self->priv->stream->watch =
            core->watch_add(core, self->priv->stream->socket,
                            SPICE_WATCH_EVENT_READ,
                            red_channel_client_event,
                            self);

    if (self->priv->monitor_latency
        && reds_stream_get_family(self->priv->stream) != AF_UNIX) {
        self->priv->latency_monitor.timer =
            core->timer_add(core, red_channel_client_ping_timer, self);

        if (!red_client_during_migrate_at_target(self->priv->client)) {
            red_channel_client_start_ping_timer(self,
                                                PING_TEST_IDLE_NET_TIMEOUT_MS);
        }
        self->priv->latency_monitor.roundtrip = -1;
    }

    red_channel_add_client(self->priv->channel, self);
    if (!red_client_add_channel(self->priv->client, self, &local_error)) {
        red_channel_remove_client(self->priv->channel, self);
    }

cleanup:
    if (local_error) {
        g_warning("Failed to create channel client: %s", local_error->message);
        g_propagate_error(error, local_error);
    }
    return local_error == NULL;
}

RedChannelClient *red_channel_client_create(RedChannel *channel, RedClient *client,
                                            RedsStream *stream,
                                            int monitor_latency,
                                            int num_common_caps, uint32_t *common_caps,
                                            int num_caps, uint32_t *caps)
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
    rcc = g_initable_new(RED_TYPE_CHANNEL_CLIENT,
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

static void red_channel_client_seamless_migration_done(RedChannelClient *rcc)
{
    rcc->priv->wait_migrate_data = FALSE;

    if (red_client_seamless_migration_done_for_channel(rcc->priv->client)) {
        if (rcc->priv->latency_monitor.timer) {
            red_channel_client_start_ping_timer(rcc, PING_TEST_IDLE_NET_TIMEOUT_MS);
        }
        if (rcc->priv->connectivity_monitor.timer) {
            SpiceCoreInterfaceInternal *core = red_channel_get_core_interface(rcc->priv->channel);
            core->timer_start(core, rcc->priv->connectivity_monitor.timer,
                              rcc->priv->connectivity_monitor.timeout);
        }
    }
}

void red_channel_client_semi_seamless_migration_complete(RedChannelClient *rcc)
{
    if (rcc->priv->latency_monitor.timer) {
        red_channel_client_start_ping_timer(rcc, PING_TEST_IDLE_NET_TIMEOUT_MS);
    }
}

int red_channel_client_is_waiting_for_migrate_data(RedChannelClient *rcc)
{
    return rcc->priv->wait_migrate_data;
}

void red_channel_client_default_migrate(RedChannelClient *rcc)
{
    SpiceCoreInterfaceInternal *core = red_channel_get_core_interface(rcc->priv->channel);
    if (rcc->priv->latency_monitor.timer) {
        red_channel_client_cancel_ping_timer(rcc);
        core->timer_remove(core, rcc->priv->latency_monitor.timer);
        rcc->priv->latency_monitor.timer = NULL;
    }
    if (rcc->priv->connectivity_monitor.timer) {
        core->timer_remove(core, rcc->priv->connectivity_monitor.timer);
        rcc->priv->connectivity_monitor.timer = NULL;
    }
    red_channel_client_pipe_add_type(rcc, RED_PIPE_ITEM_TYPE_MIGRATE);
}

void red_channel_client_destroy(RedChannelClient *rcc)
{
    rcc->priv->destroying = TRUE;
    red_channel_client_disconnect(rcc);
    red_client_remove_channel(rcc);
    g_object_unref(rcc);
}

void red_channel_client_shutdown(RedChannelClient *rcc)
{
    if (rcc->priv->stream && !rcc->priv->stream->shutdown) {
        SpiceCoreInterfaceInternal *core = red_channel_get_core_interface(rcc->priv->channel);
        core->watch_remove(core, rcc->priv->stream->watch);
        rcc->priv->stream->watch = NULL;
        shutdown(rcc->priv->stream->socket, SHUT_RDWR);
        rcc->priv->stream->shutdown = TRUE;
    }
}

static void red_peer_handle_outgoing(RedsStream *stream, OutgoingHandler *handler)
{
    ssize_t n;

    if (!stream) {
        return;
    }

    if (handler->size == 0) {
        handler->vec = handler->vec_buf;
        handler->size = handler->cb->get_msg_size(handler->opaque);
        if (!handler->size) {  // nothing to be sent
            return;
        }
    }

    for (;;) {
        handler->cb->prepare(handler->opaque, handler->vec, &handler->vec_size, handler->pos);
        n = reds_stream_writev(stream, handler->vec, handler->vec_size);
        if (n == -1) {
            switch (errno) {
            case EAGAIN:
                handler->cb->on_block(handler->opaque);
                return;
            case EINTR:
                continue;
            case EPIPE:
                handler->cb->on_error(handler->opaque);
                return;
            default:
                spice_printerr("%s", strerror(errno));
                handler->cb->on_error(handler->opaque);
                return;
            }
        } else {
            handler->pos += n;
            handler->cb->on_output(handler->opaque, n);
            if (handler->pos == handler->size) { // finished writing data
                /* reset handler before calling on_msg_done, since it
                 * can trigger another call to red_peer_handle_outgoing (when
                 * switching from the urgent marshaller to the main one */
                handler->vec = handler->vec_buf;
                handler->pos = 0;
                handler->size = 0;
                handler->cb->on_msg_done(handler->opaque);
                return;
            }
        }
    }
}

/* return the number of bytes read. -1 in case of error */
static int red_peer_receive(RedsStream *stream, uint8_t *buf, uint32_t size)
{
    uint8_t *pos = buf;
    while (size) {
        int now;
        if (stream->shutdown) {
            return -1;
        }
        now = reds_stream_read(stream, pos, size);
        if (now <= 0) {
            if (now == 0) {
                return -1;
            }
            spice_assert(now == -1);
            if (errno == EAGAIN) {
                break;
            } else if (errno == EINTR) {
                continue;
            } else if (errno == EPIPE) {
                return -1;
            } else {
                spice_printerr("%s", strerror(errno));
                return -1;
            }
        } else {
            size -= now;
            pos += now;
        }
    }
    return pos - buf;
}

// TODO: this implementation, as opposed to the old implementation in red_worker,
// does many calls to red_peer_receive and through it cb_read, and thus avoids pointer
// arithmetic for the case where a single cb_read could return multiple messages. But
// this is suboptimal potentially. Profile and consider fixing.
static void red_peer_handle_incoming(RedsStream *stream, IncomingHandler *handler)
{
    int bytes_read;
    uint8_t *parsed;
    size_t parsed_size;
    message_destructor_t parsed_free;
    uint16_t msg_type;
    uint32_t msg_size;

    /* XXX: This needs further investigation as to the underlying cause, it happened
     * after spicec disconnect (but not with spice-gtk) repeatedly. */
    if (!stream) {
        return;
    }

    for (;;) {
        int ret_handle;
        if (handler->header_pos < handler->header.header_size) {
            bytes_read = red_peer_receive(stream,
                                          handler->header.data + handler->header_pos,
                                          handler->header.header_size - handler->header_pos);
            if (bytes_read == -1) {
                handler->cb->on_error(handler->opaque);
                return;
            }
            handler->cb->on_input(handler->opaque, bytes_read);
            handler->header_pos += bytes_read;

            if (handler->header_pos != handler->header.header_size) {
                return;
            }
        }

        msg_size = handler->header.get_msg_size(&handler->header);
        msg_type = handler->header.get_msg_type(&handler->header);
        if (handler->msg_pos < msg_size) {
            if (!handler->msg) {
                handler->msg = handler->cb->alloc_msg_buf(handler->opaque, msg_type, msg_size);
                if (handler->msg == NULL) {
                    spice_printerr("ERROR: channel refused to allocate buffer.");
                    handler->cb->on_error(handler->opaque);
                    return;
                }
            }

            bytes_read = red_peer_receive(stream,
                                          handler->msg + handler->msg_pos,
                                          msg_size - handler->msg_pos);
            if (bytes_read == -1) {
                handler->cb->release_msg_buf(handler->opaque, msg_type, msg_size, handler->msg);
                handler->cb->on_error(handler->opaque);
                return;
            }
            handler->cb->on_input(handler->opaque, bytes_read);
            handler->msg_pos += bytes_read;
            if (handler->msg_pos != msg_size) {
                return;
            }
        }

        if (handler->cb->parser) {
            parsed = handler->cb->parser(handler->msg,
                handler->msg + msg_size, msg_type,
                SPICE_VERSION_MINOR, &parsed_size, &parsed_free);
            if (parsed == NULL) {
                spice_printerr("failed to parse message type %d", msg_type);
                handler->cb->release_msg_buf(handler->opaque, msg_type, msg_size, handler->msg);
                handler->cb->on_error(handler->opaque);
                return;
            }
            ret_handle = handler->cb->handle_parsed(handler->opaque, parsed_size,
                                                    msg_type, parsed);
            parsed_free(parsed);
        } else {
            ret_handle = handler->cb->handle_message(handler->opaque, msg_type, msg_size,
                                                     handler->msg);
        }
        handler->msg_pos = 0;
        handler->cb->release_msg_buf(handler->opaque, msg_type, msg_size, handler->msg);
        handler->msg = NULL;
        handler->header_pos = 0;

        if (!ret_handle) {
            handler->cb->on_error(handler->opaque);
            return;
        }
    }
}

void red_channel_client_receive(RedChannelClient *rcc)
{
    g_object_ref(rcc);
    red_peer_handle_incoming(rcc->priv->stream, &rcc->incoming);
    g_object_unref(rcc);
}

void red_channel_client_send(RedChannelClient *rcc)
{
    g_object_ref(rcc);
    red_peer_handle_outgoing(rcc->priv->stream, &rcc->priv->outgoing);
    g_object_unref(rcc);
}

static inline RedPipeItem *red_channel_client_pipe_item_get(RedChannelClient *rcc)
{
    if (!rcc || rcc->priv->send_data.blocked
             || red_channel_client_waiting_for_ack(rcc)) {
        return NULL;
    }
    return g_queue_pop_tail(&rcc->priv->pipe);
}

void red_channel_client_push(RedChannelClient *rcc)
{
    RedPipeItem *pipe_item;

    if (!rcc->priv->during_send) {
        rcc->priv->during_send = TRUE;
    } else {
        return;
    }
    g_object_ref(rcc);
    if (rcc->priv->send_data.blocked) {
        red_channel_client_send(rcc);
    }

    if (!red_channel_client_no_item_being_sent(rcc) && !rcc->priv->send_data.blocked) {
        rcc->priv->send_data.blocked = TRUE;
        spice_printerr("ERROR: an item waiting to be sent and not blocked");
    }

    while ((pipe_item = red_channel_client_pipe_item_get(rcc))) {
        red_channel_client_send_item(rcc, pipe_item);
    }
    if (red_channel_client_no_item_being_sent(rcc) && g_queue_is_empty(&rcc->priv->pipe)
        && rcc->priv->stream->watch) {
        SpiceCoreInterfaceInternal *core;
        core = red_channel_get_core_interface(rcc->priv->channel);
        core->watch_update_mask(core, rcc->priv->stream->watch,
                                SPICE_WATCH_EVENT_READ);
    }
    rcc->priv->during_send = FALSE;
    g_object_unref(rcc);
}

int red_channel_client_get_roundtrip_ms(RedChannelClient *rcc)
{
    if (rcc->priv->latency_monitor.roundtrip < 0) {
        return rcc->priv->latency_monitor.roundtrip;
    }
    return rcc->priv->latency_monitor.roundtrip / NSEC_PER_MILLISEC;
}

void red_channel_client_init_outgoing_messages_window(RedChannelClient *rcc)
{
    rcc->priv->ack_data.messages_window = 0;
    red_channel_client_push(rcc);
}

static void red_channel_client_handle_pong(RedChannelClient *rcc, SpiceMsgPing *ping)
{
    uint64_t now;

    /* ignoring unexpected pongs, or post-migration pongs for pings that
     * started just before migration */
    if (ping->id != rcc->priv->latency_monitor.id) {
        spice_warning("ping-id (%u)!= pong-id %u",
                      rcc->priv->latency_monitor.id, ping->id);
        return;
    }

    now = spice_get_monotonic_time_ns();

    if (rcc->priv->latency_monitor.state == PING_STATE_WARMUP) {
        rcc->priv->latency_monitor.state = PING_STATE_LATENCY;
        return;
    } else if (rcc->priv->latency_monitor.state != PING_STATE_LATENCY) {
        spice_warning("unexpected");
        return;
    }

    /* set TCP_NODELAY=0, in case we reverted it for the test*/
    if (!rcc->priv->latency_monitor.tcp_nodelay) {
        int delay_val = 0;

        if (setsockopt(rcc->priv->stream->socket, IPPROTO_TCP, TCP_NODELAY, &delay_val,
                       sizeof(delay_val)) == -1) {
            if (errno != ENOTSUP) {
                spice_warning("setsockopt failed, %s", strerror(errno));
            }
        }
    }

    /*
     * The real network latency shouldn't change during the connection. However,
     *  the measurements can be bigger than the real roundtrip due to other
     *  threads or processes that are utilizing the network. We update the roundtrip
     *  measurement with the minimal value we encountered till now.
     */
    if (rcc->priv->latency_monitor.roundtrip < 0 ||
        now - ping->timestamp < rcc->priv->latency_monitor.roundtrip) {
        rcc->priv->latency_monitor.roundtrip = now - ping->timestamp;
        spice_debug("update roundtrip %.2f(ms)", ((double)rcc->priv->latency_monitor.roundtrip)/NSEC_PER_MILLISEC);
    }

    rcc->priv->latency_monitor.last_pong_time = now;
    rcc->priv->latency_monitor.state = PING_STATE_NONE;
    red_channel_client_start_ping_timer(rcc, PING_TEST_TIMEOUT_MS);
}

static void red_channel_client_handle_migrate_flush_mark(RedChannelClient *rcc)
{
    RedChannel *channel = red_channel_client_get_channel(rcc);
    RedChannelClass *klass = RED_CHANNEL_GET_CLASS(channel);
    if (klass->handle_migrate_flush_mark) {
        klass->handle_migrate_flush_mark(rcc);
    }
}

// TODO: the whole migration is broken with multiple clients. What do we want to do?
// basically just
//  1) source send mark to all
//  2) source gets at various times the data (waits for all)
//  3) source migrates to target
//  4) target sends data to all
// So need to make all the handlers work with per channel/client data (what data exactly?)
static void red_channel_client_handle_migrate_data(RedChannelClient *rcc,
                                                   uint32_t size,
                                                   void *message)
{
    RedChannel *channel = red_channel_client_get_channel(rcc);
    RedChannelClass *klass = RED_CHANNEL_GET_CLASS(channel);
    uint32_t type, id;

    g_object_get(channel, "channel-type", &type, "id", &id, NULL);
    spice_debug("channel type %d id %d rcc %p size %u",
                type, id, rcc, size);
    if (!klass->handle_migrate_data) {
        return;
    }
    if (!red_channel_client_is_waiting_for_migrate_data(rcc)) {
        spice_channel_client_error(rcc, "unexpected");
        return;
    }
    if (klass->handle_migrate_data_get_serial) {
        red_channel_client_set_message_serial(rcc,
            klass->handle_migrate_data_get_serial(rcc, size, message));
    }
    if (!klass->handle_migrate_data(rcc, size, message)) {
        spice_channel_client_error(rcc, "handle_migrate_data failed");
        return;
    }
    red_channel_client_seamless_migration_done(rcc);
}


int red_channel_client_handle_message(RedChannelClient *rcc, uint32_t size,
                                      uint16_t type, void *message)
{
    switch (type) {
    case SPICE_MSGC_ACK_SYNC:
        if (size != sizeof(uint32_t)) {
            spice_printerr("bad message size");
            return FALSE;
        }
        rcc->priv->ack_data.client_generation = *(uint32_t *)(message);
        break;
    case SPICE_MSGC_ACK:
        if (rcc->priv->ack_data.client_generation == rcc->priv->ack_data.generation) {
            rcc->priv->ack_data.messages_window -= rcc->priv->ack_data.client_window;
            red_channel_client_push(rcc);
        }
        break;
    case SPICE_MSGC_DISCONNECTING:
        break;
    case SPICE_MSGC_MIGRATE_FLUSH_MARK:
        if (!rcc->priv->wait_migrate_flush_mark) {
            spice_error("unexpected flush mark");
            return FALSE;
        }
        red_channel_client_handle_migrate_flush_mark(rcc);
        rcc->priv->wait_migrate_flush_mark = FALSE;
        break;
    case SPICE_MSGC_MIGRATE_DATA:
        red_channel_client_handle_migrate_data(rcc, size, message);
        break;
    case SPICE_MSGC_PONG:
        red_channel_client_handle_pong(rcc, message);
        break;
    default:
        spice_printerr("invalid message type %u", type);
        return FALSE;
    }
    return TRUE;
}

void red_channel_client_init_send_data(RedChannelClient *rcc, uint16_t msg_type, RedPipeItem *item)
{
    spice_assert(red_channel_client_no_item_being_sent(rcc));
    spice_assert(msg_type != 0);
    rcc->priv->send_data.header.set_msg_type(&rcc->priv->send_data.header, msg_type);
    rcc->priv->send_data.item = item;
    if (item) {
        red_pipe_item_ref(item);
    }
}

void red_channel_client_begin_send_message(RedChannelClient *rcc)
{
    SpiceMarshaller *m = rcc->priv->send_data.marshaller;

    // TODO - better check: type in channel_allowed_types. Better: type in channel_allowed_types(channel_state)
    if (rcc->priv->send_data.header.get_msg_type(&rcc->priv->send_data.header) == 0) {
        spice_printerr("BUG: header->type == 0");
        return;
    }

    /* canceling the latency test timer till the nework is idle */
    red_channel_client_cancel_ping_timer(rcc);

    spice_marshaller_flush(m);
    rcc->priv->send_data.size = spice_marshaller_get_total_size(m);
    rcc->priv->send_data.header.set_msg_size(&rcc->priv->send_data.header,
                                             rcc->priv->send_data.size -
                                             rcc->priv->send_data.header.header_size);
    rcc->priv->send_data.header.set_msg_serial(&rcc->priv->send_data.header,
                                               ++rcc->priv->send_data.last_sent_serial);
    rcc->priv->ack_data.messages_window++;
    rcc->priv->send_data.header.data = NULL; /* avoid writing to this until we have a new message */
    red_channel_client_send(rcc);
}

SpiceMarshaller *red_channel_client_switch_to_urgent_sender(RedChannelClient *rcc)
{
    spice_assert(red_channel_client_no_item_being_sent(rcc));
    spice_assert(rcc->priv->send_data.header.data != NULL);
    rcc->priv->send_data.main.header_data = rcc->priv->send_data.header.data;
    rcc->priv->send_data.main.item = rcc->priv->send_data.item;

    rcc->priv->send_data.marshaller = rcc->priv->send_data.urgent.marshaller;
    rcc->priv->send_data.item = NULL;
    red_channel_client_reset_send_data(rcc);
    return rcc->priv->send_data.marshaller;
}

uint64_t red_channel_client_get_message_serial(RedChannelClient *rcc)
{
    return rcc->priv->send_data.last_sent_serial + 1;
}

void red_channel_client_set_message_serial(RedChannelClient *rcc, uint64_t serial)
{
    rcc->priv->send_data.last_sent_serial = serial - 1;
}

static inline gboolean prepare_pipe_add(RedChannelClient *rcc, RedPipeItem *item)
{
    spice_assert(rcc && item);
    if (SPICE_UNLIKELY(!red_channel_client_is_connected(rcc))) {
        spice_debug("rcc is disconnected %p", rcc);
        red_pipe_item_unref(item);
        return FALSE;
    }
    if (g_queue_is_empty(&rcc->priv->pipe) && rcc->priv->stream->watch) {
        SpiceCoreInterfaceInternal *core;
        core = red_channel_get_core_interface(rcc->priv->channel);
        core->watch_update_mask(core, rcc->priv->stream->watch,
                                SPICE_WATCH_EVENT_READ | SPICE_WATCH_EVENT_WRITE);
    }
    return TRUE;
}

void red_channel_client_pipe_add(RedChannelClient *rcc, RedPipeItem *item)
{

    if (!prepare_pipe_add(rcc, item)) {
        return;
    }
    g_queue_push_head(&rcc->priv->pipe, item);
}

void red_channel_client_pipe_add_push(RedChannelClient *rcc, RedPipeItem *item)
{
    red_channel_client_pipe_add(rcc, item);
    red_channel_client_push(rcc);
}

void red_channel_client_pipe_add_after_pos(RedChannelClient *rcc,
                                           RedPipeItem *item,
                                           GList *pipe_item_pos)
{
    spice_assert(pipe_item_pos);
    if (!prepare_pipe_add(rcc, item)) {
        return;
    }

    g_queue_insert_after(&rcc->priv->pipe, pipe_item_pos, item);
}

void red_channel_client_pipe_add_after(RedChannelClient *rcc,
                                       RedPipeItem *item,
                                       RedPipeItem *pos)
{
    GList *prev;

    spice_assert(pos);
    prev = g_queue_find(&rcc->priv->pipe, pos);
    g_return_if_fail(prev != NULL);

    red_channel_client_pipe_add_after_pos(rcc, item, prev);
}

int red_channel_client_pipe_item_is_linked(RedChannelClient *rcc,
                                           RedPipeItem *item)
{
    return g_queue_find(&rcc->priv->pipe, item) != NULL;
}

void red_channel_client_pipe_add_tail(RedChannelClient *rcc,
                                      RedPipeItem *item)
{
    if (!prepare_pipe_add(rcc, item)) {
        return;
    }
    g_queue_push_tail(&rcc->priv->pipe, item);
}

void red_channel_client_pipe_add_tail_and_push(RedChannelClient *rcc, RedPipeItem *item)
{
    if (!prepare_pipe_add(rcc, item)) {
        return;
    }
    g_queue_push_tail(&rcc->priv->pipe, item);
    red_channel_client_push(rcc);
}

void red_channel_client_pipe_add_type(RedChannelClient *rcc, int pipe_item_type)
{
    RedPipeItem *item = spice_new(RedPipeItem, 1);

    red_pipe_item_init(item, pipe_item_type);
    red_channel_client_pipe_add(rcc, item);
    red_channel_client_push(rcc);
}

void red_channel_client_pipe_add_empty_msg(RedChannelClient *rcc, int msg_type)
{
    RedEmptyMsgPipeItem *item = spice_new(RedEmptyMsgPipeItem, 1);

    red_pipe_item_init(&item->base, RED_PIPE_ITEM_TYPE_EMPTY_MSG);
    item->msg = msg_type;
    red_channel_client_pipe_add(rcc, &item->base);
    red_channel_client_push(rcc);
}

gboolean red_channel_client_pipe_is_empty(RedChannelClient *rcc)
{
    g_return_val_if_fail(rcc != NULL, TRUE);
    return g_queue_is_empty(&rcc->priv->pipe);
}

uint32_t red_channel_client_get_pipe_size(RedChannelClient *rcc)
{
    return g_queue_get_length(&rcc->priv->pipe);
}

GQueue* red_channel_client_get_pipe(RedChannelClient *rcc)
{
    return &rcc->priv->pipe;
}

gboolean red_channel_client_is_mini_header(RedChannelClient *rcc)
{
    return rcc->priv->is_mini_header;
}

static gboolean red_channel_client_default_is_connected(RedChannelClient *rcc)
{
    return rcc->priv->channel
        && (g_list_find(red_channel_get_clients(rcc->priv->channel), rcc) != NULL);
}

gboolean red_channel_client_is_connected(RedChannelClient *rcc)
{
    RedChannelClientClass *klass = RED_CHANNEL_CLIENT_GET_CLASS(rcc);

    g_return_val_if_fail(klass->is_connected != NULL, FALSE);
    return klass->is_connected(rcc);
}

static void red_channel_client_clear_sent_item(RedChannelClient *rcc)
{
    red_channel_client_release_sent_item(rcc);
    rcc->priv->send_data.blocked = FALSE;
    rcc->priv->send_data.size = 0;
}

// TODO: again - what is the context exactly? this happens in channel disconnect. but our
// current red_channel_shutdown also closes the socket - is there a socket to close?
// are we reading from an fd here? arghh
static void red_channel_client_pipe_clear(RedChannelClient *rcc)
{
    RedPipeItem *item;

    if (rcc) {
        red_channel_client_clear_sent_item(rcc);
    }
    while ((item = g_queue_pop_head(&rcc->priv->pipe)) != NULL) {
        red_pipe_item_unref(item);
    }
}

void red_channel_client_ack_zero_messages_window(RedChannelClient *rcc)
{
    rcc->priv->ack_data.messages_window = 0;
}

void red_channel_client_ack_set_client_window(RedChannelClient *rcc, int client_window)
{
    rcc->priv->ack_data.client_window = client_window;
}

void red_channel_client_push_set_ack(RedChannelClient *rcc)
{
    red_channel_client_pipe_add_type(rcc, RED_PIPE_ITEM_TYPE_SET_ACK);
}

static void red_channel_client_default_disconnect(RedChannelClient *rcc)
{
    RedChannel *channel = rcc->priv->channel;
    SpiceCoreInterfaceInternal *core = red_channel_get_core_interface(channel);
    uint32_t type, id;

    if (!red_channel_client_is_connected(rcc)) {
        return;
    }
    g_object_get(channel, "channel-type", &type, "id", &id, NULL);
    spice_printerr("rcc=%p (channel=%p type=%d id=%d)", rcc, channel,
                   type, id);
    red_channel_client_pipe_clear(rcc);
    if (rcc->priv->stream->watch) {
        core->watch_remove(core, rcc->priv->stream->watch);
        rcc->priv->stream->watch = NULL;
    }
    if (rcc->priv->latency_monitor.timer) {
        core->timer_remove(core, rcc->priv->latency_monitor.timer);
        rcc->priv->latency_monitor.timer = NULL;
    }
    if (rcc->priv->connectivity_monitor.timer) {
        core->timer_remove(core, rcc->priv->connectivity_monitor.timer);
        rcc->priv->connectivity_monitor.timer = NULL;
    }
    red_channel_remove_client(channel, rcc);
    red_channel_on_disconnect(channel, rcc);
}

void red_channel_client_disconnect(RedChannelClient *rcc)
{
    RedChannelClientClass *klass = RED_CHANNEL_CLIENT_GET_CLASS(rcc);

    g_return_if_fail(klass->is_connected != NULL);
    klass->disconnect(rcc);
}

int red_channel_client_is_blocked(RedChannelClient *rcc)
{
    return rcc && rcc->priv->send_data.blocked;
}

int red_channel_client_send_message_pending(RedChannelClient *rcc)
{
    return rcc->priv->send_data.header.get_msg_type(&rcc->priv->send_data.header) != 0;
}

SpiceMarshaller *red_channel_client_get_marshaller(RedChannelClient *rcc)
{
    return rcc->priv->send_data.marshaller;
}

RedsStream *red_channel_client_get_stream(RedChannelClient *rcc)
{
    return rcc->priv->stream;
}

RedClient *red_channel_client_get_client(RedChannelClient *rcc)
{
    return rcc->priv->client;
}

void red_channel_client_set_header_sub_list(RedChannelClient *rcc, uint32_t sub_list)
{
    rcc->priv->send_data.header.set_msg_sub_list(&rcc->priv->send_data.header, sub_list);
}

static void marker_pipe_item_free(RedPipeItem *base)
{
    MarkerPipeItem *item = SPICE_UPCAST(MarkerPipeItem, base);

    if (item->item_in_pipe) {
        *item->item_in_pipe = FALSE;
    }
    free(item);
}

/* TODO: more evil sync stuff. anything with the word wait in it's name. */
int red_channel_client_wait_pipe_item_sent(RedChannelClient *rcc,
                                           GList *item_pos,
                                           int64_t timeout)
{
    uint64_t end_time;
    gboolean item_in_pipe;

    spice_info(NULL);

    if (timeout != -1) {
        end_time = spice_get_monotonic_time_ns() + timeout;
    } else {
        end_time = UINT64_MAX;
    }

    MarkerPipeItem *mark_item = spice_new0(MarkerPipeItem, 1);

    red_pipe_item_init_full(&mark_item->base, RED_PIPE_ITEM_TYPE_MARKER,
                            marker_pipe_item_free);
    item_in_pipe = TRUE;
    mark_item->item_in_pipe = &item_in_pipe;
    red_channel_client_pipe_add_after_pos(rcc, &mark_item->base, item_pos);

    if (red_channel_client_is_blocked(rcc)) {
        red_channel_client_receive(rcc);
        red_channel_client_send(rcc);
    }
    red_channel_client_push(rcc);

    while (item_in_pipe &&
           (timeout == -1 || spice_get_monotonic_time_ns() < end_time)) {
        usleep(CHANNEL_BLOCKED_SLEEP_DURATION);
        red_channel_client_receive(rcc);
        red_channel_client_send(rcc);
        red_channel_client_push(rcc);
    }

    if (item_in_pipe) {
        // still on the queue, make sure won't overwrite the stack variable
        mark_item->item_in_pipe = NULL;
        spice_warning("timeout");
        return FALSE;
    } else {
        return red_channel_client_wait_outgoing_item(rcc,
                                                     timeout == -1 ? -1 : end_time - spice_get_monotonic_time_ns());
    }
}

int red_channel_client_wait_outgoing_item(RedChannelClient *rcc,
                                          int64_t timeout)
{
    uint64_t end_time;
    int blocked;

    if (!red_channel_client_is_blocked(rcc)) {
        return TRUE;
    }
    if (timeout != -1) {
        end_time = spice_get_monotonic_time_ns() + timeout;
    } else {
        end_time = UINT64_MAX;
    }
    spice_info("blocked");

    do {
        usleep(CHANNEL_BLOCKED_SLEEP_DURATION);
        red_channel_client_receive(rcc);
        red_channel_client_send(rcc);
    } while ((blocked = red_channel_client_is_blocked(rcc)) &&
             (timeout == -1 || spice_get_monotonic_time_ns() < end_time));

    if (blocked) {
        spice_warning("timeout");
        return FALSE;
    } else {
        spice_assert(red_channel_client_no_item_being_sent(rcc));
        return TRUE;
    }
}

void red_channel_client_disconnect_if_pending_send(RedChannelClient *rcc)
{
    if (red_channel_client_is_blocked(rcc) || !g_queue_is_empty(&rcc->priv->pipe)) {
        red_channel_client_disconnect(rcc);
    } else {
        spice_assert(red_channel_client_no_item_being_sent(rcc));
    }
}

gboolean red_channel_client_no_item_being_sent(RedChannelClient *rcc)
{
    return !rcc || (rcc->priv->send_data.size == 0);
}

void red_channel_client_pipe_remove_and_release(RedChannelClient *rcc,
                                                RedPipeItem *item)
{
    if (red_channel_client_pipe_remove(rcc, item)) {
        red_pipe_item_unref(item);
    }
}

void red_channel_client_pipe_remove_and_release_pos(RedChannelClient *rcc,
                                                    GList *item_pos)
{
    RedPipeItem *item = item_pos->data;

    g_queue_delete_link(&rcc->priv->pipe, item_pos);
    red_pipe_item_unref(item);
}

/* client mutex should be locked before this call */
gboolean red_channel_client_set_migration_seamless(RedChannelClient *rcc)
{
    gboolean ret = FALSE;
    uint32_t type, id, flags;

    g_object_get(rcc->priv->channel,
                 "channel-type", &type,
                 "id", &id,
                 "migration-flags", &flags,
                 NULL);
    if (flags & SPICE_MIGRATE_NEED_DATA_TRANSFER) {
        rcc->priv->wait_migrate_data = TRUE;
        ret = TRUE;
    }
    spice_debug("channel type %d id %d rcc %p wait data %d", type, id, rcc,
                rcc->priv->wait_migrate_data);

    return ret;
}

void red_channel_client_set_destroying(RedChannelClient *rcc)
{
    rcc->priv->destroying = TRUE;
}

gboolean red_channel_client_is_destroying(RedChannelClient *rcc)
{
    return rcc->priv->destroying;
}

GQuark spice_server_error_quark(void)
{
    return g_quark_from_static_string("spice-server-error-quark");
}
