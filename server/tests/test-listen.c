/* -*- Mode: C; c-basic-offset: 4; indent-tabs-mode: nil -*- */
/*
   Copyright (C) 2018 Red Hat, Inc.

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
/*
 * This tests the external API entry points to configure the address/port
 * spice-server is listening on
 */
#include <config.h>

#include "basic-event-loop.h"

#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <gio/gio.h>

/* Arbitrary base port, we want a port which is not in use by the system, and
 * by another of our tests (in case of parallel runs)
 */
#define BASE_PORT 5728

static bool error_is_set(GError **error)
{
    return ((error != NULL) && (*error != NULL));
}

typedef struct {
    SpiceCoreInterface *core;
    SpiceTimer *exit_mainloop_timer;
    SpiceTimer *timeout_timer;
} TestEventLoop;

static void timeout_cb(SPICE_GNUC_UNUSED void *opaque)
{
    g_assert_not_reached();
}

static void exit_mainloop_cb(SPICE_GNUC_UNUSED void *opaque)
{
    basic_event_loop_quit();
}

static void test_event_loop_quit(TestEventLoop *event_loop)
{
    event_loop->core->timer_start(event_loop->exit_mainloop_timer, 0);
}

static void test_event_loop_init(TestEventLoop *event_loop)
{
    event_loop->core = basic_event_loop_init();
    event_loop->timeout_timer = event_loop->core->timer_add(timeout_cb, NULL);
    event_loop->exit_mainloop_timer = event_loop->core->timer_add(exit_mainloop_cb, NULL);
}

static void test_event_loop_destroy(TestEventLoop *event_loop)
{
    if (event_loop->timeout_timer != NULL) {
        event_loop->core->timer_remove(event_loop->timeout_timer);
        event_loop->timeout_timer = NULL;
    }
    if (event_loop->exit_mainloop_timer != NULL) {
        event_loop->core->timer_remove(event_loop->exit_mainloop_timer);
        event_loop->exit_mainloop_timer = NULL;
    }
    basic_event_loop_destroy();
    event_loop->core = NULL;
}

static void test_event_loop_run(TestEventLoop *event_loop)
{
    event_loop->core->timer_start(event_loop->timeout_timer, 5000);
    basic_event_loop_mainloop();
}

static GIOStream *fake_client_connect(GSocketConnectable *connectable, GError **error)
{
    GSocketClient *client;
    GSocketConnection *connection;

    client = g_socket_client_new();
    connection = g_socket_client_connect(client, connectable, NULL, error);

    g_object_unref(client);

    return G_IO_STREAM(connection);
}

static void check_magic(GIOStream *io_stream, GError **error)
{
    uint8_t buffer[4];
    gsize bytes_read;
    gsize bytes_written;
    GInputStream *input_stream;
    GOutputStream *output_stream;

    /* send dummy data to trigger a response from the server */
    output_stream = g_io_stream_get_output_stream(io_stream);
    memset(buffer, 0xa5, G_N_ELEMENTS(buffer));
    g_output_stream_write_all(output_stream, buffer, G_N_ELEMENTS(buffer), &bytes_written, NULL, error);
    if (error_is_set(error)) {
        return;
    }

    input_stream = g_io_stream_get_input_stream(io_stream);
    g_input_stream_read_all(input_stream, buffer, G_N_ELEMENTS(buffer), &bytes_read, NULL, error);
    if (error_is_set(error)) {
        return;
    }
    g_assert_cmpuint(bytes_read, ==, G_N_ELEMENTS(buffer));
    g_assert_cmpint(memcmp(buffer, "REDQ", 4), ==, 0);
}

typedef struct
{
    GSocketConnectable *connectable;
    TestEventLoop *event_loop;
} ThreadData;

static gpointer check_magic_thread(gpointer data)
{
    GError *error = NULL;
    ThreadData *thread_data = data;
    GSocketConnectable *connectable = G_SOCKET_CONNECTABLE(thread_data->connectable);
    GIOStream *stream;

    stream = fake_client_connect(connectable, &error);
    g_assert_no_error(error);
    check_magic(stream, &error);
    g_assert_no_error(error);

    g_object_unref(stream);
    g_object_unref(connectable);
    g_free(thread_data);

    test_event_loop_quit(thread_data->event_loop);

    return NULL;
}

static gpointer check_no_connect_thread(gpointer data)
{
    GError *error = NULL;
    ThreadData *thread_data = data;
    GSocketConnectable *connectable = G_SOCKET_CONNECTABLE(thread_data->connectable);
    GIOStream *stream;

    stream = fake_client_connect(connectable, &error);
    g_assert(error != NULL);
    g_assert(stream == NULL);
    g_clear_error(&error);

    g_object_unref(connectable);
    g_free(thread_data);

    test_event_loop_quit(thread_data->event_loop);

    return NULL;
}

static GThread *fake_client_new(GThreadFunc thread_func,
                                const char *hostname, int port,
                                TestEventLoop *event_loop)
{
    ThreadData *thread_data = g_new0(ThreadData, 1);

    g_assert_cmpuint(port, >, 0);
    g_assert_cmpuint(port, <, 65536);
    thread_data->connectable = g_network_address_new(hostname, port);
    thread_data->event_loop = event_loop;

    /* check_magic_thread will assume ownership of 'connectable' */
    return g_thread_new("fake-client-thread", thread_func, thread_data);
}

static void test_connect_plain(void)
{
    GThread *thread;
    int result;

    TestEventLoop event_loop = { 0, };

    test_event_loop_init(&event_loop);

    /* server */
    SpiceServer *server = spice_server_new();
    spice_server_set_name(server, "SPICE listen test");
    spice_server_set_noauth(server);
    spice_server_set_port(server, BASE_PORT);
    result = spice_server_init(server, event_loop.core);
    g_assert_cmpint(result, ==, 0);

    /* fake client */
    thread = fake_client_new(check_magic_thread, "localhost", BASE_PORT, &event_loop);
    test_event_loop_run(&event_loop);
    g_assert_null(g_thread_join(thread));

    test_event_loop_destroy(&event_loop);
    spice_server_destroy(server);
}

static void test_connect_ko(void)
{
    GThread *thread;
    TestEventLoop event_loop = { 0, };

    test_event_loop_init(&event_loop);

    /* fake client */
    thread = fake_client_new(check_no_connect_thread, "localhost", BASE_PORT, &event_loop);
    test_event_loop_run(&event_loop);
    g_assert_null(g_thread_join(thread));

    test_event_loop_destroy(&event_loop);
}

int main(int argc, char **argv)
{
    g_test_init(&argc, &argv, NULL);

    g_test_add_func("/server/listen/connect_plain", test_connect_plain);
    g_test_add_func("/server/listen/connect_ko", test_connect_ko);

    return g_test_run();
}
