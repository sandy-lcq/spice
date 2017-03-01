/* -*- Mode: C; c-basic-offset: 4; indent-tabs-mode: nil -*- */
/*
   Copyright (C) 2017 Red Hat, Inc.

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
#include <config.h>
#include <glib.h>
#include <spice.h>

#include "basic-event-loop.h"

/* GLIB_CHECK_VERSION(2, 40, 0) */
#ifndef g_assert_nonnull
#define g_assert_nonnull g_assert
#endif

static void leaks(void)
{
    int result;
    SpiceCoreInterface *core;
    SpiceServer *server = spice_server_new();

    g_assert_nonnull(server);

    core = basic_event_loop_init();
    g_assert_nonnull(core);

    g_assert_cmpint(spice_server_init(server, core), ==, 0);

    /* cause the allocation of spice name */
    spice_server_set_name(server, "Test Spice Name");

    /* cause the allocation of security options */
    result = spice_server_set_channel_security(server, "main", SPICE_CHANNEL_SECURITY_SSL);
    g_assert_cmpint(result, ==, 0);

    spice_server_destroy(server);
    basic_event_loop_destroy();
}

int main(int argc, char *argv[])
{
    g_test_init(&argc, &argv, NULL);

    g_test_add_func("/server/server leaks", leaks);

    return g_test_run();
}
