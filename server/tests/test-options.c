/* -*- Mode: C; c-basic-offset: 4; indent-tabs-mode: nil -*- */
/*
   Copyright (C) 2016 Red Hat, Inc.

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
#include <spice.h>

#include "test-glib-compat.h"
#include "basic-event-loop.h"

static void agent_options(void)
{
    SpiceCoreInterface *core ;
    SpiceServer *server = spice_server_new();

    g_assert_nonnull(server);

    core = basic_event_loop_init();
    g_assert_nonnull(core);

    /* test before init */
    spice_server_set_agent_mouse(server, 0);
    spice_server_set_agent_copypaste(server, 0);
    spice_server_set_agent_file_xfer(server, 0);

    g_assert_cmpint(spice_server_init(server, core), ==, 0);

    /* test after init */
    spice_server_set_agent_mouse(server, 0);
    spice_server_set_agent_copypaste(server, 0);
    spice_server_set_agent_file_xfer(server, 0);

    spice_server_destroy(server);
}

int main(int argc, char *argv[])
{
    g_test_init(&argc, &argv, NULL);

    g_test_add_func("/server/agent options", agent_options);

    return g_test_run();
}
