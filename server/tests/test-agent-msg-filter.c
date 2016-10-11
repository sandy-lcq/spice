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
#include <glib.h>
#include <spice/vd_agent.h>

#include "agent-msg-filter.h"

static void test_agent_msg_filter_setup(void)
{
    AgentMsgFilter filter = {
        .msg_data_to_read = 42,
        .result = AGENT_MSG_FILTER_PROTO_ERROR,
        .copy_paste_enabled = FALSE,
        .file_xfer_enabled = FALSE,
        .use_client_monitors_config = FALSE,
        .discard_all = TRUE,
    };

    /* config doesn't change other fields */
    agent_msg_filter_config(&filter, TRUE, TRUE, TRUE);
    g_assert_cmpint(filter.msg_data_to_read, ==, 42);
    g_assert_cmpint(filter.result, ==, AGENT_MSG_FILTER_PROTO_ERROR);
    g_assert(filter.copy_paste_enabled == TRUE);
    g_assert(filter.file_xfer_enabled == TRUE);
    g_assert(filter.use_client_monitors_config == TRUE);
    g_assert(filter.discard_all == TRUE);

    /* init clears filter */
    agent_msg_filter_init(&filter, FALSE, FALSE, FALSE, FALSE);
    g_assert_cmpint(filter.msg_data_to_read, ==, 0);
    g_assert_cmpint(filter.result, ==, AGENT_MSG_FILTER_OK);
    g_assert(filter.copy_paste_enabled == FALSE);
    g_assert(filter.file_xfer_enabled == FALSE);
    g_assert(filter.use_client_monitors_config == FALSE);
    g_assert(filter.discard_all == FALSE);
}

int main(int argc, char *argv[])
{
    g_test_init(&argc, &argv, NULL);

    g_test_add_func("/agent message filter/setup", test_agent_msg_filter_setup);

    return g_test_run();
}
