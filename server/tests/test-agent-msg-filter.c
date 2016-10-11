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
#include <string.h>

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

static void test_agent_msg_filter_run(void)
{
    AgentMsgFilter filter;
    union {
        struct VDAgentMessage msg_header;
        uint8_t data[VD_AGENT_MAX_DATA_SIZE];
    } msg;
    uint32_t len, type;

    agent_msg_filter_init(&filter, TRUE, TRUE, TRUE, TRUE); /* discard all */

    /* message size too large */
    len = VD_AGENT_MAX_DATA_SIZE + 1;
    g_assert_cmpint(agent_msg_filter_process_data(&filter, msg.data, len), ==,
                    AGENT_MSG_FILTER_PROTO_ERROR);

    /* data len too small */
    len = 0;
    g_assert_cmpint(agent_msg_filter_process_data(&filter, msg.data, len), ==,
                    AGENT_MSG_FILTER_PROTO_ERROR);

    /* invalid protocol */
    memset(&msg.msg_header, 0, sizeof(msg.msg_header));
    len = sizeof(msg.msg_header);
    g_assert_cmpint(agent_msg_filter_process_data(&filter, msg.data, len), ==,
                    AGENT_MSG_FILTER_PROTO_ERROR);

    /* all messages should be discarded */
    msg.msg_header.protocol = VD_AGENT_PROTOCOL;
    for (type = VD_AGENT_MOUSE_STATE; type < VD_AGENT_END_MESSAGE; type++) {
        msg.msg_header.type = type;
        g_assert_cmpint(agent_msg_filter_process_data(&filter, msg.data, len), ==,
                        AGENT_MSG_FILTER_DISCARD);
    }

    /* data exceeds size from header */
    msg.msg_header.size = 1;
    len = sizeof(msg.msg_header) + msg.msg_header.size + 1;
    g_assert_cmpint(agent_msg_filter_process_data(&filter, msg.data, len), ==,
                    AGENT_MSG_FILTER_PROTO_ERROR);

    len = sizeof(msg.msg_header) + msg.msg_header.size; /* restore correct size */

    /* forward everything to the agent */
    agent_msg_filter_init(&filter, TRUE, TRUE, FALSE, FALSE);
    for (type = VD_AGENT_MOUSE_STATE; type < VD_AGENT_END_MESSAGE; type++) {
        msg.msg_header.type = type;
        g_assert_cmpint(agent_msg_filter_process_data(&filter, msg.data, len), ==,
                        AGENT_MSG_FILTER_OK);
    }

    /* filter everything */
    agent_msg_filter_config(&filter, FALSE, FALSE, TRUE);
    for (type = VD_AGENT_MOUSE_STATE; type < VD_AGENT_END_MESSAGE; type++) {
        AgentMsgFilterResult result;
        msg.msg_header.type = type;
        switch (type) {
        case VD_AGENT_CLIPBOARD:
        case VD_AGENT_CLIPBOARD_GRAB:
        case VD_AGENT_CLIPBOARD_REQUEST:
        case VD_AGENT_CLIPBOARD_RELEASE:
        case VD_AGENT_FILE_XFER_START:
        case VD_AGENT_FILE_XFER_STATUS:
        case VD_AGENT_FILE_XFER_DATA:
            result = AGENT_MSG_FILTER_DISCARD;
            break;
        case VD_AGENT_MONITORS_CONFIG:
            result = AGENT_MSG_FILTER_MONITORS_CONFIG;
            break;
        default:
            result = AGENT_MSG_FILTER_OK;
        }
        g_assert_cmpint(agent_msg_filter_process_data(&filter, msg.data, len), ==, result);
    }
}

int main(int argc, char *argv[])
{
    g_test_init(&argc, &argv, NULL);

    g_test_add_func("/agent message filter/setup", test_agent_msg_filter_setup);
    g_test_add_func("/agent message filter/run", test_agent_msg_filter_run);

    return g_test_run();
}
