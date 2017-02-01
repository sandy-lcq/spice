/* -*- Mode: C; c-basic-offset: 4; indent-tabs-mode: nil -*- */
/*
   Copyright (C) 2009-2015 Red Hat, Inc.

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
/**
 * Test vdagent guest to server messages
 */

#include <config.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include <spice/vd_agent.h>

#include "test-display-base.h"

SpiceCoreInterface *core;
SpiceTimer *ping_timer;

int ping_ms = 100;

#ifndef MIN
#define MIN(a, b) ((a) > (b) ? (b) : (a))
#endif

#if !GLIB_CHECK_VERSION(2, 34, 0)

/* The code in this #ifdef block is taken from glib and is licensed under the
 * GNU Lesser General Public License version 2 or later.
 *
 * Copyright (C) 1995-1997  Peter Mattis, Spencer Kimball and Josh MacDonald
 * Modified by the GLib Team and others 1997-2000.  See GLib AUTHORS
 * file for a list of people on the GLib Team.
 */

typedef struct {
    gchar *log_domain;
    GLogLevelFlags log_level;
    gchar *pattern;
} GTestExpectedMessage;

static GSList *expected_messages = NULL;

static gboolean fatal_log_filter(const gchar *log_domain,
                                 GLogLevelFlags log_level,
                                 const gchar *msg,
                                 gpointer user_data)
{
    GTestExpectedMessage *expected = expected_messages->data;

    if ((g_strcmp0(expected->log_domain, log_domain) == 0)
            && ((log_level & expected->log_level) == expected->log_level)
            && (g_pattern_match_simple(expected->pattern, msg))) {
        expected_messages = g_slist_delete_link(expected_messages,
                                                expected_messages);
        g_free (expected->log_domain);
        g_free (expected->pattern);
        g_free (expected);

        return FALSE;
    }
    return TRUE;
}

static void
g_test_assert_expected_messages_internal (const char     *domain,
                                          const char     *file,
                                          int             line,
                                          const char     *func)
{
  if (expected_messages)
    {
      GTestExpectedMessage *expected;
      gchar *message;

      expected = expected_messages->data;

      message = g_strdup_printf ("Did not see expected message %s: %s",
                                 expected->log_domain ? expected->log_domain : "**",
                                 expected->pattern);
      g_error ("%s", message);
      g_free (message);
    }
}

#define g_test_assert_expected_messages() g_test_assert_expected_messages_internal (G_LOG_DOMAIN, __FILE__, __LINE__, G_STRFUNC)

static void
g_test_expect_message (const gchar    *log_domain,
                       GLogLevelFlags  log_level,
                       const gchar    *pattern)
{
  GTestExpectedMessage *expected;

  g_return_if_fail (log_level != 0);
  g_return_if_fail (pattern != NULL);
  g_return_if_fail (~log_level & G_LOG_LEVEL_ERROR);

  if (expected_messages == NULL)
    {
      g_test_log_set_fatal_handler(fatal_log_filter, NULL);
    }

  expected = g_new (GTestExpectedMessage, 1);
  expected->log_domain = g_strdup (log_domain);
  expected->log_level = log_level;
  expected->pattern = g_strdup (pattern);

  if ((log_level & G_LOG_LEVEL_MASK) <= G_LOG_LEVEL_WARNING)
    {
      expected_messages = g_slist_append (expected_messages, expected);
    }
}

#endif /* GLIB_CHECK_VERSION(2, 34, 0) */


static int vmc_write(SPICE_GNUC_UNUSED SpiceCharDeviceInstance *sin,
                     SPICE_GNUC_UNUSED const uint8_t *buf,
                     int len)
{
    return len;
}

static int vmc_read(SPICE_GNUC_UNUSED SpiceCharDeviceInstance *sin,
                    uint8_t *buf,
                    int len)
{
    static uint8_t c = 0;
    static uint8_t message[2048];
    static unsigned pos = 0;
    static unsigned message_size;
    int ret;

    if (pos == sizeof(message)) {
        g_message("sent whole message");
        pos++; /* Only print message once */
    }
    if (pos > sizeof(message)) {
        return 0;
    }
    if (pos == 0) {
        VDIChunkHeader *hdr = (VDIChunkHeader *)message;
        VDAgentMessage *msg = (VDAgentMessage *)&hdr[1];
        uint8_t *p = message;
        int size = sizeof(message);
        message_size = size;
        /* fill in message */
        hdr->port = VDP_SERVER_PORT;
        hdr->size = message_size - sizeof(VDIChunkHeader);
        msg->protocol = VD_AGENT_PROTOCOL;
        msg->type = VD_AGENT_END_MESSAGE;
        msg->opaque = 0;
        msg->size = message_size - sizeof(VDIChunkHeader) - sizeof(VDAgentMessage);
        size -= sizeof(VDIChunkHeader) + sizeof(VDAgentMessage);
        p += sizeof(VDIChunkHeader) + sizeof(VDAgentMessage);
        for (; size; --size, ++p, ++c)
            *p = c;
    }
    ret = MIN(message_size - pos, len);
    memcpy(buf, &message[pos], ret);
    pos += ret;
    //printf("vmc_read %d (ret %d)\n", len, ret);
    return ret;
}

static void vmc_state(SPICE_GNUC_UNUSED SpiceCharDeviceInstance *sin,
                      SPICE_GNUC_UNUSED int connected)
{
}

static SpiceCharDeviceInterface vmc_interface = {
    .base.type          = SPICE_INTERFACE_CHAR_DEVICE,
    .base.description   = "test spice virtual channel char device",
    .base.major_version = SPICE_INTERFACE_CHAR_DEVICE_MAJOR,
    .base.minor_version = SPICE_INTERFACE_CHAR_DEVICE_MINOR,
    .state              = vmc_state,
    .write              = vmc_write,
    .read               = vmc_read,
};

static SpiceCharDeviceInstance vmc_instance = {
    .subtype = "vdagent",
};

static void test_multiple_vmc_devices(void)
{
    SpiceCharDeviceInstance vmc_instances[2] = {
        { .subtype = "vdagent", },
        { .subtype = "vdagent", }
    };
    int status;

    SpiceCoreInterface *core = basic_event_loop_init();
    Test *test = test_new(core);

    g_test_expect_message(G_LOG_DOMAIN, G_LOG_LEVEL_WARNING,
                          "*spice_server_char_device_add_interface: vdagent already attached");
    g_test_expect_message(G_LOG_DOMAIN, G_LOG_LEVEL_CRITICAL,
                          "*spice_server_remove_interface: assertion ?char_device->st != NULL'*");
    vmc_instances[0].base.sif = &vmc_interface.base;
    spice_server_add_interface(test->server, &vmc_instances[0].base);
    vmc_instances[1].base.sif = &vmc_interface.base;
    spice_server_add_interface(test->server, &vmc_instances[1].base);
    status = spice_server_remove_interface(&vmc_instances[1].base);
    g_assert_cmpint(status, ==, -1);
    status = spice_server_remove_interface(&vmc_instances[0].base);
    g_assert_cmpint(status, ==, 0);
    g_test_assert_expected_messages();
    test_destroy(test);
    basic_event_loop_destroy();
}

static void test_duplicate_removal(void)
{
    SpiceCoreInterface *core = basic_event_loop_init();
    Test *test = test_new(core);
    int status;

    g_test_expect_message(G_LOG_DOMAIN, G_LOG_LEVEL_CRITICAL,
                          "*spice_server_remove_interface: assertion ?char_device->st != NULL'*");
    vmc_instance.base.sif = &vmc_interface.base;
    spice_server_add_interface(test->server, &vmc_instance.base);
    status = spice_server_remove_interface(&vmc_instance.base);
    g_assert_cmpint(status, ==, 0);
    status = spice_server_remove_interface(&vmc_instance.base);
    g_assert_cmpint(status, ==, -1);
    g_test_assert_expected_messages();
    test_destroy(test);
    basic_event_loop_destroy();
}

static void test_agent_to_server(void)
{
    SpiceCoreInterface *core = basic_event_loop_init();
    Test *test = test_new(core);

    g_test_expect_message(G_LOG_DOMAIN, G_LOG_LEVEL_MESSAGE, "sent whole message");
    vmc_instance.base.sif = &vmc_interface.base;
    spice_server_add_interface(test->server, &vmc_instance.base);
    g_test_assert_expected_messages();
    test_destroy(test);
    basic_event_loop_destroy();
}

int main(int argc, char *argv[])
{
    g_test_init(&argc, &argv, NULL);

    g_test_add_func("/server/vdagent/agent-to-server", test_agent_to_server);
    g_test_add_func("/server/vdagent/duplicate-removal", test_duplicate_removal);
    g_test_add_func("/server/vdagent/multiple-vmc-devices", test_multiple_vmc_devices);

    return g_test_run();
}
