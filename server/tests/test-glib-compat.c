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

#include "test-glib-compat.h"

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

void
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

void
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
