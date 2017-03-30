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
#ifndef TEST_GLIB_COMPAT_H_
#define TEST_GLIB_COMPAT_H_

#include <glib.h>

#include "glib-compat.h"

#if !GLIB_CHECK_VERSION(2, 34, 0)
void g_test_assert_expected_messages_internal(const char *domain,
                                              const char *file, int line, const char *func);
#define g_test_assert_expected_messages() \
    g_test_assert_expected_messages_internal (G_LOG_DOMAIN, __FILE__, __LINE__, G_STRFUNC)
void g_test_expect_message(const gchar *log_domain, GLogLevelFlags log_level,
                           const gchar *pattern);
#endif

/* GLIB_CHECK_VERSION(2, 40, 0) */
#ifndef g_assert_nonnull
#define g_assert_nonnull g_assert
#endif
#ifndef g_assert_null
#define g_assert_null(ptr) g_assert((ptr) == NULL)
#endif

#ifndef g_assert_true
#define g_assert_true g_assert
#endif
#ifndef g_assert_false
#define g_assert_false(cond) g_assert(!(cond))
#endif

#endif // TEST_GLIB_COMPAT_H_
