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
/*
 * Test SASL connections.
 */
#include <config.h>

#include <unistd.h>
#include <spice.h>
#include <stdbool.h>
#include <sasl/sasl.h>

#include "test-glib-compat.h"
#include "basic-event-loop.h"

static char *mechlist;

static void
check_sasl_conn(sasl_conn_t *conn)
{
    g_assert_nonnull(conn);
}

int
sasl_server_init(const sasl_callback_t *callbacks, const char *appname)
{
    g_assert_null(callbacks);
    return SASL_OK;
}

int
sasl_decode(sasl_conn_t *conn,
            const char *input, unsigned inputlen,
            const char **output, unsigned *outputlen)
{
    check_sasl_conn(conn);
    return SASL_NOTDONE;
}

int
sasl_encode(sasl_conn_t *conn,
            const char *input, unsigned inputlen,
            const char **output, unsigned *outputlen)
{
    check_sasl_conn(conn);
    return SASL_NOTDONE;
}

const char *
sasl_errdetail(sasl_conn_t *conn)
{
    check_sasl_conn(conn);
    return "XXX";
}

const char *
sasl_errstring(int saslerr,
               const char *langlist,
               const char **outlang)
{
    return "YYY";
}

int
sasl_getprop(sasl_conn_t *conn, int propnum,
             const void **pvalue)
{
    check_sasl_conn(conn);
    g_assert_nonnull(pvalue);

    if (propnum == SASL_SSF) {
        static const int val = 64;
        *pvalue = &val;
    }
    return SASL_OK;
}

int
sasl_setprop(sasl_conn_t *conn,
             int propnum,
             const void *value)
{
    check_sasl_conn(conn);
    g_assert(value);
    return SASL_OK;
}

int
sasl_server_new(const char *service,
                const char *serverFQDN,
                const char *user_realm,
                const char *iplocalport,
                const char *ipremoteport,
                const sasl_callback_t *callbacks,
                unsigned flags,
                sasl_conn_t **pconn)
{
    g_assert_nonnull(pconn);
    g_assert_null(callbacks);

    *pconn = GUINT_TO_POINTER(0xdeadbeef);
    return SASL_OK;
}

void
sasl_dispose(sasl_conn_t **pconn)
{
    g_assert_nonnull(pconn);
    check_sasl_conn(*pconn);
}

int
sasl_listmech(sasl_conn_t *conn,
              const char *user,
              const char *prefix,
              const char *sep,
              const char *suffix,
              const char **result,
              unsigned *plen,
              int *pcount)
{
    check_sasl_conn(conn);
    g_assert_nonnull(result);
    g_assert_nonnull(prefix);
    g_assert_nonnull(sep);
    g_assert_nonnull(suffix);

    g_free(mechlist);
    mechlist = g_strjoin("", prefix, "ONE", sep, "TWO", sep, "THREE", suffix, NULL);
    *result = mechlist;
    return SASL_OK;
}

int
sasl_server_start(sasl_conn_t *conn,
                  const char *mech,
                  const char *clientin,
                  unsigned clientinlen,
                  const char **serverout,
                  unsigned *serveroutlen)
{
    check_sasl_conn(conn);
    g_assert_nonnull(serverout);

    *serverout = "foo";
    *serveroutlen = 3;
    return SASL_OK;
}

int
sasl_server_step(sasl_conn_t *conn,
                 const char *clientin,
                 unsigned clientinlen,
                 const char **serverout,
                 unsigned *serveroutlen)
{
    check_sasl_conn(conn);
    g_assert_nonnull(serverout);

    *serverout = "foo";
    *serveroutlen = 3;
    return SASL_OK;
}

int
main(int argc, char *argv[])
{
    return 0;
}
