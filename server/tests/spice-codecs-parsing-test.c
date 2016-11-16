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
#include <spice.h>

/* GLIB_CHECK_VERSION(2, 40, 0) */
#ifndef g_assert_nonnull
#define g_assert_nonnull g_assert
#endif

static void codecs_good(void)
{
    guint i;
    const gchar *codecs[] = {
        "",
        ";",
        ";;;;",
        "spice:mjpeg",
        "spice:mjpeg;;;",
        "spice:mjpeg;;spice:mjpeg;;;",
        ";;spice:mjpeg;;spice:mjpeg;;;",
#if defined(HAVE_GSTREAMER_1_0) || defined(HAVE_GSTREAMER_0_10)
        "gstreamer:mjpeg;gstreamer:h264;gstreamer:vp8;",
        ";;spice:mjpeg;;gstreamer:mjpeg;gstreamer:h264;gstreamer:vp8;",
#endif
    };

    SpiceServer *server = spice_server_new();

    g_assert_nonnull(server);

    for (i = 0; i < G_N_ELEMENTS(codecs); ++i) {
        g_assert_cmpint(spice_server_set_video_codecs(server, codecs[i]), ==, 0);
    }

    spice_server_destroy(server);
}

/* g_test_expect_message is available since Glib 2.34 */
#if GLIB_CHECK_VERSION(2, 34, 0)
static void codecs_bad(void)
{
    guint i;
    const struct {
        const gchar *codecs;
        const GLogLevelFlags log_level;
        const gchar *error_message;
    } test_cases[] = {
        {
            NULL,
            G_LOG_LEVEL_CRITICAL,
            "*assertion 'codecs != NULL' failed"
        },{
            ";:;",
            G_LOG_LEVEL_WARNING,
            "*spice: invalid encoder:codec value*",
        },{
            "::::",
            G_LOG_LEVEL_WARNING,
            "*spice: invalid encoder:codec value*",
        },{
            "missingcolon",
            G_LOG_LEVEL_WARNING,
            "*spice: invalid encoder:codec value*",
        },{
            ":missing_encoder",
            G_LOG_LEVEL_WARNING,
            "*spice: invalid encoder:codec value*",
        },{
            "missing_value:;",
            G_LOG_LEVEL_WARNING,
            "*spice: invalid encoder:codec value*",
        },{
            "unknown_encoder:mjpeg",
            G_LOG_LEVEL_WARNING,
            "*spice: unknown video encoder unknown_encoder",
        },{
            "spice:unknown_codec",
            G_LOG_LEVEL_WARNING,
            "*spice: unknown video codec unknown_codec",
        },
#if !defined(HAVE_GSTREAMER_1_0) && !defined(HAVE_GSTREAMER_0_10)
        {
            "gstreamer:mjpeg",
            G_LOG_LEVEL_WARNING,
            "*spice: unsupported video encoder gstreamer",
        }
#endif
    };

    SpiceServer *server = spice_server_new();

    g_assert_nonnull(server);

    for (i = 0; i < G_N_ELEMENTS(test_cases); ++i) {
        g_test_expect_message(G_LOG_DOMAIN, test_cases[i].log_level, test_cases[i].error_message);
        g_assert_cmpint(spice_server_set_video_codecs(server, test_cases[i].codecs), ==, 0);
        g_test_assert_expected_messages();
    }

    spice_server_destroy(server);
}
#endif

int main(int argc, char *argv[])
{
    g_test_init(&argc, &argv, NULL);

    g_test_add_func("/server/codecs-good", codecs_good);
#if GLIB_CHECK_VERSION(2, 34, 0)
    g_test_add_func("/server/codecs-bad", codecs_bad);
#endif

    return g_test_run();
}
