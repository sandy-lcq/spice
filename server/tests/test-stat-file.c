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
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <spice.h>

#include "test-glib-compat.h"
#include "stat-file.h"

static void stat_file(void)
{
    RedStatFile *stat_file;
    StatNodeRef ref, refs[10];
    uint64_t *counter, *counters[10] SPICE_GNUC_UNUSED;
    int i;
    char *filename = NULL;
    char name[20];

    /* create */
    stat_file = stat_file_new(10);

    g_assert_nonnull(stat_file);
    g_assert_nonnull(stat_file_get_shm_name(stat_file));
    filename = g_strdup(stat_file_get_shm_name(stat_file));
    g_assert(access(filename, R_OK));

    /* fill all nodes */
    for (i = 0; i < 10; ++i) {
        sprintf(name, "node %d", i);
        ref = stat_file_add_node(stat_file, INVALID_STAT_REF, name, TRUE);
        refs[i] = ref;
        g_assert_cmpuint(ref,!=,INVALID_STAT_REF);
    }

    /* should fail */
    ref = stat_file_add_node(stat_file, INVALID_STAT_REF, "invalid", TRUE);
    g_assert_cmpuint(ref,==,INVALID_STAT_REF);

    /* we should find already present nodes */
    for (i = 0; i < 10; ++i) {
        /* the formula used here is to take nodes here and there.
         * As 17 and 10 are coprime numbers you'll get the same numbers
         * after 10 iterations */
        sprintf(name, "node %d", (i * 17 + 5) % 10);
        ref = stat_file_add_node(stat_file, INVALID_STAT_REF, name, TRUE);
        g_assert_cmpuint(ref,!=,INVALID_STAT_REF);
    }

    /* delete some nodes */
    for (i = 0; i < 6; ++i) {
        /* see above why the formula is used */
        int n = (i * 23 + 3) % 10;
        stat_file_remove_node(stat_file, refs[n]);
        refs[n] = INVALID_STAT_REF;
    }

    /* now there should be some place for some counters */
    for (i = 0; i < 6; ++i) {
        sprintf(name, "counter %d", i);
        counter = stat_file_add_counter(stat_file, INVALID_STAT_REF, name, TRUE);
        counters[i] = counter;
        g_assert_nonnull(counter);
    }
    counter = stat_file_add_counter(stat_file, INVALID_STAT_REF, "invalid", TRUE);
    g_assert_null(counter);

    stat_file_unlink(stat_file);
    g_assert_null(stat_file_get_shm_name(stat_file));
    g_assert_cmpint(access(filename, F_OK),==,-1);
    g_free(filename);

    stat_file_free(stat_file);
}

/* make sure first node is node 0 */
static void stat_file_start(void)
{
    RedStatFile *stat_file;
    StatNodeRef ref;

    /* create */
    stat_file = stat_file_new(10);
    g_assert_nonnull(stat_file);

    ref = stat_file_add_node(stat_file, INVALID_STAT_REF, "ZERO", TRUE);
    g_assert_cmpuint(ref,==,0);

    stat_file_unlink(stat_file);
    stat_file_free(stat_file);
}


int main(int argc, char *argv[])
{
    g_test_init(&argc, &argv, NULL);

    g_test_add_func("/server/stat-file", stat_file);
    g_test_add_func("/server/stat-file-start", stat_file_start);

    return g_test_run();
}
