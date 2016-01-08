/* -*- Mode: C; c-basic-offset: 4; indent-tabs-mode: nil -*- */
/*
   Copyright (C) 2015 Red Hat, Inc.

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

/* check statistics
 */

#include <config.h>
#include <glib.h>

typedef void test_func_t(void);

test_func_t stat_test1;
test_func_t stat_test2;
test_func_t stat_test3;
test_func_t stat_test4;

static test_func_t *test_funcs[] = {
    stat_test1,
    stat_test2,
    stat_test3,
    stat_test4,
    NULL
};

int main(void)
{
    test_func_t **func_p;

    for (func_p = test_funcs; *func_p; ++func_p) {
        (*func_p)();
    }
    return 0;
}
