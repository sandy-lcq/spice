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
#ifndef _H_RED_PIPE_ITEM
#define _H_RED_PIPE_ITEM

#include <glib.h>

typedef struct {
    RingItem link;
    int type;

    /* private */
    int refcount;

    GDestroyNotify free_func;
} PipeItem;

void pipe_item_init_full(PipeItem *item, int type, GDestroyNotify free_func);
PipeItem *pipe_item_ref(gpointer item);
void pipe_item_unref(gpointer item);

static inline int pipe_item_is_linked(PipeItem *item)
{
    return ring_item_is_linked(&item->link);
}

static inline void pipe_item_init(PipeItem *item, int type)
{
    pipe_item_init_full(item, type, NULL);
}
#endif
