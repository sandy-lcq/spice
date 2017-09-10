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

#ifndef GLIB_COMPAT_H_
#define GLIB_COMPAT_H_

#include <glib.h>

#if !GLIB_CHECK_VERSION(2,34,0)
#define g_clear_pointer(pp, destroy) \
  G_STMT_START {                                                               \
    SPICE_VERIFY (sizeof *(pp) == sizeof (gpointer));                          \
    /* Only one access, please */                                              \
    gpointer *_pp = (gpointer *) (pp);                                         \
    gpointer _p;                                                               \
    /* This assignment is needed to avoid a gcc warning */                     \
    GDestroyNotify _destroy = (GDestroyNotify) (destroy);                      \
                                                                               \
    _p = *_pp;                                                                 \
    if (_p)                                                                    \
      {                                                                        \
        *_pp = NULL;                                                           \
        _destroy (_p);                                                         \
      }                                                                        \
  } G_STMT_END
#endif

#if !GLIB_CHECK_VERSION(2,32,0)
static inline void
g_queue_free_full(GQueue *queue, GDestroyNotify free_func)
{
    /* quite hack cast but work with standard C call convention */
    g_queue_foreach(queue, (GFunc) free_func, NULL);
    g_queue_clear(queue);
}
#endif

#if !GLIB_CHECK_VERSION(2,30,0)
static inline gboolean
g_queue_remove_boolean(GQueue *queue, gconstpointer data)
{
    GList *link = g_queue_find(queue, data);
    if (!link) {
        return FALSE;
    }
    g_queue_unlink(queue, link);
    return TRUE;
}
#define g_queue_remove g_queue_remove_boolean
#endif

#endif /* GLIB_COMPAT_H_ */
