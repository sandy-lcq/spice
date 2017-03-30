/*
   Copyright (C) 2009 Red Hat, Inc.

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

#ifndef RED_COMMON_H_
#define RED_COMMON_H_

#include <errno.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <glib.h>
#include <spice/macros.h>
#include <common/log.h>
#include <common/lz_common.h>
#include <common/marshaller.h>
#include <common/mem.h>
#include <common/messages.h>
#include <common/ring.h>
#include <common/spice_common.h>
#include <common/draw.h>
#include <common/verify.h>

#include "spice.h"
#include "utils.h"

#define SPICE_UPCAST(type, ptr) \
    (verify_expr(SPICE_OFFSETOF(type, base) == 0,SPICE_CONTAINEROF(ptr, type, base)))

typedef struct SpiceCoreInterfaceInternal SpiceCoreInterfaceInternal;

struct SpiceCoreInterfaceInternal {
    SpiceTimer *(*timer_add)(const SpiceCoreInterfaceInternal *iface, SpiceTimerFunc func, void *opaque);
    void (*timer_start)(const SpiceCoreInterfaceInternal *iface, SpiceTimer *timer, uint32_t ms);
    void (*timer_cancel)(const SpiceCoreInterfaceInternal *iface, SpiceTimer *timer);
    void (*timer_remove)(const SpiceCoreInterfaceInternal *iface, SpiceTimer *timer);

    SpiceWatch *(*watch_add)(const SpiceCoreInterfaceInternal *iface, int fd, int event_mask, SpiceWatchFunc func, void *opaque);
    void (*watch_update_mask)(const SpiceCoreInterfaceInternal *iface, SpiceWatch *watch, int event_mask);
    void (*watch_remove)(const SpiceCoreInterfaceInternal *iface, SpiceWatch *watch);

    void (*channel_event)(const SpiceCoreInterfaceInternal *iface, int event, SpiceChannelEventInfo *info);

    /* This structure is an adapter that allows us to use the same API to
     * implement the core interface in a couple different ways. The first
     * method is to use a public SpiceCoreInterface provided to us by the
     * library user (for example, qemu). The second method is to implement the
     * core interface functions using the glib event loop. In order to avoid
     * global variables, each method needs to store additional data in this
     * adapter structure. Instead of using a generic void* data parameter, we
     * provide a bit more type-safety by using a union to store the type of
     * data needed by each implementation.
     */
    union {
        GMainContext *main_context;
        SpiceCoreInterface *public_interface;
    };
};

extern const SpiceCoreInterfaceInternal event_loop_core;

typedef struct RedsState RedsState;

typedef struct GListIter {
    GList *link;
    GList *next;
} GListIter;

#define GLIST_FOREACH_GENERIC(_list, _iter, _type, _data, _dir) \
    for (_iter.link = _list; \
        (_data = (_type *) (_iter.link ? _iter.link->data : NULL), \
         _iter.next = (_iter.link ? _iter.link->_dir : NULL), \
         _iter.link) != NULL; \
         _iter.link = _iter.next)

#define GLIST_FOREACH(_list, _iter, _type, _data) \
    GLIST_FOREACH_GENERIC(_list, _iter, _type, _data, next)

#define GLIST_FOREACH_REVERSED(_list, _iter, _type, _data) \
    GLIST_FOREACH_GENERIC(_list, _iter, _type, _data, prev)

#endif /* RED_COMMON_H_ */
