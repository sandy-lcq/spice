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

/* This is a template file to implement event loop using GLib one.
 *
 * Is implemented as a template file to avoid some linker problem.
 *
 * This file export a variable:
 *
 * SpiceCoreInterfaceInternal event_loop_core;
 *
 * You should also define some functions like:
 *
 * GMainContext *event_loop_context_from_iface(const SpiceCoreInterfaceInternal *opaque);
 * void event_loop_channel_event(int event, SpiceChannelEventInfo *info);
 */

#include "red-common.h"

struct SpiceTimer {
    GMainContext *context;
    SpiceTimerFunc func;
    void *opaque;
    GSource *source;
};

static SpiceTimer* timer_add(const SpiceCoreInterfaceInternal *iface,
                             SpiceTimerFunc func, void *opaque)
{
    SpiceTimer *timer = spice_malloc0(sizeof(SpiceTimer));

    timer->context = event_loop_context_from_iface(iface);
    timer->func = func;
    timer->opaque = opaque;

    return timer;
}

static gboolean timer_func(gpointer user_data)
{
    SpiceTimer *timer = user_data;

    timer->func(timer->opaque);
    /* timer might be free after func(), don't touch */

    return FALSE;
}

static void timer_cancel(SpiceTimer *timer)
{
    if (timer->source) {
        g_source_destroy(timer->source);
        g_source_unref(timer->source);
        timer->source = NULL;
    }
}

static void timer_start(SpiceTimer *timer, uint32_t ms)
{
    timer_cancel(timer);

    timer->source = g_timeout_source_new(ms);
    spice_assert(timer->source != NULL);

    g_source_set_callback(timer->source, timer_func, timer, NULL);

    g_source_attach(timer->source, timer->context);
}

static void timer_remove(SpiceTimer *timer)
{
    timer_cancel(timer);
    spice_assert(timer->source == NULL);
    free(timer);
}

struct SpiceWatch {
    GMainContext *context;
    void *opaque;
    GSource *source;
    GIOChannel *channel;
    SpiceWatchFunc func;
};

static GIOCondition spice_event_to_giocondition(int event_mask)
{
    GIOCondition condition = 0;

    if (event_mask & SPICE_WATCH_EVENT_READ)
        condition |= G_IO_IN;
    if (event_mask & SPICE_WATCH_EVENT_WRITE)
        condition |= G_IO_OUT;

    return condition;
}

static int giocondition_to_spice_event(GIOCondition condition)
{
    int event = 0;

    if (condition & G_IO_IN)
        event |= SPICE_WATCH_EVENT_READ;
    if (condition & G_IO_OUT)
        event |= SPICE_WATCH_EVENT_WRITE;

    return event;
}

static gboolean watch_func(GIOChannel *source, GIOCondition condition,
                           gpointer data)
{
    SpiceWatch *watch = data;
    int fd = g_io_channel_unix_get_fd(source);

    watch->func(fd, giocondition_to_spice_event(condition), watch->opaque);

    return TRUE;
}

static void watch_update_mask(SpiceWatch *watch, int event_mask)
{
    if (watch->source) {
        g_source_destroy(watch->source);
        g_source_unref(watch->source);
        watch->source = NULL;
    }

    if (!event_mask)
        return;

    watch->source = g_io_create_watch(watch->channel, spice_event_to_giocondition(event_mask));
    g_source_set_callback(watch->source, (GSourceFunc)watch_func, watch, NULL);
    g_source_attach(watch->source, watch->context);
}

static SpiceWatch *watch_add(const SpiceCoreInterfaceInternal *iface,
                             int fd, int event_mask, SpiceWatchFunc func, void *opaque)
{
    SpiceWatch *watch;

    spice_return_val_if_fail(fd != -1, NULL);
    spice_return_val_if_fail(func != NULL, NULL);

    watch = spice_malloc0(sizeof(SpiceWatch));
    watch->context = event_loop_context_from_iface(iface);
    watch->channel = g_io_channel_unix_new(fd);
    watch->func = func;
    watch->opaque = opaque;

    watch_update_mask(watch, event_mask);

    return watch;
}

static void watch_remove(SpiceWatch *watch)
{
    watch_update_mask(watch, 0);
    spice_assert(watch->source == NULL);

    g_io_channel_unref(watch->channel);
    free(watch);
}

static SpiceCoreInterfaceInternal event_loop_core = {
    .timer_add = timer_add,
    .timer_start = timer_start,
    .timer_cancel = timer_cancel,
    .timer_remove = timer_remove,

    .watch_add = watch_add,
    .watch_update_mask = watch_update_mask,
    .watch_remove = watch_remove,

    .channel_event = event_loop_channel_event
};
