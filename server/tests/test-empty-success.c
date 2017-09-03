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
#include <config.h>
#include <stdlib.h>
#include <string.h>
#include <common/log.h>
#include <spice.h>

static unsigned int watches_created = 0;
static unsigned int timers_created = 0;

static SpiceWatch *const dummy_watch = (SpiceWatch*)(uintptr_t)0xdeadbeef;
static SpiceTimer *const dummy_timer = (SpiceTimer*)(uintptr_t)0xbeefdead;

static SpiceTimer*
timer_add(SPICE_GNUC_UNUSED SpiceTimerFunc func,
          SPICE_GNUC_UNUSED void *opaque)
{
    ++timers_created;
    return dummy_timer;
}

static void
timer_start(SPICE_GNUC_UNUSED SpiceTimer *timer,
            SPICE_GNUC_UNUSED uint32_t ms)
{
    spice_assert(timer == dummy_timer);
}

static void
timer_cancel(SPICE_GNUC_UNUSED SpiceTimer *timer)
{
    spice_assert(timer == dummy_timer);
}

static void
timer_remove(SPICE_GNUC_UNUSED SpiceTimer *timer)
{
    spice_assert(timer == dummy_timer);
}

static SpiceWatch *
watch_add(SPICE_GNUC_UNUSED int fd,
          SPICE_GNUC_UNUSED int event_mask,
          SPICE_GNUC_UNUSED SpiceWatchFunc func,
          SPICE_GNUC_UNUSED void *opaque)
{
    ++watches_created;
    return dummy_watch;
}

static void
watch_update_mask(SPICE_GNUC_UNUSED SpiceWatch *watch,
                  SPICE_GNUC_UNUSED int event_mask)
{
    spice_assert(watch == dummy_watch);
}

static void
watch_remove(SPICE_GNUC_UNUSED SpiceWatch *watch)
{
    spice_assert(watch == dummy_watch);
}

static void
channel_event(SPICE_GNUC_UNUSED int event,
              SPICE_GNUC_UNUSED SpiceChannelEventInfo *info)
{
}

int main(void)
{
    SpiceServer *server = spice_server_new();
    SpiceCoreInterface core;

    memset(&core, 0, sizeof(core));
    core.base.major_version = SPICE_INTERFACE_CORE_MAJOR;
    core.timer_add = timer_add;
    core.timer_start = timer_start;
    core.timer_cancel = timer_cancel;
    core.timer_remove = timer_remove;
    core.watch_add = watch_add;
    core.watch_update_mask = watch_update_mask;
    core.watch_remove = watch_remove;
    core.channel_event = channel_event;

    spice_server_set_port(server, 5911);
    spice_server_init(server, &core);

    spice_server_destroy(server);

    // should have created some timers and watch
    spice_assert(watches_created > 0);
    spice_assert(timers_created > 0);

    return 0;
}
