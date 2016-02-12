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
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <pthread.h>
#include <sys/socket.h>
#include <inttypes.h>

#include <spice/qxl_dev.h>
#include "common/quic.h"

#include "spice.h"
#include "red-worker.h"
#include "sw-canvas.h"
#include "reds.h"
#include "dispatcher.h"
#include "red-parse-qxl.h"

#include "red-qxl.h"


struct AsyncCommand {
    RedWorkerMessage message;
    uint64_t cookie;
};

struct QXLState {
    QXLWorker base;
    QXLInstance *qxl;
    Dispatcher dispatcher;
    uint32_t pending;
    int primary_active;
    int x_res;
    int y_res;
    int use_hardware_cursor;
    QXLDevSurfaceCreate surface_create;
    unsigned int max_monitors;
    RedsState *reds;

    pthread_mutex_t scanout_mutex;
    SpiceMsgDisplayGlScanoutUnix scanout;
    struct AsyncCommand *gl_draw_async;
};

static int red_qxl_check_qxl_version(QXLState *rq, int major, int minor)
{
    int qxl_major = qxl_get_interface(rq->qxl)->base.major_version;
    int qxl_minor = qxl_get_interface(rq->qxl)->base.minor_version;

    return ((qxl_major > major) ||
            ((qxl_major == major) && (qxl_minor >= minor)));
}

static void red_qxl_set_display_peer(RedChannel *channel, RedClient *client,
                                     RedsStream *stream, int migration,
                                     int num_common_caps, uint32_t *common_caps, int num_caps,
                                     uint32_t *caps)
{
    RedWorkerMessageDisplayConnect payload = {0,};
    QXLState *qxl_state;

    spice_debug("%s", "");
    qxl_state = (QXLState *)channel->data;
    payload.client = client;
    payload.stream = stream;
    payload.migration = migration;
    payload.num_common_caps = num_common_caps;
    payload.common_caps = spice_malloc(sizeof(uint32_t)*num_common_caps);
    payload.num_caps = num_caps;
    payload.caps = spice_malloc(sizeof(uint32_t)*num_caps);

    memcpy(payload.common_caps, common_caps, sizeof(uint32_t)*num_common_caps);
    memcpy(payload.caps, caps, sizeof(uint32_t)*num_caps);

    dispatcher_send_message(&qxl_state->dispatcher,
                            RED_WORKER_MESSAGE_DISPLAY_CONNECT,
                            &payload);
}

static void red_qxl_disconnect_display_peer(RedChannelClient *rcc)
{
    RedWorkerMessageDisplayDisconnect payload;
    QXLState *qxl_state;

    if (!rcc->channel) {
        return;
    }

    qxl_state = (QXLState *)rcc->channel->data;

    spice_printerr("");
    payload.rcc = rcc;

    // TODO: we turned it to be sync, due to client_destroy . Should we support async? - for this we will need ref count
    // for channels
    dispatcher_send_message(&qxl_state->dispatcher,
                            RED_WORKER_MESSAGE_DISPLAY_DISCONNECT,
                            &payload);
}

static void red_qxl_display_migrate(RedChannelClient *rcc)
{
    RedWorkerMessageDisplayMigrate payload;
    QXLState *qxl_state;
    if (!rcc->channel) {
        return;
    }
    qxl_state = (QXLState *)rcc->channel->data;
    spice_printerr("channel type %u id %u", rcc->channel->type, rcc->channel->id);
    payload.rcc = rcc;
    dispatcher_send_message(&qxl_state->dispatcher,
                            RED_WORKER_MESSAGE_DISPLAY_MIGRATE,
                            &payload);
}

static void red_qxl_set_cursor_peer(RedChannel *channel, RedClient *client, RedsStream *stream,
                                    int migration, int num_common_caps,
                                    uint32_t *common_caps, int num_caps,
                                    uint32_t *caps)
{
    RedWorkerMessageCursorConnect payload = {0,};
    QXLState *qxl_state = (QXLState *)channel->data;
    spice_printerr("");
    payload.client = client;
    payload.stream = stream;
    payload.migration = migration;
    payload.num_common_caps = num_common_caps;
    payload.common_caps = spice_malloc(sizeof(uint32_t)*num_common_caps);
    payload.num_caps = num_caps;
    payload.caps = spice_malloc(sizeof(uint32_t)*num_caps);

    memcpy(payload.common_caps, common_caps, sizeof(uint32_t)*num_common_caps);
    memcpy(payload.caps, caps, sizeof(uint32_t)*num_caps);

    dispatcher_send_message(&qxl_state->dispatcher,
                            RED_WORKER_MESSAGE_CURSOR_CONNECT,
                            &payload);
}

static void red_qxl_disconnect_cursor_peer(RedChannelClient *rcc)
{
    RedWorkerMessageCursorDisconnect payload;
    QXLState *qxl_state;

    if (!rcc->channel) {
        return;
    }

    qxl_state = (QXLState *)rcc->channel->data;
    spice_printerr("");
    payload.rcc = rcc;

    dispatcher_send_message(&qxl_state->dispatcher,
                            RED_WORKER_MESSAGE_CURSOR_DISCONNECT,
                            &payload);
}

static void red_qxl_cursor_migrate(RedChannelClient *rcc)
{
    RedWorkerMessageCursorMigrate payload;
    QXLState *qxl_state;

    if (!rcc->channel) {
        return;
    }
    qxl_state = (QXLState *)rcc->channel->data;
    spice_printerr("channel type %u id %u", rcc->channel->type, rcc->channel->id);
    payload.rcc = rcc;
    dispatcher_send_message(&qxl_state->dispatcher,
                            RED_WORKER_MESSAGE_CURSOR_MIGRATE,
                            &payload);
}

static void red_qxl_update_area(QXLState *qxl_state, uint32_t surface_id,
                                QXLRect *qxl_area, QXLRect *qxl_dirty_rects,
                                uint32_t num_dirty_rects, uint32_t clear_dirty_region)
{
    RedWorkerMessageUpdate payload = {0,};

    payload.surface_id = surface_id;
    payload.qxl_area = qxl_area;
    payload.qxl_dirty_rects = qxl_dirty_rects;
    payload.num_dirty_rects = num_dirty_rects;
    payload.clear_dirty_region = clear_dirty_region;
    dispatcher_send_message(&qxl_state->dispatcher,
                            RED_WORKER_MESSAGE_UPDATE,
                            &payload);
}

gboolean red_qxl_use_client_monitors_config(QXLState *qxl_state)
{
    return (red_qxl_check_qxl_version(qxl_state, 3, 3) &&
        qxl_get_interface(qxl_state->qxl)->client_monitors_config &&
        qxl_get_interface(qxl_state->qxl)->client_monitors_config(qxl_state->qxl, NULL));
}

gboolean red_qxl_client_monitors_config(QXLState *qxl_state,
                                        VDAgentMonitorsConfig *monitors_config)
{
    return (qxl_get_interface(qxl_state->qxl)->client_monitors_config &&
        qxl_get_interface(qxl_state->qxl)->client_monitors_config(qxl_state->qxl,
                                                                  monitors_config));
}

static AsyncCommand *async_command_alloc(QXLState *qxl_state,
                                         RedWorkerMessage message,
                                         uint64_t cookie)
{
    AsyncCommand *async_command = spice_new0(AsyncCommand, 1);

    async_command->cookie = cookie;
    async_command->message = message;

    spice_debug("%p", async_command);
    return async_command;
}

static void red_qxl_update_area_async(QXLState *qxl_state,
                                      uint32_t surface_id,
                                      QXLRect *qxl_area,
                                      uint32_t clear_dirty_region,
                                      uint64_t cookie)
{
    RedWorkerMessage message = RED_WORKER_MESSAGE_UPDATE_ASYNC;
    RedWorkerMessageUpdateAsync payload;

    payload.base.cmd = async_command_alloc(qxl_state, message, cookie);
    payload.surface_id = surface_id;
    payload.qxl_area = *qxl_area;
    payload.clear_dirty_region = clear_dirty_region;
    dispatcher_send_message(&qxl_state->dispatcher,
                            message,
                            &payload);
}

static void qxl_worker_update_area(QXLWorker *qxl_worker, uint32_t surface_id,
                                   QXLRect *qxl_area, QXLRect *qxl_dirty_rects,
                                   uint32_t num_dirty_rects, uint32_t clear_dirty_region)
{
    red_qxl_update_area((QXLState*)qxl_worker, surface_id, qxl_area,
                        qxl_dirty_rects, num_dirty_rects, clear_dirty_region);
}

static void red_qxl_add_memslot(QXLState *qxl_state, QXLDevMemSlot *mem_slot)
{
    RedWorkerMessageAddMemslot payload;

    payload.mem_slot = *mem_slot;
    dispatcher_send_message(&qxl_state->dispatcher,
                            RED_WORKER_MESSAGE_ADD_MEMSLOT,
                            &payload);
}

static void qxl_worker_add_memslot(QXLWorker *qxl_worker, QXLDevMemSlot *mem_slot)
{
    red_qxl_add_memslot((QXLState*)qxl_worker, mem_slot);
}

static void red_qxl_add_memslot_async(QXLState *qxl_state, QXLDevMemSlot *mem_slot, uint64_t cookie)
{
    RedWorkerMessageAddMemslotAsync payload;
    RedWorkerMessage message = RED_WORKER_MESSAGE_ADD_MEMSLOT_ASYNC;

    payload.base.cmd = async_command_alloc(qxl_state, message, cookie);
    payload.mem_slot = *mem_slot;
    dispatcher_send_message(&qxl_state->dispatcher, message, &payload);
}

static void red_qxl_del_memslot(QXLState *qxl_state, uint32_t slot_group_id, uint32_t slot_id)
{
    RedWorkerMessageDelMemslot payload;
    RedWorkerMessage message = RED_WORKER_MESSAGE_DEL_MEMSLOT;

    payload.slot_group_id = slot_group_id;
    payload.slot_id = slot_id;
    dispatcher_send_message(&qxl_state->dispatcher, message, &payload);
}

static void qxl_worker_del_memslot(QXLWorker *qxl_worker, uint32_t slot_group_id, uint32_t slot_id)
{
    red_qxl_del_memslot((QXLState*)qxl_worker, slot_group_id, slot_id);
}

static void red_qxl_destroy_surfaces(QXLState *qxl_state)
{
    RedWorkerMessageDestroySurfaces payload;

    dispatcher_send_message(&qxl_state->dispatcher,
                            RED_WORKER_MESSAGE_DESTROY_SURFACES,
                            &payload);
}

static void qxl_worker_destroy_surfaces(QXLWorker *qxl_worker)
{
    red_qxl_destroy_surfaces((QXLState*)qxl_worker);
}

static void red_qxl_destroy_surfaces_async(QXLState *qxl_state, uint64_t cookie)
{
    RedWorkerMessageDestroySurfacesAsync payload;
    RedWorkerMessage message = RED_WORKER_MESSAGE_DESTROY_SURFACES_ASYNC;

    payload.base.cmd = async_command_alloc(qxl_state, message, cookie);
    dispatcher_send_message(&qxl_state->dispatcher, message, &payload);
}

static void red_qxl_destroy_primary_surface_complete(QXLState *qxl_state)
{
    qxl_state->x_res = 0;
    qxl_state->y_res = 0;
    qxl_state->use_hardware_cursor = FALSE;
    qxl_state->primary_active = FALSE;

    reds_update_client_mouse_allowed(qxl_state->reds);
}

static void
red_qxl_destroy_primary_surface_sync(QXLState *qxl_state,
                                     uint32_t surface_id)
{
    RedWorkerMessageDestroyPrimarySurface payload;
    payload.surface_id = surface_id;
    dispatcher_send_message(&qxl_state->dispatcher,
                            RED_WORKER_MESSAGE_DESTROY_PRIMARY_SURFACE,
                            &payload);
    red_qxl_destroy_primary_surface_complete(qxl_state);
}

static void
red_qxl_destroy_primary_surface_async(QXLState *qxl_state,
                                      uint32_t surface_id, uint64_t cookie)
{
    RedWorkerMessageDestroyPrimarySurfaceAsync payload;
    RedWorkerMessage message = RED_WORKER_MESSAGE_DESTROY_PRIMARY_SURFACE_ASYNC;

    payload.base.cmd = async_command_alloc(qxl_state, message, cookie);
    payload.surface_id = surface_id;
    dispatcher_send_message(&qxl_state->dispatcher, message, &payload);
}

static void
red_qxl_destroy_primary_surface(QXLState *qxl_state,
                                uint32_t surface_id, int async, uint64_t cookie)
{
    if (async) {
        red_qxl_destroy_primary_surface_async(qxl_state, surface_id, cookie);
    } else {
        red_qxl_destroy_primary_surface_sync(qxl_state, surface_id);
    }
}

static void qxl_worker_destroy_primary_surface(QXLWorker *qxl_worker, uint32_t surface_id)
{
    red_qxl_destroy_primary_surface((QXLState*)qxl_worker, surface_id, 0, 0);
}

static void red_qxl_create_primary_surface_complete(QXLState *qxl_state)
{
    QXLDevSurfaceCreate *surface = &qxl_state->surface_create;

    qxl_state->x_res = surface->width;
    qxl_state->y_res = surface->height;
    qxl_state->use_hardware_cursor = surface->mouse_mode;
    qxl_state->primary_active = TRUE;

    reds_update_client_mouse_allowed(qxl_state->reds);
    memset(&qxl_state->surface_create, 0, sizeof(QXLDevSurfaceCreate));
}

static void
red_qxl_create_primary_surface_async(QXLState *qxl_state, uint32_t surface_id,
                                     QXLDevSurfaceCreate *surface, uint64_t cookie)
{
    RedWorkerMessageCreatePrimarySurfaceAsync payload;
    RedWorkerMessage message = RED_WORKER_MESSAGE_CREATE_PRIMARY_SURFACE_ASYNC;

    qxl_state->surface_create = *surface;
    payload.base.cmd = async_command_alloc(qxl_state, message, cookie);
    payload.surface_id = surface_id;
    payload.surface = *surface;
    dispatcher_send_message(&qxl_state->dispatcher, message, &payload);
}

static void
red_qxl_create_primary_surface_sync(QXLState *qxl_state, uint32_t surface_id,
                                    QXLDevSurfaceCreate *surface)
{
    RedWorkerMessageCreatePrimarySurface payload = {0,};

    qxl_state->surface_create = *surface;
    payload.surface_id = surface_id;
    payload.surface = *surface;
    dispatcher_send_message(&qxl_state->dispatcher,
                            RED_WORKER_MESSAGE_CREATE_PRIMARY_SURFACE,
                            &payload);
    red_qxl_create_primary_surface_complete(qxl_state);
}

static void
red_qxl_create_primary_surface(QXLState *qxl_state, uint32_t surface_id,
                               QXLDevSurfaceCreate *surface, int async, uint64_t cookie)
{
    if (async) {
        red_qxl_create_primary_surface_async(qxl_state, surface_id, surface, cookie);
    } else {
        red_qxl_create_primary_surface_sync(qxl_state, surface_id, surface);
    }
}

static void qxl_worker_create_primary_surface(QXLWorker *qxl_worker, uint32_t surface_id,
                                      QXLDevSurfaceCreate *surface)
{
    red_qxl_create_primary_surface((QXLState*)qxl_worker, surface_id, surface, 0, 0);
}

static void red_qxl_reset_image_cache(QXLState *qxl_state)
{
    RedWorkerMessageResetImageCache payload;

    dispatcher_send_message(&qxl_state->dispatcher,
                            RED_WORKER_MESSAGE_RESET_IMAGE_CACHE,
                            &payload);
}

static void qxl_worker_reset_image_cache(QXLWorker *qxl_worker)
{
    red_qxl_reset_image_cache((QXLState*)qxl_worker);
}

static void red_qxl_reset_cursor(QXLState *qxl_state)
{
    RedWorkerMessageResetCursor payload;

    dispatcher_send_message(&qxl_state->dispatcher,
                            RED_WORKER_MESSAGE_RESET_CURSOR,
                            &payload);
}

static void qxl_worker_reset_cursor(QXLWorker *qxl_worker)
{
    red_qxl_reset_cursor((QXLState*)qxl_worker);
}

static void red_qxl_destroy_surface_wait_sync(QXLState *qxl_state,
                                              uint32_t surface_id)
{
    RedWorkerMessageDestroySurfaceWait payload;

    payload.surface_id = surface_id;
    dispatcher_send_message(&qxl_state->dispatcher,
                            RED_WORKER_MESSAGE_DESTROY_SURFACE_WAIT,
                            &payload);
}

static void red_qxl_destroy_surface_wait_async(QXLState *qxl_state,
                                               uint32_t surface_id,
                                               uint64_t cookie)
{
    RedWorkerMessageDestroySurfaceWaitAsync payload;
    RedWorkerMessage message = RED_WORKER_MESSAGE_DESTROY_SURFACE_WAIT_ASYNC;

    payload.base.cmd = async_command_alloc(qxl_state, message, cookie);
    payload.surface_id = surface_id;
    dispatcher_send_message(&qxl_state->dispatcher, message, &payload);
}

static void red_qxl_destroy_surface_wait(QXLState *qxl_state,
                                         uint32_t surface_id,
                                         int async, uint64_t cookie)
{
    if (async) {
        red_qxl_destroy_surface_wait_async(qxl_state, surface_id, cookie);
    } else {
        red_qxl_destroy_surface_wait_sync(qxl_state, surface_id);
    }
}

static void qxl_worker_destroy_surface_wait(QXLWorker *qxl_worker, uint32_t surface_id)
{
    red_qxl_destroy_surface_wait((QXLState*)qxl_worker, surface_id, 0, 0);
}

static void red_qxl_reset_memslots(QXLState *qxl_state)
{
    RedWorkerMessageResetMemslots payload;

    dispatcher_send_message(&qxl_state->dispatcher,
                            RED_WORKER_MESSAGE_RESET_MEMSLOTS,
                            &payload);
}

static void qxl_worker_reset_memslots(QXLWorker *qxl_worker)
{
    red_qxl_reset_memslots((QXLState*)qxl_worker);
}

static bool red_qxl_set_pending(QXLState *qxl_state, int pending)
{
    // this is not atomic but is not an issue
    if (test_bit(pending, qxl_state->pending)) {
        return TRUE;
    }

    set_bit(pending, &qxl_state->pending);
    return FALSE;
}

static void red_qxl_wakeup(QXLState *qxl_state)
{
    RedWorkerMessageWakeup payload;

    if (red_qxl_set_pending(qxl_state, RED_DISPATCHER_PENDING_WAKEUP))
        return;

    dispatcher_send_message(&qxl_state->dispatcher,
                            RED_WORKER_MESSAGE_WAKEUP,
                            &payload);
}

static void qxl_worker_wakeup(QXLWorker *qxl_worker)
{
    red_qxl_wakeup((QXLState*)qxl_worker);
}

static void red_qxl_oom(QXLState *qxl_state)
{
    RedWorkerMessageOom payload;

    if (red_qxl_set_pending(qxl_state, RED_DISPATCHER_PENDING_OOM))
        return;

    dispatcher_send_message(&qxl_state->dispatcher,
                            RED_WORKER_MESSAGE_OOM,
                            &payload);
}

static void qxl_worker_oom(QXLWorker *qxl_worker)
{
    red_qxl_oom((QXLState*)qxl_worker);
}

void red_qxl_start(QXLState *qxl_state)
{
    RedWorkerMessageStart payload;

    dispatcher_send_message(&qxl_state->dispatcher,
                            RED_WORKER_MESSAGE_START,
                            &payload);
}

static void qxl_worker_start(QXLWorker *qxl_worker)
{
    red_qxl_start((QXLState*)qxl_worker);
}

static void red_qxl_flush_surfaces_async(QXLState *qxl_state, uint64_t cookie)
{
    RedWorkerMessageFlushSurfacesAsync payload;
    RedWorkerMessage message = RED_WORKER_MESSAGE_FLUSH_SURFACES_ASYNC;

    payload.base.cmd = async_command_alloc(qxl_state, message, cookie);
    dispatcher_send_message(&qxl_state->dispatcher, message, &payload);
}

static void red_qxl_monitors_config_async(QXLState *qxl_state,
                                          QXLPHYSICAL monitors_config,
                                          int group_id,
                                          uint64_t cookie)
{
    RedWorkerMessageMonitorsConfigAsync payload;
    RedWorkerMessage message = RED_WORKER_MESSAGE_MONITORS_CONFIG_ASYNC;

    payload.base.cmd = async_command_alloc(qxl_state, message, cookie);
    payload.monitors_config = monitors_config;
    payload.group_id = group_id;
    payload.max_monitors = qxl_state->max_monitors;

    dispatcher_send_message(&qxl_state->dispatcher, message, &payload);
}

static void red_qxl_driver_unload(QXLState *qxl_state)
{
    RedWorkerMessageDriverUnload payload;

    dispatcher_send_message(&qxl_state->dispatcher,
                            RED_WORKER_MESSAGE_DRIVER_UNLOAD,
                            &payload);
}

void red_qxl_stop(QXLState *qxl_state)
{
    RedWorkerMessageStop payload;

    dispatcher_send_message(&qxl_state->dispatcher,
                            RED_WORKER_MESSAGE_STOP,
                            &payload);
}

static void qxl_worker_stop(QXLWorker *qxl_worker)
{
    red_qxl_stop((QXLState*)qxl_worker);
}

static void red_qxl_loadvm_commands(QXLState *qxl_state,
                                    struct QXLCommandExt *ext,
                                    uint32_t count)
{
    RedWorkerMessageLoadvmCommands payload;

    spice_printerr("");
    payload.count = count;
    payload.ext = ext;
    dispatcher_send_message(&qxl_state->dispatcher,
                            RED_WORKER_MESSAGE_LOADVM_COMMANDS,
                            &payload);
}

static void qxl_worker_loadvm_commands(QXLWorker *qxl_worker,
                                       struct QXLCommandExt *ext,
                                       uint32_t count)
{
    red_qxl_loadvm_commands((QXLState*)qxl_worker, ext, count);
}

void red_qxl_set_mm_time(QXLState *qxl_state, uint32_t mm_time)
{
    qxl_get_interface(qxl_state->qxl)->set_mm_time(qxl_state->qxl, mm_time);
}

void red_qxl_attach_worker(QXLState *qxl_state)
{
    QXLInstance *qxl = qxl_state->qxl;
    qxl_get_interface(qxl_state->qxl)->attache_worker(qxl, &qxl_state->base);
}

void red_qxl_set_compression_level(QXLState *qxl_state, int level)
{
    qxl_get_interface(qxl_state->qxl)->set_compression_level(qxl_state->qxl, level);
}

uint32_t red_qxl_get_ram_size(QXLState *qxl_state)
{
    QXLDevInitInfo qxl_info;
    qxl_get_interface(qxl_state->qxl)->get_init_info(qxl_state->qxl, &qxl_info);
    return qxl_info.qxl_ram_size;
}

SPICE_GNUC_VISIBLE
void spice_qxl_wakeup(QXLInstance *instance)
{
    red_qxl_wakeup(instance->st);
}

SPICE_GNUC_VISIBLE
void spice_qxl_oom(QXLInstance *instance)
{
    red_qxl_oom(instance->st);
}

SPICE_GNUC_VISIBLE
void spice_qxl_start(QXLInstance *instance)
{
    red_qxl_start(instance->st);
}

SPICE_GNUC_VISIBLE
void spice_qxl_stop(QXLInstance *instance)
{
    red_qxl_stop(instance->st);
}

SPICE_GNUC_VISIBLE
void spice_qxl_update_area(QXLInstance *instance, uint32_t surface_id,
                    struct QXLRect *area, struct QXLRect *dirty_rects,
                    uint32_t num_dirty_rects, uint32_t clear_dirty_region)
{
    red_qxl_update_area(instance->st, surface_id, area, dirty_rects,
                        num_dirty_rects, clear_dirty_region);
}

SPICE_GNUC_VISIBLE
void spice_qxl_add_memslot(QXLInstance *instance, QXLDevMemSlot *slot)
{
    red_qxl_add_memslot(instance->st, slot);
}

SPICE_GNUC_VISIBLE
void spice_qxl_del_memslot(QXLInstance *instance, uint32_t slot_group_id, uint32_t slot_id)
{
    red_qxl_del_memslot(instance->st, slot_group_id, slot_id);
}

SPICE_GNUC_VISIBLE
void spice_qxl_reset_memslots(QXLInstance *instance)
{
    red_qxl_reset_memslots(instance->st);
}

SPICE_GNUC_VISIBLE
void spice_qxl_destroy_surfaces(QXLInstance *instance)
{
    red_qxl_destroy_surfaces(instance->st);
}

SPICE_GNUC_VISIBLE
void spice_qxl_destroy_primary_surface(QXLInstance *instance, uint32_t surface_id)
{
    red_qxl_destroy_primary_surface(instance->st, surface_id, 0, 0);
}

SPICE_GNUC_VISIBLE
void spice_qxl_create_primary_surface(QXLInstance *instance, uint32_t surface_id,
                                QXLDevSurfaceCreate *surface)
{
    red_qxl_create_primary_surface(instance->st, surface_id, surface, 0, 0);
}

SPICE_GNUC_VISIBLE
void spice_qxl_reset_image_cache(QXLInstance *instance)
{
    red_qxl_reset_image_cache(instance->st);
}

SPICE_GNUC_VISIBLE
void spice_qxl_reset_cursor(QXLInstance *instance)
{
    red_qxl_reset_cursor(instance->st);
}

SPICE_GNUC_VISIBLE
void spice_qxl_destroy_surface_wait(QXLInstance *instance, uint32_t surface_id)
{
    red_qxl_destroy_surface_wait(instance->st, surface_id, 0, 0);
}

SPICE_GNUC_VISIBLE
void spice_qxl_loadvm_commands(QXLInstance *instance, struct QXLCommandExt *ext, uint32_t count)
{
    red_qxl_loadvm_commands(instance->st, ext, count);
}

SPICE_GNUC_VISIBLE
void spice_qxl_update_area_async(QXLInstance *instance, uint32_t surface_id, QXLRect *qxl_area,
                                 uint32_t clear_dirty_region, uint64_t cookie)
{
    red_qxl_update_area_async(instance->st, surface_id, qxl_area,
                                     clear_dirty_region, cookie);
}

SPICE_GNUC_VISIBLE
void spice_qxl_add_memslot_async(QXLInstance *instance, QXLDevMemSlot *slot, uint64_t cookie)
{
    red_qxl_add_memslot_async(instance->st, slot, cookie);
}

SPICE_GNUC_VISIBLE
void spice_qxl_destroy_surfaces_async(QXLInstance *instance, uint64_t cookie)
{
    red_qxl_destroy_surfaces_async(instance->st, cookie);
}

SPICE_GNUC_VISIBLE
void spice_qxl_destroy_primary_surface_async(QXLInstance *instance, uint32_t surface_id, uint64_t cookie)
{
    red_qxl_destroy_primary_surface(instance->st, surface_id, 1, cookie);
}

SPICE_GNUC_VISIBLE
void spice_qxl_create_primary_surface_async(QXLInstance *instance, uint32_t surface_id,
                                QXLDevSurfaceCreate *surface, uint64_t cookie)
{
    red_qxl_create_primary_surface(instance->st, surface_id, surface, 1, cookie);
}

SPICE_GNUC_VISIBLE
void spice_qxl_destroy_surface_async(QXLInstance *instance, uint32_t surface_id, uint64_t cookie)
{
    red_qxl_destroy_surface_wait(instance->st, surface_id, 1, cookie);
}

SPICE_GNUC_VISIBLE
void spice_qxl_flush_surfaces_async(QXLInstance *instance, uint64_t cookie)
{
    red_qxl_flush_surfaces_async(instance->st, cookie);
}

SPICE_GNUC_VISIBLE
void spice_qxl_monitors_config_async(QXLInstance *instance, QXLPHYSICAL monitors_config,
                                     int group_id, uint64_t cookie)
{
    red_qxl_monitors_config_async(instance->st, monitors_config, group_id, cookie);
}

SPICE_GNUC_VISIBLE
void spice_qxl_set_max_monitors(QXLInstance *instance, unsigned int max_monitors)
{
    instance->st->max_monitors = MAX(1u, max_monitors);
}

SPICE_GNUC_VISIBLE
void spice_qxl_driver_unload(QXLInstance *instance)
{
    red_qxl_driver_unload(instance->st);
}

SpiceMsgDisplayGlScanoutUnix *red_qxl_get_gl_scanout(QXLState *qxl_state)
{
    pthread_mutex_lock(&qxl_state->scanout_mutex);
    if (qxl_state->scanout.drm_dma_buf_fd >= 0) {
        return &qxl_state->scanout;
    }
    pthread_mutex_unlock(&qxl_state->scanout_mutex);
    return NULL;
}

void red_qxl_put_gl_scanout(QXLState *qxl_state, SpiceMsgDisplayGlScanoutUnix *scanout)
{
    if (scanout) {
        pthread_mutex_unlock(&qxl_state->scanout_mutex);
    }
}

SPICE_GNUC_VISIBLE
void spice_qxl_gl_scanout(QXLInstance *qxl,
                          int fd,
                          uint32_t width, uint32_t height,
                          uint32_t stride, uint32_t format,
                          int y_0_top)
{
    spice_return_if_fail(qxl != NULL);

    QXLState *qxl_state = qxl->st;
    spice_return_if_fail(qxl_state->gl_draw_async == NULL);

    pthread_mutex_lock(&qxl_state->scanout_mutex);

    if (qxl_state->scanout.drm_dma_buf_fd != -1) {
        close(qxl_state->scanout.drm_dma_buf_fd);
    }

    qxl_state->scanout = (SpiceMsgDisplayGlScanoutUnix) {
        .flags = y_0_top ? SPICE_GL_SCANOUT_FLAGS_Y0TOP : 0,
        .drm_dma_buf_fd = fd,
        .width = width,
        .height = height,
        .stride = stride,
        .drm_fourcc_format = format
    };

    pthread_mutex_unlock(&qxl_state->scanout_mutex);

    /* FIXME: find a way to coallesce all pending SCANOUTs */
    dispatcher_send_message(&qxl_state->dispatcher,
                            RED_WORKER_MESSAGE_GL_SCANOUT, NULL);
}

SPICE_GNUC_VISIBLE
void spice_qxl_gl_draw_async(QXLInstance *qxl,
                             uint32_t x, uint32_t y,
                             uint32_t w, uint32_t h,
                             uint64_t cookie)
{
    QXLState *qxl_state;
    RedWorkerMessage message = RED_WORKER_MESSAGE_GL_DRAW_ASYNC;
    SpiceMsgDisplayGlDraw draw = {
        .x = x,
        .y = y,
        .w = w,
        .h = h
    };

    spice_return_if_fail(qxl != NULL);
    qxl_state = qxl->st;
    spice_return_if_fail(qxl_state->scanout.drm_dma_buf_fd != -1);
    spice_return_if_fail(qxl_state->gl_draw_async == NULL);

    qxl_state->gl_draw_async = async_command_alloc(qxl_state, message, cookie);
    dispatcher_send_message(&qxl_state->dispatcher, message, &draw);
}

void red_qxl_async_complete(QXLState *qxl_state,
                            AsyncCommand *async_command)
{
    spice_debug("%p: cookie %" PRId64, async_command, async_command->cookie);
    switch (async_command->message) {
    case RED_WORKER_MESSAGE_UPDATE_ASYNC:
    case RED_WORKER_MESSAGE_ADD_MEMSLOT_ASYNC:
    case RED_WORKER_MESSAGE_DESTROY_SURFACES_ASYNC:
    case RED_WORKER_MESSAGE_DESTROY_SURFACE_WAIT_ASYNC:
    case RED_WORKER_MESSAGE_FLUSH_SURFACES_ASYNC:
    case RED_WORKER_MESSAGE_MONITORS_CONFIG_ASYNC:
    case RED_WORKER_MESSAGE_GL_DRAW_ASYNC:
        break;
    case RED_WORKER_MESSAGE_CREATE_PRIMARY_SURFACE_ASYNC:
        red_qxl_create_primary_surface_complete(qxl_state);
        break;
    case RED_WORKER_MESSAGE_DESTROY_PRIMARY_SURFACE_ASYNC:
        red_qxl_destroy_primary_surface_complete(qxl_state);
        break;
    default:
        spice_warning("unexpected message %d", async_command->message);
    }
    qxl_get_interface(qxl_state->qxl)->async_complete(qxl_state->qxl,
                                                      async_command->cookie);
    free(async_command);
}

void red_qxl_gl_draw_async_complete(QXLState *qxl_state)
{
    /* this reset before usage prevent a possible race condition */
    struct AsyncCommand *async = qxl_state->gl_draw_async;
    qxl_state->gl_draw_async = NULL;
    red_qxl_async_complete(qxl_state, async);
}

void red_qxl_init(RedsState *reds, QXLInstance *qxl)
{
    QXLState *qxl_state;
    RedChannel *channel;
    ClientCbs client_cbs = { NULL, };

    spice_return_if_fail(qxl != NULL);
    spice_return_if_fail(qxl->st == NULL);

    static gsize initialized = FALSE;
    if (g_once_init_enter(&initialized)) {
        quic_init();
        sw_canvas_init();
        g_once_init_leave(&initialized, TRUE);
    }

    qxl_state = spice_new0(QXLState, 1);
    qxl_state->reds = reds;
    qxl_state->qxl = qxl;
    pthread_mutex_init(&qxl_state->scanout_mutex, NULL);
    qxl_state->scanout.drm_dma_buf_fd = -1;
    dispatcher_init(&qxl_state->dispatcher, RED_WORKER_MESSAGE_COUNT, NULL);
    qxl_state->base.major_version = SPICE_INTERFACE_QXL_MAJOR;
    qxl_state->base.minor_version = SPICE_INTERFACE_QXL_MINOR;
    qxl_state->base.wakeup = qxl_worker_wakeup;
    qxl_state->base.oom = qxl_worker_oom;
    qxl_state->base.start = qxl_worker_start;
    qxl_state->base.stop = qxl_worker_stop;
    qxl_state->base.update_area = qxl_worker_update_area;
    qxl_state->base.add_memslot = qxl_worker_add_memslot;
    qxl_state->base.del_memslot = qxl_worker_del_memslot;
    qxl_state->base.reset_memslots = qxl_worker_reset_memslots;
    qxl_state->base.destroy_surfaces = qxl_worker_destroy_surfaces;
    qxl_state->base.create_primary_surface = qxl_worker_create_primary_surface;
    qxl_state->base.destroy_primary_surface = qxl_worker_destroy_primary_surface;

    qxl_state->base.reset_image_cache = qxl_worker_reset_image_cache;
    qxl_state->base.reset_cursor = qxl_worker_reset_cursor;
    qxl_state->base.destroy_surface_wait = qxl_worker_destroy_surface_wait;
    qxl_state->base.loadvm_commands = qxl_worker_loadvm_commands;

    qxl_state->max_monitors = UINT_MAX;
    qxl->st = qxl_state;

    // TODO: reference and free
    RedWorker *worker = red_worker_new(qxl);

    // TODO: move to their respective channel files
    channel = red_worker_get_cursor_channel(worker);
    client_cbs.connect = red_qxl_set_cursor_peer;
    client_cbs.disconnect = red_qxl_disconnect_cursor_peer;
    client_cbs.migrate = red_qxl_cursor_migrate;
    red_channel_register_client_cbs(channel, &client_cbs);
    red_channel_set_data(channel, qxl_state);
    reds_register_channel(reds, channel);

    channel = red_worker_get_display_channel(worker);
    client_cbs.connect = red_qxl_set_display_peer;
    client_cbs.disconnect = red_qxl_disconnect_display_peer;
    client_cbs.migrate = red_qxl_display_migrate;
    red_channel_register_client_cbs(channel, &client_cbs);
    red_channel_set_data(channel, qxl_state);
    red_channel_set_cap(channel, SPICE_DISPLAY_CAP_MONITORS_CONFIG);
    red_channel_set_cap(channel, SPICE_DISPLAY_CAP_PREF_COMPRESSION);
    red_channel_set_cap(channel, SPICE_DISPLAY_CAP_STREAM_REPORT);
    reds_register_channel(reds, channel);

    red_worker_run(worker);
}

struct Dispatcher *red_qxl_get_dispatcher(QXLState *qxl_state)
{
    return &qxl_state->dispatcher;
}

void red_qxl_set_dispatcher_opaque(QXLState *qxl_state,
                                   void *opaque)
{
    dispatcher_set_opaque(&qxl_state->dispatcher, opaque);
}

void red_qxl_clear_pending(QXLState *qxl_state, int pending)
{
    spice_return_if_fail(qxl_state != NULL);

    clear_bit(pending, &qxl_state->pending);
}

gboolean red_qxl_get_primary_active(QXLState *qxl_state)
{
    return qxl_state->primary_active;
}

gboolean red_qxl_get_allow_client_mouse(QXLState *qxl_state, gint *x_res, gint *y_res)
{
    if (qxl_state->use_hardware_cursor) {
        if (x_res)
            *x_res = qxl_state->x_res;
        if (y_res)
            *y_res = qxl_state->y_res;
    }
    return qxl_state->use_hardware_cursor;
}

void red_qxl_on_ic_change(QXLState *qxl_state, SpiceImageCompression ic)
{
    RedWorkerMessageSetCompression payload;
    payload.image_compression = ic;
    dispatcher_send_message(&qxl_state->dispatcher,
                            RED_WORKER_MESSAGE_SET_COMPRESSION,
                            &payload);
}

void red_qxl_on_sv_change(QXLState *qxl_state, int sv)
{
    RedWorkerMessageSetStreamingVideo payload;
    payload.streaming_video = sv;
    dispatcher_send_message(&qxl_state->dispatcher,
                            RED_WORKER_MESSAGE_SET_STREAMING_VIDEO,
                            &payload);
}

void red_qxl_set_mouse_mode(QXLState *qxl_state, uint32_t mode)
{
    RedWorkerMessageSetMouseMode payload;
    payload.mode = mode;
    dispatcher_send_message(&qxl_state->dispatcher,
                            RED_WORKER_MESSAGE_SET_MOUSE_MODE,
                            &payload);
}
