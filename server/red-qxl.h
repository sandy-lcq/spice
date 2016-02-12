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

#ifndef _H_RED_DISPATCHER
#define _H_RED_DISPATCHER

#include "red-channel.h"

typedef struct QXLState QXLState;

typedef struct AsyncCommand AsyncCommand;

void red_qxl_init(SpiceServer *reds, QXLInstance *qxl);

void red_qxl_set_mm_time(QXLState *qxl_state, uint32_t);
void red_qxl_on_ic_change(QXLState *qxl_state, SpiceImageCompression ic);
void red_qxl_on_sv_change(QXLState *qxl_state, int sv);
void red_qxl_set_mouse_mode(QXLState *qxl_state, uint32_t mode);
void red_qxl_attach_worker(QXLState *qxl_state);
void red_qxl_set_compression_level(QXLState *qxl_state, int level);
void red_qxl_stop(QXLState *qxl_state);
void red_qxl_start(QXLState *qxl_state);
uint32_t red_qxl_get_ram_size(QXLState *qxl_state);
void red_qxl_async_complete(QXLState *qxl_state, AsyncCommand *cmd);
struct Dispatcher *red_qxl_get_dispatcher(QXLState *qxl_state);
gboolean red_qxl_use_client_monitors_config(QXLState *qxl_state);
gboolean red_qxl_client_monitors_config(QXLState *qxl_state, VDAgentMonitorsConfig *monitors_config);
gboolean red_qxl_get_primary_active(QXLState *qxl_state);
gboolean red_qxl_get_allow_client_mouse(QXLState *qxl_state, gint *x_res, gint *y_res);
SpiceMsgDisplayGlScanoutUnix *red_qxl_get_gl_scanout(QXLState *qxl_state);
void red_qxl_put_gl_scanout(QXLState *qxl_state, SpiceMsgDisplayGlScanoutUnix *scanout);
void red_qxl_gl_draw_async_complete(QXLState *qxl);

typedef uint32_t RedWorkerMessage;

/* Keep message order, only append new messages!
 * Replay code store enum values into save files.
 */
enum {
    RED_WORKER_MESSAGE_NOP,

    RED_WORKER_MESSAGE_UPDATE,
    RED_WORKER_MESSAGE_WAKEUP,
    RED_WORKER_MESSAGE_OOM,
    RED_WORKER_MESSAGE_READY, /* unused */

    RED_WORKER_MESSAGE_DISPLAY_CONNECT,
    RED_WORKER_MESSAGE_DISPLAY_DISCONNECT,
    RED_WORKER_MESSAGE_DISPLAY_MIGRATE,
    RED_WORKER_MESSAGE_START,
    RED_WORKER_MESSAGE_STOP,
    RED_WORKER_MESSAGE_CURSOR_CONNECT,
    RED_WORKER_MESSAGE_CURSOR_DISCONNECT,
    RED_WORKER_MESSAGE_CURSOR_MIGRATE,
    RED_WORKER_MESSAGE_SET_COMPRESSION,
    RED_WORKER_MESSAGE_SET_STREAMING_VIDEO,
    RED_WORKER_MESSAGE_SET_MOUSE_MODE,
    RED_WORKER_MESSAGE_ADD_MEMSLOT,
    RED_WORKER_MESSAGE_DEL_MEMSLOT,
    RED_WORKER_MESSAGE_RESET_MEMSLOTS,
    RED_WORKER_MESSAGE_DESTROY_SURFACES,
    RED_WORKER_MESSAGE_CREATE_PRIMARY_SURFACE,
    RED_WORKER_MESSAGE_DESTROY_PRIMARY_SURFACE,
    RED_WORKER_MESSAGE_RESET_CURSOR,
    RED_WORKER_MESSAGE_RESET_IMAGE_CACHE,
    RED_WORKER_MESSAGE_DESTROY_SURFACE_WAIT,
    RED_WORKER_MESSAGE_LOADVM_COMMANDS,
    /* async commands */
    RED_WORKER_MESSAGE_UPDATE_ASYNC,
    RED_WORKER_MESSAGE_ADD_MEMSLOT_ASYNC,
    RED_WORKER_MESSAGE_DESTROY_SURFACES_ASYNC,
    RED_WORKER_MESSAGE_CREATE_PRIMARY_SURFACE_ASYNC,
    RED_WORKER_MESSAGE_DESTROY_PRIMARY_SURFACE_ASYNC,
    RED_WORKER_MESSAGE_DESTROY_SURFACE_WAIT_ASYNC,
    /* suspend/windows resolution change command */
    RED_WORKER_MESSAGE_FLUSH_SURFACES_ASYNC,

    RED_WORKER_MESSAGE_DISPLAY_CHANNEL_CREATE, /* unused */
    RED_WORKER_MESSAGE_CURSOR_CHANNEL_CREATE, /* unused */

    RED_WORKER_MESSAGE_MONITORS_CONFIG_ASYNC,
    RED_WORKER_MESSAGE_DRIVER_UNLOAD,
    RED_WORKER_MESSAGE_GL_SCANOUT,
    RED_WORKER_MESSAGE_GL_DRAW_ASYNC,

    RED_WORKER_MESSAGE_COUNT // LAST
};

typedef struct RedWorkerMessageDisplayConnect {
    RedClient * client;
    RedsStream * stream;
    uint32_t *common_caps; // red_worker should free
    uint32_t *caps;        // red_worker should free
    int migration;
    int num_common_caps;
    int num_caps;
} RedWorkerMessageDisplayConnect;

typedef struct RedWorkerMessageDisplayDisconnect {
    RedChannelClient *rcc;
} RedWorkerMessageDisplayDisconnect;

typedef struct RedWorkerMessageDisplayMigrate {
    RedChannelClient *rcc;
} RedWorkerMessageDisplayMigrate;

typedef struct RedWorkerMessageCursorConnect {
    RedClient *client;
    RedsStream *stream;
    int migration;
    uint32_t *common_caps; // red_worker should free
    int num_common_caps;
    uint32_t *caps;        // red_worker should free
    int num_caps;
} RedWorkerMessageCursorConnect;

typedef struct RedWorkerMessageCursorDisconnect {
    RedChannelClient *rcc;
} RedWorkerMessageCursorDisconnect;

typedef struct RedWorkerMessageCursorMigrate {
    RedChannelClient *rcc;
} RedWorkerMessageCursorMigrate;

typedef struct RedWorkerMessageUpdate {
    uint32_t surface_id;
    QXLRect * qxl_area;
    QXLRect * qxl_dirty_rects;
    uint32_t num_dirty_rects;
    uint32_t clear_dirty_region;
} RedWorkerMessageUpdate;

typedef struct RedWorkerMessageAsync {
    AsyncCommand *cmd;
} RedWorkerMessageAsync;

typedef struct RedWorkerMessageUpdateAsync {
    RedWorkerMessageAsync base;
    uint32_t surface_id;
    QXLRect qxl_area;
    uint32_t clear_dirty_region;
} RedWorkerMessageUpdateAsync;

typedef struct RedWorkerMessageAddMemslot {
    QXLDevMemSlot mem_slot;
} RedWorkerMessageAddMemslot;

typedef struct RedWorkerMessageAddMemslotAsync {
    RedWorkerMessageAsync base;
    QXLDevMemSlot mem_slot;
} RedWorkerMessageAddMemslotAsync;

typedef struct RedWorkerMessageDelMemslot {
    uint32_t slot_group_id;
    uint32_t slot_id;
} RedWorkerMessageDelMemslot;

typedef struct RedWorkerMessageDestroySurfaces {
} RedWorkerMessageDestroySurfaces;

typedef struct RedWorkerMessageDestroySurfacesAsync {
    RedWorkerMessageAsync base;
} RedWorkerMessageDestroySurfacesAsync;


typedef struct RedWorkerMessageDestroyPrimarySurface {
    uint32_t surface_id;
} RedWorkerMessageDestroyPrimarySurface;

typedef struct RedWorkerMessageDestroyPrimarySurfaceAsync {
    RedWorkerMessageAsync base;
    uint32_t surface_id;
} RedWorkerMessageDestroyPrimarySurfaceAsync;

typedef struct RedWorkerMessageCreatePrimarySurfaceAsync {
    RedWorkerMessageAsync base;
    uint32_t surface_id;
    QXLDevSurfaceCreate surface;
} RedWorkerMessageCreatePrimarySurfaceAsync;

typedef struct RedWorkerMessageCreatePrimarySurface {
    uint32_t surface_id;
    QXLDevSurfaceCreate surface;
} RedWorkerMessageCreatePrimarySurface;

typedef struct RedWorkerMessageResetImageCache {
} RedWorkerMessageResetImageCache;

typedef struct RedWorkerMessageResetCursor {
} RedWorkerMessageResetCursor;

typedef struct RedWorkerMessageWakeup {
} RedWorkerMessageWakeup;

typedef struct RedWorkerMessageOom {
} RedWorkerMessageOom;

typedef struct RedWorkerMessageStart {
} RedWorkerMessageStart;

typedef struct RedWorkerMessageFlushSurfacesAsync {
    RedWorkerMessageAsync base;
} RedWorkerMessageFlushSurfacesAsync;

typedef struct RedWorkerMessageStop {
} RedWorkerMessageStop;

/* this command is sync, so it's ok to pass a pointer */
typedef struct RedWorkerMessageLoadvmCommands {
    uint32_t count;
    QXLCommandExt *ext;
} RedWorkerMessageLoadvmCommands;

typedef struct RedWorkerMessageSetCompression {
    SpiceImageCompression image_compression;
} RedWorkerMessageSetCompression;

typedef struct RedWorkerMessageSetStreamingVideo {
    uint32_t streaming_video;
} RedWorkerMessageSetStreamingVideo;

typedef struct RedWorkerMessageSetMouseMode {
    uint32_t mode;
} RedWorkerMessageSetMouseMode;

typedef struct RedWorkerMessageDisplayChannelCreate {
} RedWorkerMessageDisplayChannelCreate;

typedef struct RedWorkerMessageCursorChannelCreate {
} RedWorkerMessageCursorChannelCreate;

typedef struct RedWorkerMessageDestroySurfaceWait {
    uint32_t surface_id;
} RedWorkerMessageDestroySurfaceWait;

typedef struct RedWorkerMessageDestroySurfaceWaitAsync {
    RedWorkerMessageAsync base;
    uint32_t surface_id;
} RedWorkerMessageDestroySurfaceWaitAsync;

typedef struct RedWorkerMessageResetMemslots {
} RedWorkerMessageResetMemslots;

typedef struct RedWorkerMessageMonitorsConfigAsync {
    RedWorkerMessageAsync base;
    QXLPHYSICAL monitors_config;
    int group_id;
    unsigned int max_monitors;
} RedWorkerMessageMonitorsConfigAsync;

typedef struct RedWorkerMessageDriverUnload {
} RedWorkerMessageDriverUnload;

enum {
    RED_DISPATCHER_PENDING_WAKEUP,
    RED_DISPATCHER_PENDING_OOM,
};

void red_qxl_clear_pending(QXLState *qxl_state, int pending);

#endif
