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

#ifndef RED_DISPATCHER_H_
#define RED_DISPATCHER_H_

#include "red-channel.h"
#include "spice-qxl.h"

typedef struct AsyncCommand AsyncCommand;

void red_qxl_init(SpiceServer *reds, QXLInstance *qxl);
void red_qxl_destroy(QXLInstance *qxl);

void red_qxl_on_ic_change(QXLInstance *qxl, SpiceImageCompression ic);
void red_qxl_on_sv_change(QXLInstance *qxl, int sv);
void red_qxl_on_vc_change(QXLInstance *qxl, GArray* video_codecs);
void red_qxl_set_mouse_mode(QXLInstance *qxl, uint32_t mode);
void red_qxl_attach_worker(QXLInstance *qxl);
void red_qxl_set_compression_level(QXLInstance *qxl, int level);
void red_qxl_stop(QXLInstance *qxl);
void red_qxl_start(QXLInstance *qxl);
uint32_t red_qxl_get_ram_size(QXLInstance *qxl);
void red_qxl_async_complete(QXLInstance *qxl, AsyncCommand *async_command);
struct Dispatcher *red_qxl_get_dispatcher(QXLInstance *qxl);
gboolean red_qxl_use_client_monitors_config(QXLInstance *qxl);
gboolean red_qxl_client_monitors_config(QXLInstance *qxl, VDAgentMonitorsConfig *monitors_config);
gboolean red_qxl_get_primary_active(QXLInstance *qxl);
gboolean red_qxl_get_allow_client_mouse(QXLInstance *qxl, gint *x_res, gint *y_res);
SpiceMsgDisplayGlScanoutUnix *red_qxl_get_gl_scanout(QXLInstance *qxl);
void red_qxl_put_gl_scanout(QXLInstance *qxl, SpiceMsgDisplayGlScanoutUnix *scanout);
void red_qxl_gl_draw_async_complete(QXLInstance *qxl);
int red_qxl_check_qxl_version(QXLInstance *qxl, int major, int minor);
SpiceServer* red_qxl_get_server(QXLState *qxl);

/* Wrappers around QXLInterface vfuncs */
void red_qxl_get_init_info(QXLInstance *qxl, QXLDevInitInfo *info);
int red_qxl_get_command(QXLInstance *qxl, struct QXLCommandExt *cmd);
int red_qxl_req_cmd_notification(QXLInstance *qxl);
void red_qxl_release_resource(QXLInstance *qxl, struct QXLReleaseInfoExt release_info);
int red_qxl_get_cursor_command(QXLInstance *qxl, struct QXLCommandExt *cmd);
int red_qxl_req_cursor_notification(QXLInstance *qxl);
void red_qxl_notify_update(QXLInstance *qxl, uint32_t update_id);
int red_qxl_flush_resources(QXLInstance *qxl);
void red_qxl_async_complete(QXLInstance *qxl, AsyncCommand *cmd);
void red_qxl_update_area_complete(QXLInstance *qxl, uint32_t surface_id,
                                  struct QXLRect *updated_rects,
                                  uint32_t num_updated_rects);
void red_qxl_set_client_capabilities(QXLInstance *qxl,
                                     uint8_t client_present,
                                     uint8_t caps[SPICE_CAPABILITIES_SIZE]);

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
    RED_WORKER_MESSAGE_SET_VIDEO_CODECS,

    /* close worker thread */
    RED_WORKER_MESSAGE_CLOSE_WORKER,

    RED_WORKER_MESSAGE_COUNT // LAST
};

typedef struct RedWorkerMessageDisplayConnect {
    RedClient * client;
    RedsStream * stream;
    RedChannelCapabilities caps;   // red_worker should reset
    int migration;
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
    RedChannelCapabilities caps;   // red_worker should reset
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

typedef struct RedWorkerMessageSetVideoCodecs {
    GArray* video_codecs;
} RedWorkerMessageSetVideoCodecs;

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

typedef struct RedWorkerMessageGlScanout {
} RedWorkerMessageGlScanout;

typedef struct RedWorkerMessageClose {
} RedWorkerMessageClose;

typedef struct RedWorkerMessageGlDraw {
    SpiceMsgDisplayGlDraw draw;
} RedWorkerMessageGlDraw;

enum {
    RED_DISPATCHER_PENDING_WAKEUP,
    RED_DISPATCHER_PENDING_OOM,
};

void red_qxl_clear_pending(QXLState *qxl_state, int pending);

#endif /* RED_DISPATCHER_H_ */
