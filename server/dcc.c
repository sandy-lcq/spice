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
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "dcc.h"
#include "display-channel.h"

#define DISPLAY_CLIENT_SHORT_TIMEOUT 15000000000ULL //nano

static RedSurfaceCreateItem *red_surface_create_item_new(RedChannel* channel,
                                                         uint32_t surface_id,
                                                         uint32_t width,
                                                         uint32_t height,
                                                         uint32_t format,
                                                         uint32_t flags)
{
    RedSurfaceCreateItem *create;

    create = spice_malloc(sizeof(RedSurfaceCreateItem));

    create->surface_create.surface_id = surface_id;
    create->surface_create.width = width;
    create->surface_create.height = height;
    create->surface_create.flags = flags;
    create->surface_create.format = format;

    red_pipe_item_init(&create->pipe_item, RED_PIPE_ITEM_TYPE_CREATE_SURFACE);
    return create;
}

int dcc_drawable_is_in_pipe(DisplayChannelClient *dcc, Drawable *drawable)
{
    RedDrawablePipeItem *dpi;
    RingItem *dpi_link, *dpi_next;

    DRAWABLE_FOREACH_DPI_SAFE(drawable, dpi_link, dpi_next, dpi) {
        if (dpi->dcc == dcc) {
            return TRUE;
        }
    }

    return FALSE;
}

/*
 * Return: TRUE if wait_if_used == FALSE, or otherwise, if all of the pipe items that
 * are related to the surface have been cleared (or sent) from the pipe.
 */
int dcc_clear_surface_drawables_from_pipe(DisplayChannelClient *dcc, int surface_id,
                                          int wait_if_used)
{
    Ring *ring;
    RedPipeItem *item;
    int x;
    RedChannelClient *rcc;

    spice_return_val_if_fail(dcc != NULL, TRUE);
    /* removing the newest drawables that their destination is surface_id and
       no other drawable depends on them */

    rcc = RED_CHANNEL_CLIENT(dcc);
    ring = &rcc->pipe;
    item = (RedPipeItem *) ring;
    while ((item = (RedPipeItem *)ring_next(ring, (RingItem *)item))) {
        Drawable *drawable;
        RedDrawablePipeItem *dpi = NULL;
        int depend_found = FALSE;

        if (item->type == RED_PIPE_ITEM_TYPE_DRAW) {
            dpi = SPICE_CONTAINEROF(item, RedDrawablePipeItem, dpi_pipe_item);
            drawable = dpi->drawable;
        } else if (item->type == RED_PIPE_ITEM_TYPE_UPGRADE) {
            drawable = ((RedUpgradeItem *)item)->drawable;
        } else {
            continue;
        }

        if (drawable->surface_id == surface_id) {
            RedPipeItem *tmp_item = item;
            item = (RedPipeItem *)ring_prev(ring, (RingItem *)item);
            red_channel_client_pipe_remove_and_release(rcc, tmp_item);
            if (!item) {
                item = (RedPipeItem *)ring;
            }
            continue;
        }

        for (x = 0; x < 3; ++x) {
            if (drawable->surface_deps[x] == surface_id) {
                depend_found = TRUE;
                break;
            }
        }

        if (depend_found) {
            spice_debug("surface %d dependent item found %p, %p", surface_id, drawable, item);
            if (wait_if_used) {
                break;
            } else {
                return TRUE;
            }
        }
    }

    if (!wait_if_used) {
        return TRUE;
    }

    if (item) {
        return red_channel_client_wait_pipe_item_sent(RED_CHANNEL_CLIENT(dcc), item,
                                                      COMMON_CLIENT_TIMEOUT);
    } else {
        /*
         * in case that the pipe didn't contain any item that is dependent on the surface, but
         * there is one during sending. Use a shorter timeout, since it is just one item
         */
        return red_channel_client_wait_outgoing_item(RED_CHANNEL_CLIENT(dcc),
                                                     DISPLAY_CLIENT_SHORT_TIMEOUT);
    }
    return TRUE;
}

void dcc_create_surface(DisplayChannelClient *dcc, int surface_id)
{
    DisplayChannel *display;
    RedSurface *surface;
    RedSurfaceCreateItem *create;
    uint32_t flags;

    if (!dcc) {
        return;
    }

    display = DCC_TO_DC(dcc);
    flags = is_primary_surface(DCC_TO_DC(dcc), surface_id) ? SPICE_SURFACE_FLAGS_PRIMARY : 0;

    /* don't send redundant create surface commands to client */
    if (!dcc || display->common.during_target_migrate ||
        dcc->surface_client_created[surface_id]) {
        return;
    }
    surface = &display->surfaces[surface_id];
    create = red_surface_create_item_new(RED_CHANNEL_CLIENT(dcc)->channel,
                                         surface_id, surface->context.width,
                                         surface->context.height,
                                         surface->context.format, flags);
    dcc->surface_client_created[surface_id] = TRUE;
    red_channel_client_pipe_add(RED_CHANNEL_CLIENT(dcc), &create->pipe_item);
}

// adding the pipe item after pos. If pos == NULL, adding to head.
RedImageItem *dcc_add_surface_area_image(DisplayChannelClient *dcc,
                                         int surface_id,
                                         SpiceRect *area,
                                         RedPipeItem *pos,
                                         int can_lossy)
{
    DisplayChannel *display = DCC_TO_DC(dcc);
    RedSurface *surface = &display->surfaces[surface_id];
    SpiceCanvas *canvas = surface->context.canvas;
    RedImageItem *item;
    int stride;
    int width;
    int height;
    int bpp;
    int all_set;

    spice_assert(area);

    width = area->right - area->left;
    height = area->bottom - area->top;
    bpp = SPICE_SURFACE_FMT_DEPTH(surface->context.format) / 8;
    stride = width * bpp;

    item = (RedImageItem *)spice_malloc_n_m(height, stride, sizeof(RedImageItem));

    red_pipe_item_init(&item->base, RED_PIPE_ITEM_TYPE_IMAGE);

    item->surface_id = surface_id;
    item->image_format =
        spice_bitmap_from_surface_type(surface->context.format);
    item->image_flags = 0;
    item->pos.x = area->left;
    item->pos.y = area->top;
    item->width = width;
    item->height = height;
    item->stride = stride;
    item->top_down = surface->context.top_down;
    item->can_lossy = can_lossy;

    canvas->ops->read_bits(canvas, item->data, stride, area);

    /* For 32bit non-primary surfaces we need to keep any non-zero
       high bytes as the surface may be used as source to an alpha_blend */
    if (!is_primary_surface(display, surface_id) &&
        item->image_format == SPICE_BITMAP_FMT_32BIT &&
        rgb32_data_has_alpha(item->width, item->height, item->stride, item->data, &all_set)) {
        if (all_set) {
            item->image_flags |= SPICE_IMAGE_FLAGS_HIGH_BITS_SET;
        } else {
            item->image_format = SPICE_BITMAP_FMT_RGBA;
        }
    }

    if (pos) {
        red_channel_client_pipe_add_after(RED_CHANNEL_CLIENT(dcc), &item->base, pos);
    } else {
        red_channel_client_pipe_add(RED_CHANNEL_CLIENT(dcc), &item->base);
    }

    return item;
}

void dcc_push_surface_image(DisplayChannelClient *dcc, int surface_id)
{
    DisplayChannel *display;
    SpiceRect area;
    RedSurface *surface;

    if (!dcc) {
        return;
    }

    display = DCC_TO_DC(dcc);
    surface = &display->surfaces[surface_id];
    if (!surface->context.canvas) {
        return;
    }
    area.top = area.left = 0;
    area.right = surface->context.width;
    area.bottom = surface->context.height;

    /* not allowing lossy compression because probably, especially if it is a primary surface,
       it combines both "picture-like" areas with areas that are more "artificial"*/
    dcc_add_surface_area_image(dcc, surface_id, &area, NULL, FALSE);
    red_channel_client_push(RED_CHANNEL_CLIENT(dcc));
}

static void add_drawable_surface_images(DisplayChannelClient *dcc, Drawable *drawable)
{
    DisplayChannel *display = DCC_TO_DC(dcc);
    int x;

    for (x = 0; x < 3; ++x) {
        int surface_id;

        surface_id = drawable->surface_deps[x];
        if (surface_id != -1) {
            if (dcc->surface_client_created[surface_id] == TRUE) {
                continue;
            }
            dcc_create_surface(dcc, surface_id);
            display_channel_current_flush(display, surface_id);
            dcc_push_surface_image(dcc, surface_id);
        }
    }

    if (dcc->surface_client_created[drawable->surface_id] == TRUE) {
        return;
    }

    dcc_create_surface(dcc, drawable->surface_id);
    display_channel_current_flush(display, drawable->surface_id);
    dcc_push_surface_image(dcc, drawable->surface_id);
}

void red_drawable_pipe_item_free(RedPipeItem *item)
{
    RedDrawablePipeItem *dpi = SPICE_CONTAINEROF(item, RedDrawablePipeItem,
                                                 dpi_pipe_item);
    spice_assert(item->refcount == 0);

    spice_warn_if_fail(!ring_item_is_linked(&item->link));
    spice_warn_if_fail(!ring_item_is_linked(&dpi->base));
    drawable_unref(dpi->drawable);
    free(dpi);
}

static RedDrawablePipeItem *red_drawable_pipe_item_new(DisplayChannelClient *dcc,
                                                       Drawable *drawable)
{
    RedDrawablePipeItem *dpi;

    dpi = spice_malloc0(sizeof(*dpi));
    dpi->drawable = drawable;
    dpi->dcc = dcc;
    ring_item_init(&dpi->base);
    ring_add(&drawable->pipes, &dpi->base);
    red_pipe_item_init_full(&dpi->dpi_pipe_item, RED_PIPE_ITEM_TYPE_DRAW,
                            (GDestroyNotify)red_drawable_pipe_item_free);
    drawable->refs++;
    return dpi;
}

void dcc_prepend_drawable(DisplayChannelClient *dcc, Drawable *drawable)
{
    RedDrawablePipeItem *dpi = red_drawable_pipe_item_new(dcc, drawable);

    add_drawable_surface_images(dcc, drawable);
    red_channel_client_pipe_add(RED_CHANNEL_CLIENT(dcc), &dpi->dpi_pipe_item);
}

void dcc_append_drawable(DisplayChannelClient *dcc, Drawable *drawable)
{
    RedDrawablePipeItem *dpi = red_drawable_pipe_item_new(dcc, drawable);

    add_drawable_surface_images(dcc, drawable);
    red_channel_client_pipe_add_tail_and_push(RED_CHANNEL_CLIENT(dcc), &dpi->dpi_pipe_item);
}

void dcc_add_drawable_after(DisplayChannelClient *dcc, Drawable *drawable, RedPipeItem *pos)
{
    RedDrawablePipeItem *dpi = red_drawable_pipe_item_new(dcc, drawable);

    add_drawable_surface_images(dcc, drawable);
    red_channel_client_pipe_add_after(RED_CHANNEL_CLIENT(dcc), &dpi->dpi_pipe_item, pos);
}

static void dcc_init_stream_agents(DisplayChannelClient *dcc)
{
    int i;
    DisplayChannel *display = DCC_TO_DC(dcc);

    for (i = 0; i < NUM_STREAMS; i++) {
        StreamAgent *agent = &dcc->stream_agents[i];
        agent->stream = &display->streams_buf[i];
        region_init(&agent->vis_region);
        region_init(&agent->clip);
        red_pipe_item_init(&agent->create_item, RED_PIPE_ITEM_TYPE_STREAM_CREATE);
        red_pipe_item_init(&agent->destroy_item, RED_PIPE_ITEM_TYPE_STREAM_DESTROY);
    }
    dcc->use_mjpeg_encoder_rate_control =
        red_channel_client_test_remote_cap(RED_CHANNEL_CLIENT(dcc), SPICE_DISPLAY_CAP_STREAM_REPORT);
}

#define DISPLAY_FREE_LIST_DEFAULT_SIZE 128

DisplayChannelClient *dcc_new(DisplayChannel *display,
                              RedClient *client, RedsStream *stream,
                              int mig_target,
                              uint32_t *common_caps, int num_common_caps,
                              uint32_t *caps, int num_caps,
                              SpiceImageCompression image_compression,
                              spice_wan_compression_t jpeg_state,
                              spice_wan_compression_t zlib_glz_state)

{
    DisplayChannelClient *dcc;

    dcc = (DisplayChannelClient*)common_graphics_channel_new_client(
        COMMON_GRAPHICS_CHANNEL(display), sizeof(DisplayChannelClient),
        client, stream, mig_target, TRUE,
        common_caps, num_common_caps,
        caps, num_caps);
    dcc->id = display->common.qxl->id;
    spice_return_val_if_fail(dcc, NULL);
    spice_info("New display (client %p) dcc %p stream %p", client, dcc, stream);

    ring_init(&dcc->palette_cache_lru);
    dcc->palette_cache_available = CLIENT_PALETTE_CACHE_SIZE;
    dcc->image_compression = image_compression;
    dcc->jpeg_state = jpeg_state;
    dcc->zlib_glz_state = zlib_glz_state;
    // TODO: tune quality according to bandwidth
    dcc->jpeg_quality = 85;

    size_t stream_buf_size;
    stream_buf_size = 32*1024;
    dcc->send_data.stream_outbuf = spice_malloc(stream_buf_size);
    dcc->send_data.stream_outbuf_size = stream_buf_size;
    dcc->send_data.free_list.res =
        spice_malloc(sizeof(SpiceResourceList) +
                     DISPLAY_FREE_LIST_DEFAULT_SIZE * sizeof(SpiceResourceID));
    dcc->send_data.free_list.res_size = DISPLAY_FREE_LIST_DEFAULT_SIZE;

    dcc_init_stream_agents(dcc);

    dcc_encoders_init(dcc);

    return dcc;
}

static void dcc_create_all_streams(DisplayChannelClient *dcc)
{
    Ring *ring = &DCC_TO_DC(dcc)->streams;
    RingItem *item = ring;

    while ((item = ring_next(ring, item))) {
        Stream *stream = SPICE_CONTAINEROF(item, Stream, link);
        dcc_create_stream(dcc, stream);
    }
}

/* TODO: this function is evil^Wsynchronous, fix */
static int display_channel_client_wait_for_init(DisplayChannelClient *dcc)
{
    dcc->expect_init = TRUE;
    uint64_t end_time = spice_get_monotonic_time_ns() + COMMON_CLIENT_TIMEOUT;
    for (;;) {
        red_channel_client_receive(RED_CHANNEL_CLIENT(dcc));
        if (!red_channel_client_is_connected(RED_CHANNEL_CLIENT(dcc))) {
            break;
        }
        if (dcc->pixmap_cache && dcc->glz_dict) {
            dcc->pixmap_cache_generation = dcc->pixmap_cache->generation;
            /* TODO: move common.id? if it's used for a per client structure.. */
            spice_info("creating encoder with id == %d", dcc->id);
            dcc->glz = glz_encoder_create(dcc->id, dcc->glz_dict->dict, &dcc->glz_data.usr);
            if (!dcc->glz) {
                spice_critical("create global lz failed");
            }
            return TRUE;
        }
        if (spice_get_monotonic_time_ns() > end_time) {
            spice_warning("timeout");
            red_channel_client_disconnect(RED_CHANNEL_CLIENT(dcc));
            break;
        }
        usleep(DISPLAY_CLIENT_RETRY_INTERVAL);
    }
    return FALSE;
}

void dcc_start(DisplayChannelClient *dcc)
{
    DisplayChannel *display = DCC_TO_DC(dcc);
    RedChannelClient *rcc = RED_CHANNEL_CLIENT(dcc);

    red_channel_client_push_set_ack(RED_CHANNEL_CLIENT(dcc));

    if (red_channel_client_is_waiting_for_migrate_data(rcc))
        return;

    if (!display_channel_client_wait_for_init(dcc))
        return;

    red_channel_client_ack_zero_messages_window(RED_CHANNEL_CLIENT(dcc));
    if (display->surfaces[0].context.canvas) {
        display_channel_current_flush(display, 0);
        red_channel_client_pipe_add_type(rcc, RED_PIPE_ITEM_TYPE_INVAL_PALETTE_CACHE);
        dcc_create_surface(dcc, 0);
        dcc_push_surface_image(dcc, 0);
        dcc_push_monitors_config(dcc);
        red_pipe_add_verb(rcc, SPICE_MSG_DISPLAY_MARK);
        dcc_create_all_streams(dcc);
    }

    if (reds_stream_is_plain_unix(rcc->stream) &&
        red_channel_client_test_remote_cap(rcc, SPICE_DISPLAY_CAP_GL_SCANOUT)) {
        red_channel_client_pipe_add(rcc, dcc_gl_scanout_item_new(rcc, NULL, 0));
        dcc_push_monitors_config(dcc);
    }
}

static void dcc_destroy_stream_agents(DisplayChannelClient *dcc)
{
    int i;

    for (i = 0; i < NUM_STREAMS; i++) {
        StreamAgent *agent = &dcc->stream_agents[i];
        region_destroy(&agent->vis_region);
        region_destroy(&agent->clip);
        if (agent->mjpeg_encoder) {
            mjpeg_encoder_destroy(agent->mjpeg_encoder);
            agent->mjpeg_encoder = NULL;
        }
    }
}

void dcc_stop(DisplayChannelClient *dcc)
{
    DisplayChannel *dc = DCC_TO_DC(dcc);

    pixmap_cache_unref(dcc->pixmap_cache);
    dcc->pixmap_cache = NULL;
    dcc_release_glz(dcc);
    dcc_palette_cache_reset(dcc);
    free(dcc->send_data.stream_outbuf);
    free(dcc->send_data.free_list.res);
    dcc_destroy_stream_agents(dcc);
    dcc_encoders_free(dcc);

    if (dcc->gl_draw_ongoing) {
        display_channel_gl_draw_done(dc);
    }
}

void dcc_stream_agent_clip(DisplayChannelClient* dcc, StreamAgent *agent)
{
    RedStreamClipItem *item = red_stream_clip_item_new(agent);
    int n_rects;

    item->clip_type = SPICE_CLIP_TYPE_RECTS;

    n_rects = pixman_region32_n_rects(&agent->clip);
    item->rects = spice_malloc_n_m(n_rects, sizeof(SpiceRect), sizeof(SpiceClipRects));
    item->rects->num_rects = n_rects;
    region_ret_rects(&agent->clip, item->rects->rects, n_rects);

    red_channel_client_pipe_add(RED_CHANNEL_CLIENT(dcc), (RedPipeItem *)item);
}

static void red_monitors_config_item_free(RedMonitorsConfigItem *item)
{
    monitors_config_unref(item->monitors_config);
    free(item);
}

static RedMonitorsConfigItem *red_monitors_config_item_new(RedChannel* channel,
                                                           MonitorsConfig *monitors_config)
{
    RedMonitorsConfigItem *mci;

    mci = (RedMonitorsConfigItem *)spice_malloc(sizeof(*mci));
    mci->monitors_config = monitors_config;

    red_pipe_item_init_full(&mci->pipe_item, RED_PIPE_ITEM_TYPE_MONITORS_CONFIG,
                            (GDestroyNotify)red_monitors_config_item_free);
    return mci;
}

void dcc_push_monitors_config(DisplayChannelClient *dcc)
{
    DisplayChannel *dc = DCC_TO_DC(dcc);
    MonitorsConfig *monitors_config = dc->monitors_config;
    RedMonitorsConfigItem *mci;

    if (monitors_config == NULL) {
        spice_warning("monitors_config is NULL");
        return;
    }

    if (!red_channel_client_test_remote_cap(&dcc->common.base,
                                            SPICE_DISPLAY_CAP_MONITORS_CONFIG)) {
        return;
    }

    mci = red_monitors_config_item_new(dcc->common.base.channel,
                                       monitors_config_ref(dc->monitors_config));
    red_channel_client_pipe_add(&dcc->common.base, &mci->pipe_item);
    red_channel_client_push(&dcc->common.base);
}

static RedSurfaceDestroyItem *red_surface_destroy_item_new(RedChannel *channel,
                                                           uint32_t surface_id)
{
    RedSurfaceDestroyItem *destroy;

    destroy = spice_malloc(sizeof(RedSurfaceDestroyItem));
    destroy->surface_destroy.surface_id = surface_id;
    red_pipe_item_init(&destroy->pipe_item, RED_PIPE_ITEM_TYPE_DESTROY_SURFACE);

    return destroy;
}

RedPipeItem *dcc_gl_scanout_item_new(RedChannelClient *rcc, void *data, int num)
{
    RedGlScanoutUnixItem *item = spice_new(RedGlScanoutUnixItem, 1);
    spice_return_val_if_fail(item != NULL, NULL);

    /* FIXME: on !unix peer, start streaming with a video codec */
    if (!reds_stream_is_plain_unix(rcc->stream) ||
        !red_channel_client_test_remote_cap(rcc, SPICE_DISPLAY_CAP_GL_SCANOUT)) {
        spice_printerr("FIXME: client does not support GL scanout");
        red_channel_client_disconnect(rcc);
        return NULL;
    }

    red_pipe_item_init(&item->base, RED_PIPE_ITEM_TYPE_GL_SCANOUT);

    return &item->base;
}

RedPipeItem *dcc_gl_draw_item_new(RedChannelClient *rcc, void *data, int num)
{
    DisplayChannelClient *dcc = RCC_TO_DCC(rcc);
    const SpiceMsgDisplayGlDraw *draw = data;
    RedGlDrawItem *item = spice_new(RedGlDrawItem, 1);
    spice_return_val_if_fail(item != NULL, NULL);

    if (!red_channel_client_test_remote_cap(rcc, SPICE_DISPLAY_CAP_GL_SCANOUT)) {
        spice_printerr("FIXME: client does not support GL scanout");
        red_channel_client_disconnect(rcc);
        return NULL;
    }

    dcc->gl_draw_ongoing = TRUE;
    item->draw = *draw;
    red_pipe_item_init(&item->base, RED_PIPE_ITEM_TYPE_GL_DRAW);

    return &item->base;
}

void dcc_destroy_surface(DisplayChannelClient *dcc, uint32_t surface_id)
{
    DisplayChannel *display;
    RedChannel *channel;
    RedSurfaceDestroyItem *destroy;

    if (!dcc) {
        return;
    }

    display = DCC_TO_DC(dcc);
    channel = RED_CHANNEL(display);

    if (COMMON_GRAPHICS_CHANNEL(display)->during_target_migrate ||
        !dcc->surface_client_created[surface_id]) {
        return;
    }

    dcc->surface_client_created[surface_id] = FALSE;
    destroy = red_surface_destroy_item_new(channel, surface_id);
    red_channel_client_pipe_add(RED_CHANNEL_CLIENT(dcc), &destroy->pipe_item);
}

/* if already exists, returns it. Otherwise allocates and adds it (1) to the ring tail
   in the channel (2) to the Drawable*/
static RedGlzDrawable *get_glz_drawable(DisplayChannelClient *dcc, Drawable *drawable)
{
    RedGlzDrawable *ret;
    RingItem *item, *next;

    // TODO - I don't really understand what's going on here, so doing the technical equivalent
    // now that we have multiple glz_dicts, so the only way to go from dcc to drawable glz is to go
    // over the glz_ring (unless adding some better data structure then a ring)
    DRAWABLE_FOREACH_GLZ_SAFE(drawable, item, next, ret) {
        if (ret->dcc == dcc) {
            return ret;
        }
    }

    ret = spice_new(RedGlzDrawable, 1);

    ret->dcc = dcc;
    ret->red_drawable = red_drawable_ref(drawable->red_drawable);
    ret->drawable = drawable;
    ret->instances_count = 0;
    ring_init(&ret->instances);

    ring_item_init(&ret->link);
    ring_item_init(&ret->drawable_link);
    ring_add_before(&ret->link, &dcc->glz_drawables);
    ring_add(&drawable->glz_ring, &ret->drawable_link);
    DCC_TO_DC(dcc)->glz_drawable_count++;
    return ret;
}

/* allocates new instance and adds it to instances in the given drawable.
   NOTE - the caller should set the glz_instance returned by the encoder by itself.*/
static GlzDrawableInstanceItem *add_glz_drawable_instance(RedGlzDrawable *glz_drawable)
{
    spice_assert(glz_drawable->instances_count < MAX_GLZ_DRAWABLE_INSTANCES);
    // NOTE: We assume the additions are performed consecutively, without removals in the middle
    GlzDrawableInstanceItem *ret = glz_drawable->instances_pool + glz_drawable->instances_count;
    glz_drawable->instances_count++;

    ring_item_init(&ret->free_link);
    ring_item_init(&ret->glz_link);
    ring_add(&glz_drawable->instances, &ret->glz_link);
    ret->context = NULL;
    ret->glz_drawable = glz_drawable;

    return ret;
}

static const LzImageType bitmap_fmt_to_lz_image_type[] = {
    LZ_IMAGE_TYPE_INVALID,
    LZ_IMAGE_TYPE_PLT1_LE,
    LZ_IMAGE_TYPE_PLT1_BE,
    LZ_IMAGE_TYPE_PLT4_LE,
    LZ_IMAGE_TYPE_PLT4_BE,
    LZ_IMAGE_TYPE_PLT8,
    LZ_IMAGE_TYPE_RGB16,
    LZ_IMAGE_TYPE_RGB24,
    LZ_IMAGE_TYPE_RGB32,
    LZ_IMAGE_TYPE_RGBA,
    LZ_IMAGE_TYPE_A8
};

#define MIN_GLZ_SIZE_FOR_ZLIB 100

static int dcc_compress_image_glz(DisplayChannelClient *dcc,
                                  SpiceImage *dest, SpiceBitmap *src, Drawable *drawable,
                                  compress_send_data_t* o_comp_data)
{
    DisplayChannel *display_channel = DCC_TO_DC(dcc);
    stat_start_time_t start_time;
    stat_start_time_init(&start_time, &display_channel->zlib_glz_stat);
    spice_assert(bitmap_fmt_is_rgb(src->format));
    GlzData *glz_data = &dcc->glz_data;
    ZlibData *zlib_data;
    LzImageType type = bitmap_fmt_to_lz_image_type[src->format];
    RedGlzDrawable *glz_drawable;
    GlzDrawableInstanceItem *glz_drawable_instance;
    int glz_size;
    int zlib_size;

#ifdef COMPRESS_DEBUG
    spice_info("LZ global compress fmt=%d", src->format);
#endif

    encoder_data_init(&glz_data->data, dcc);

    glz_drawable = get_glz_drawable(dcc, drawable);
    glz_drawable_instance = add_glz_drawable_instance(glz_drawable);

    glz_data->data.u.lines_data.chunks = src->data;
    glz_data->data.u.lines_data.stride = src->stride;
    glz_data->data.u.lines_data.next = 0;
    glz_data->data.u.lines_data.reverse = 0;

    glz_size = glz_encode(dcc->glz, type, src->x, src->y,
                          (src->flags & SPICE_BITMAP_FLAGS_TOP_DOWN), NULL, 0,
                          src->stride, glz_data->data.bufs_head->buf.bytes,
                          sizeof(glz_data->data.bufs_head->buf),
                          glz_drawable_instance,
                          &glz_drawable_instance->context);

    stat_compress_add(&display_channel->glz_stat, start_time, src->stride * src->y, glz_size);

    if (!display_channel->enable_zlib_glz_wrap || (glz_size < MIN_GLZ_SIZE_FOR_ZLIB)) {
        goto glz;
    }
    stat_start_time_init(&start_time, &display_channel->zlib_glz_stat);
    zlib_data = &dcc->zlib_data;

    encoder_data_init(&zlib_data->data, dcc);

    zlib_data->data.u.compressed_data.next = glz_data->data.bufs_head;
    zlib_data->data.u.compressed_data.size_left = glz_size;

    zlib_size = zlib_encode(dcc->zlib, dcc->zlib_level,
                            glz_size, zlib_data->data.bufs_head->buf.bytes,
                            sizeof(zlib_data->data.bufs_head->buf));

    // the compressed buffer is bigger than the original data
    if (zlib_size >= glz_size) {
        encoder_data_reset(&zlib_data->data);
        goto glz;
    } else {
        encoder_data_reset(&glz_data->data);
    }

    dest->descriptor.type = SPICE_IMAGE_TYPE_ZLIB_GLZ_RGB;
    dest->u.zlib_glz.glz_data_size = glz_size;
    dest->u.zlib_glz.data_size = zlib_size;

    o_comp_data->comp_buf = zlib_data->data.bufs_head;
    o_comp_data->comp_buf_size = zlib_size;

    stat_compress_add(&display_channel->zlib_glz_stat, start_time, glz_size, zlib_size);
    return TRUE;
glz:
    dest->descriptor.type = SPICE_IMAGE_TYPE_GLZ_RGB;
    dest->u.lz_rgb.data_size = glz_size;

    o_comp_data->comp_buf = glz_data->data.bufs_head;
    o_comp_data->comp_buf_size = glz_size;

    return TRUE;
}

static int dcc_compress_image_lz(DisplayChannelClient *dcc,
                                 SpiceImage *dest, SpiceBitmap *src,
                                 compress_send_data_t* o_comp_data)
{
    LzData *lz_data = &dcc->lz_data;
    LzContext *lz = dcc->lz;
    LzImageType type = bitmap_fmt_to_lz_image_type[src->format];
    int size;            // size of the compressed data

    stat_start_time_t start_time;
    stat_start_time_init(&start_time, &DCC_TO_DC(dcc)->lz_stat);

#ifdef COMPRESS_DEBUG
    spice_info("LZ LOCAL compress");
#endif

    encoder_data_init(&lz_data->data, dcc);

    if (setjmp(lz_data->data.jmp_env)) {
        encoder_data_reset(&lz_data->data);
        return FALSE;
    }

    lz_data->data.u.lines_data.chunks = src->data;
    lz_data->data.u.lines_data.stride = src->stride;
    lz_data->data.u.lines_data.next = 0;
    lz_data->data.u.lines_data.reverse = 0;

    size = lz_encode(lz, type, src->x, src->y,
                     !!(src->flags & SPICE_BITMAP_FLAGS_TOP_DOWN),
                     NULL, 0, src->stride,
                     lz_data->data.bufs_head->buf.bytes,
                     sizeof(lz_data->data.bufs_head->buf));

    // the compressed buffer is bigger than the original data
    if (size > (src->y * src->stride)) {
        longjmp(lz_data->data.jmp_env, 1);
    }

    if (bitmap_fmt_is_rgb(src->format)) {
        dest->descriptor.type = SPICE_IMAGE_TYPE_LZ_RGB;
        dest->u.lz_rgb.data_size = size;

        o_comp_data->comp_buf = lz_data->data.bufs_head;
        o_comp_data->comp_buf_size = size;
    } else {
        /* masks are 1BIT bitmaps without palettes, but they are not compressed
         * (see fill_mask) */
        spice_assert(src->palette);
        dest->descriptor.type = SPICE_IMAGE_TYPE_LZ_PLT;
        dest->u.lz_plt.data_size = size;
        dest->u.lz_plt.flags = src->flags & SPICE_BITMAP_FLAGS_TOP_DOWN;
        dest->u.lz_plt.palette = src->palette;
        dest->u.lz_plt.palette_id = src->palette->unique;
        o_comp_data->comp_buf = lz_data->data.bufs_head;
        o_comp_data->comp_buf_size = size;

        dcc_palette_cache_palette(dcc, dest->u.lz_plt.palette, &(dest->u.lz_plt.flags));
        o_comp_data->lzplt_palette = dest->u.lz_plt.palette;
    }

    stat_compress_add(&DCC_TO_DC(dcc)->lz_stat, start_time, src->stride * src->y,
                      o_comp_data->comp_buf_size);
    return TRUE;
}

static int dcc_compress_image_jpeg(DisplayChannelClient *dcc, SpiceImage *dest,
                                   SpiceBitmap *src, compress_send_data_t* o_comp_data)
{
    JpegData *jpeg_data = &dcc->jpeg_data;
    LzData *lz_data = &dcc->lz_data;
    JpegEncoderContext *jpeg = dcc->jpeg;
    LzContext *lz = dcc->lz;
    volatile JpegEncoderImageType jpeg_in_type;
    int jpeg_size = 0;
    volatile int has_alpha = FALSE;
    int alpha_lz_size = 0;
    int comp_head_filled;
    int comp_head_left;
    int stride;
    uint8_t *lz_out_start_byte;
    stat_start_time_t start_time;
    stat_start_time_init(&start_time, &DCC_TO_DC(dcc)->jpeg_alpha_stat);

#ifdef COMPRESS_DEBUG
    spice_info("JPEG compress");
#endif

    switch (src->format) {
    case SPICE_BITMAP_FMT_16BIT:
        jpeg_in_type = JPEG_IMAGE_TYPE_RGB16;
        break;
    case SPICE_BITMAP_FMT_24BIT:
        jpeg_in_type = JPEG_IMAGE_TYPE_BGR24;
        break;
    case SPICE_BITMAP_FMT_32BIT:
        jpeg_in_type = JPEG_IMAGE_TYPE_BGRX32;
        break;
    case SPICE_BITMAP_FMT_RGBA:
        jpeg_in_type = JPEG_IMAGE_TYPE_BGRX32;
        has_alpha = TRUE;
        break;
    default:
        return FALSE;
    }

    encoder_data_init(&jpeg_data->data, dcc);

    if (setjmp(jpeg_data->data.jmp_env)) {
        encoder_data_reset(&jpeg_data->data);
        return FALSE;
    }

    if (src->data->flags & SPICE_CHUNKS_FLAGS_UNSTABLE) {
        spice_chunks_linearize(src->data);
    }

    jpeg_data->data.u.lines_data.chunks = src->data;
    jpeg_data->data.u.lines_data.stride = src->stride;
    if ((src->flags & SPICE_BITMAP_FLAGS_TOP_DOWN)) {
        jpeg_data->data.u.lines_data.next = 0;
        jpeg_data->data.u.lines_data.reverse = 0;
        stride = src->stride;
    } else {
        jpeg_data->data.u.lines_data.next = src->data->num_chunks - 1;
        jpeg_data->data.u.lines_data.reverse = 1;
        stride = -src->stride;
    }
    jpeg_size = jpeg_encode(jpeg, dcc->jpeg_quality, jpeg_in_type,
                            src->x, src->y, NULL,
                            0, stride, jpeg_data->data.bufs_head->buf.bytes,
                            sizeof(jpeg_data->data.bufs_head->buf));

    // the compressed buffer is bigger than the original data
    if (jpeg_size > (src->y * src->stride)) {
        longjmp(jpeg_data->data.jmp_env, 1);
    }

    if (!has_alpha) {
        dest->descriptor.type = SPICE_IMAGE_TYPE_JPEG;
        dest->u.jpeg.data_size = jpeg_size;

        o_comp_data->comp_buf = jpeg_data->data.bufs_head;
        o_comp_data->comp_buf_size = jpeg_size;
        o_comp_data->is_lossy = TRUE;

        stat_compress_add(&DCC_TO_DC(dcc)->jpeg_stat, start_time, src->stride * src->y,
                          o_comp_data->comp_buf_size);
        return TRUE;
    }

    lz_data->data.bufs_head = jpeg_data->data.bufs_tail;
    lz_data->data.bufs_tail = lz_data->data.bufs_head;

    comp_head_filled = jpeg_size % sizeof(lz_data->data.bufs_head->buf);
    comp_head_left = sizeof(lz_data->data.bufs_head->buf) - comp_head_filled;
    lz_out_start_byte = lz_data->data.bufs_head->buf.bytes + comp_head_filled;

    lz_data->data.dcc = dcc;

    lz_data->data.u.lines_data.chunks = src->data;
    lz_data->data.u.lines_data.stride = src->stride;
    lz_data->data.u.lines_data.next = 0;
    lz_data->data.u.lines_data.reverse = 0;

    alpha_lz_size = lz_encode(lz, LZ_IMAGE_TYPE_XXXA, src->x, src->y,
                               !!(src->flags & SPICE_BITMAP_FLAGS_TOP_DOWN),
                               NULL, 0, src->stride,
                               lz_out_start_byte,
                               comp_head_left);

    // the compressed buffer is bigger than the original data
    if ((jpeg_size + alpha_lz_size) > (src->y * src->stride)) {
        longjmp(jpeg_data->data.jmp_env, 1);
    }

    dest->descriptor.type = SPICE_IMAGE_TYPE_JPEG_ALPHA;
    dest->u.jpeg_alpha.flags = 0;
    if (src->flags & SPICE_BITMAP_FLAGS_TOP_DOWN) {
        dest->u.jpeg_alpha.flags |= SPICE_JPEG_ALPHA_FLAGS_TOP_DOWN;
    }

    dest->u.jpeg_alpha.jpeg_size = jpeg_size;
    dest->u.jpeg_alpha.data_size = jpeg_size + alpha_lz_size;

    o_comp_data->comp_buf = jpeg_data->data.bufs_head;
    o_comp_data->comp_buf_size = jpeg_size + alpha_lz_size;
    o_comp_data->is_lossy = TRUE;
    stat_compress_add(&DCC_TO_DC(dcc)->jpeg_alpha_stat, start_time, src->stride * src->y,
                      o_comp_data->comp_buf_size);
    return TRUE;
}

#ifdef USE_LZ4
static int dcc_compress_image_lz4(DisplayChannelClient *dcc, SpiceImage *dest,
                                  SpiceBitmap *src, compress_send_data_t* o_comp_data)
{
    Lz4Data *lz4_data = &dcc->lz4_data;
    Lz4EncoderContext *lz4 = dcc->lz4;
    int lz4_size = 0;
    stat_start_time_t start_time;
    stat_start_time_init(&start_time, &DCC_TO_DC(dcc)->lz4_stat);

#ifdef COMPRESS_DEBUG
    spice_info("LZ4 compress");
#endif

    encoder_data_init(&lz4_data->data, dcc);

    if (setjmp(lz4_data->data.jmp_env)) {
        encoder_data_reset(&lz4_data->data);
        return FALSE;
    }

    if (src->data->flags & SPICE_CHUNKS_FLAGS_UNSTABLE) {
        spice_chunks_linearize(src->data);
    }

    lz4_data->data.u.lines_data.chunks = src->data;
    lz4_data->data.u.lines_data.stride = src->stride;
    lz4_data->data.u.lines_data.next = 0;
    lz4_data->data.u.lines_data.reverse = 0;

    lz4_size = lz4_encode(lz4, src->y, src->stride, lz4_data->data.bufs_head->buf.bytes,
                          sizeof(lz4_data->data.bufs_head->buf),
                          src->flags & SPICE_BITMAP_FLAGS_TOP_DOWN, src->format);

    // the compressed buffer is bigger than the original data
    if (lz4_size > (src->y * src->stride)) {
        longjmp(lz4_data->data.jmp_env, 1);
    }

    dest->descriptor.type = SPICE_IMAGE_TYPE_LZ4;
    dest->u.lz4.data_size = lz4_size;

    o_comp_data->comp_buf = lz4_data->data.bufs_head;
    o_comp_data->comp_buf_size = lz4_size;

    stat_compress_add(&DCC_TO_DC(dcc)->lz4_stat, start_time, src->stride * src->y,
                      o_comp_data->comp_buf_size);
    return TRUE;
}
#endif

static int dcc_compress_image_quic(DisplayChannelClient *dcc, SpiceImage *dest,
                                   SpiceBitmap *src, compress_send_data_t* o_comp_data)
{
    QuicData *quic_data = &dcc->quic_data;
    QuicContext *quic = dcc->quic;
    volatile QuicImageType type;
    int size, stride;
    stat_start_time_t start_time;
    stat_start_time_init(&start_time, &DCC_TO_DC(dcc)->quic_stat);

#ifdef COMPRESS_DEBUG
    spice_info("QUIC compress");
#endif

    switch (src->format) {
    case SPICE_BITMAP_FMT_32BIT:
        type = QUIC_IMAGE_TYPE_RGB32;
        break;
    case SPICE_BITMAP_FMT_RGBA:
        type = QUIC_IMAGE_TYPE_RGBA;
        break;
    case SPICE_BITMAP_FMT_16BIT:
        type = QUIC_IMAGE_TYPE_RGB16;
        break;
    case SPICE_BITMAP_FMT_24BIT:
        type = QUIC_IMAGE_TYPE_RGB24;
        break;
    default:
        return FALSE;
    }

    encoder_data_init(&quic_data->data, dcc);

    if (setjmp(quic_data->data.jmp_env)) {
        encoder_data_reset(&quic_data->data);
        return FALSE;
    }

    if (src->data->flags & SPICE_CHUNKS_FLAGS_UNSTABLE) {
        spice_chunks_linearize(src->data);
    }

    quic_data->data.u.lines_data.chunks = src->data;
    quic_data->data.u.lines_data.stride = src->stride;
    if ((src->flags & SPICE_BITMAP_FLAGS_TOP_DOWN)) {
        quic_data->data.u.lines_data.next = 0;
        quic_data->data.u.lines_data.reverse = 0;
        stride = src->stride;
    } else {
        quic_data->data.u.lines_data.next = src->data->num_chunks - 1;
        quic_data->data.u.lines_data.reverse = 1;
        stride = -src->stride;
    }
    size = quic_encode(quic, type, src->x, src->y, NULL, 0, stride,
                       quic_data->data.bufs_head->buf.words,
                       G_N_ELEMENTS(quic_data->data.bufs_head->buf.words));

    // the compressed buffer is bigger than the original data
    if ((size << 2) > (src->y * src->stride)) {
        longjmp(quic_data->data.jmp_env, 1);
    }

    dest->descriptor.type = SPICE_IMAGE_TYPE_QUIC;
    dest->u.quic.data_size = size << 2;

    o_comp_data->comp_buf = quic_data->data.bufs_head;
    o_comp_data->comp_buf_size = size << 2;

    stat_compress_add(&DCC_TO_DC(dcc)->quic_stat, start_time, src->stride * src->y,
                      o_comp_data->comp_buf_size);
    return TRUE;
}

#define MIN_DIMENSION_TO_QUIC 3
/**
 * quic doesn't handle:
 *       (1) palette
 */
static bool can_quic_compress(SpiceBitmap *bitmap)
{
    return !bitmap_fmt_is_plt(bitmap->format) &&
            bitmap->x >= MIN_DIMENSION_TO_QUIC && bitmap->y >= MIN_DIMENSION_TO_QUIC;
}
/**
 * lz doesn't handle:
 *       (1) bitmaps with strides that are larger than the width of the image in bytes
 *       (2) unstable bitmaps
 */
static bool can_lz_compress(SpiceBitmap *bitmap)
{
    return !bitmap_has_extra_stride(bitmap) &&
           !(bitmap->data->flags & SPICE_CHUNKS_FLAGS_UNSTABLE);
}

#define MIN_SIZE_TO_COMPRESS 54
static SpiceImageCompression get_compression_for_bitmap(SpiceBitmap *bitmap,
                                                        SpiceImageCompression preferred_compression,
                                                        Drawable *drawable)
{
    if (bitmap->y * bitmap->stride < MIN_SIZE_TO_COMPRESS) { // TODO: change the size cond
        return SPICE_IMAGE_COMPRESSION_OFF;
    }
    if (preferred_compression == SPICE_IMAGE_COMPRESSION_OFF) {
        return SPICE_IMAGE_COMPRESSION_OFF;
    }
    if (preferred_compression == SPICE_IMAGE_COMPRESSION_QUIC) {
        if (can_quic_compress(bitmap)) {
            return SPICE_IMAGE_COMPRESSION_QUIC;
        }
        return SPICE_IMAGE_COMPRESSION_OFF;
    }

    if (preferred_compression == SPICE_IMAGE_COMPRESSION_AUTO_GLZ ||
        preferred_compression == SPICE_IMAGE_COMPRESSION_AUTO_LZ) {
        if (can_quic_compress(bitmap)) {
            if (drawable == NULL ||
                drawable->copy_bitmap_graduality == BITMAP_GRADUAL_INVALID) {
                if (bitmap_fmt_has_graduality(bitmap->format) &&
                    bitmap_get_graduality_level(bitmap) == BITMAP_GRADUAL_HIGH) {
                    return SPICE_IMAGE_COMPRESSION_QUIC;
                }
            } else if (!can_lz_compress(bitmap) ||
                       drawable->copy_bitmap_graduality == BITMAP_GRADUAL_HIGH) {
                return SPICE_IMAGE_COMPRESSION_QUIC;
            }
        }
        if (preferred_compression == SPICE_IMAGE_COMPRESSION_AUTO_LZ) {
            preferred_compression = SPICE_IMAGE_COMPRESSION_LZ;
        } else {
            preferred_compression = SPICE_IMAGE_COMPRESSION_GLZ;
        }
    }

    if (preferred_compression == SPICE_IMAGE_COMPRESSION_GLZ) {
        if (drawable == NULL || !bitmap_fmt_has_graduality(bitmap->format)) {
            preferred_compression = SPICE_IMAGE_COMPRESSION_LZ;
        }
    }

    if (preferred_compression == SPICE_IMAGE_COMPRESSION_LZ4) {
        if (!bitmap_fmt_is_rgb(bitmap->format)) {
            preferred_compression = SPICE_IMAGE_COMPRESSION_LZ;
        }
    }

    if (preferred_compression == SPICE_IMAGE_COMPRESSION_LZ ||
        preferred_compression == SPICE_IMAGE_COMPRESSION_LZ4 ||
        preferred_compression == SPICE_IMAGE_COMPRESSION_GLZ) {
        if (can_lz_compress(bitmap)) {
            return preferred_compression;
        }
        return SPICE_IMAGE_COMPRESSION_OFF;
    }

    return SPICE_IMAGE_COMPRESSION_INVALID;
}

int dcc_compress_image(DisplayChannelClient *dcc,
                       SpiceImage *dest, SpiceBitmap *src, Drawable *drawable,
                       int can_lossy,
                       compress_send_data_t* o_comp_data)
{
    DisplayChannel *display_channel = DCC_TO_DC(dcc);
    SpiceImageCompression image_compression;
    stat_start_time_t start_time;
    int success = FALSE;

    stat_start_time_init(&start_time, &display_channel->off_stat);

    image_compression = get_compression_for_bitmap(src, dcc->image_compression, drawable);
    switch (image_compression) {
    case SPICE_IMAGE_COMPRESSION_OFF:
        break;
    case SPICE_IMAGE_COMPRESSION_QUIC:
        if (can_lossy && display_channel->enable_jpeg &&
            (src->format != SPICE_BITMAP_FMT_RGBA || !bitmap_has_extra_stride(src))) {
            success = dcc_compress_image_jpeg(dcc, dest, src, o_comp_data);
            break;
        }
        success = dcc_compress_image_quic(dcc, dest, src, o_comp_data);
        break;
    case SPICE_IMAGE_COMPRESSION_GLZ:
        if ((src->x * src->y) < glz_enc_dictionary_get_size(dcc->glz_dict->dict)) {
            int frozen;
            /* using the global dictionary only if it is not frozen */
            pthread_rwlock_rdlock(&dcc->glz_dict->encode_lock);
            frozen = dcc->glz_dict->migrate_freeze;
            if (!frozen) {
                success = dcc_compress_image_glz(dcc, dest, src, drawable, o_comp_data);
            }
            pthread_rwlock_unlock(&dcc->glz_dict->encode_lock);
            if (!frozen) {
                break;
            }
        }
        goto lz_compress;
#ifdef USE_LZ4
    case SPICE_IMAGE_COMPRESSION_LZ4:
        if (red_channel_client_test_remote_cap(&dcc->common.base,
                                               SPICE_DISPLAY_CAP_LZ4_COMPRESSION)) {
            success = dcc_compress_image_lz4(dcc, dest, src, o_comp_data);
            break;
        }
#endif
lz_compress:
    case SPICE_IMAGE_COMPRESSION_LZ:
        success = dcc_compress_image_lz(dcc, dest, src, o_comp_data);
        break;
    default:
        spice_error("invalid image compression type %u", image_compression);
    }

    if (!success) {
        uint64_t image_size = src->stride * src->y;
        stat_compress_add(&display_channel->off_stat, start_time, image_size, image_size);
    }

    return success;
}

#define CLIENT_PALETTE_CACHE
#include "cache-item.tmpl.c"
#undef CLIENT_PALETTE_CACHE

void dcc_palette_cache_palette(DisplayChannelClient *dcc, SpicePalette *palette,
                               uint8_t *flags)
{
    if (palette == NULL) {
        return;
    }
    if (palette->unique) {
        if (red_palette_cache_find(dcc, palette->unique)) {
            *flags |= SPICE_BITMAP_FLAGS_PAL_FROM_CACHE;
            return;
        }
        if (red_palette_cache_add(dcc, palette->unique, 1)) {
            *flags |= SPICE_BITMAP_FLAGS_PAL_CACHE_ME;
        }
    }
}

void dcc_palette_cache_reset(DisplayChannelClient *dcc)
{
    red_palette_cache_reset(dcc, CLIENT_PALETTE_CACHE_SIZE);
}

static void dcc_push_release(DisplayChannelClient *dcc, uint8_t type, uint64_t id,
                             uint64_t* sync_data)
{
    FreeList *free_list = &dcc->send_data.free_list;
    int i;

    for (i = 0; i < MAX_CACHE_CLIENTS; i++) {
        free_list->sync[i] = MAX(free_list->sync[i], sync_data[i]);
    }

    if (free_list->res->count == free_list->res_size) {
        SpiceResourceList *new_list;
        new_list = spice_malloc(sizeof(*new_list) +
                                free_list->res_size * sizeof(SpiceResourceID) * 2);
        new_list->count = free_list->res->count;
        memcpy(new_list->resources, free_list->res->resources,
               new_list->count * sizeof(SpiceResourceID));
        free(free_list->res);
        free_list->res = new_list;
        free_list->res_size *= 2;
    }
    free_list->res->resources[free_list->res->count].type = type;
    free_list->res->resources[free_list->res->count++].id = id;
}

int dcc_pixmap_cache_unlocked_add(DisplayChannelClient *dcc, uint64_t id,
                                  uint32_t size, int lossy)
{
    PixmapCache *cache = dcc->pixmap_cache;
    NewCacheItem *item;
    uint64_t serial;
    int key;

    spice_assert(size > 0);

    item = spice_new(NewCacheItem, 1);
    serial = red_channel_client_get_message_serial(RED_CHANNEL_CLIENT(dcc));

    if (cache->generation != dcc->pixmap_cache_generation) {
        if (!dcc->pending_pixmaps_sync) {
            red_channel_client_pipe_add_type(
                                             RED_CHANNEL_CLIENT(dcc), RED_PIPE_ITEM_TYPE_PIXMAP_SYNC);
            dcc->pending_pixmaps_sync = TRUE;
        }
        free(item);
        return FALSE;
    }

    cache->available -= size;
    while (cache->available < 0) {
        NewCacheItem *tail;
        NewCacheItem **now;

        if (!(tail = (NewCacheItem *)ring_get_tail(&cache->lru)) ||
                                                   tail->sync[dcc->id] == serial) {
            cache->available += size;
            free(item);
            return FALSE;
        }

        now = &cache->hash_table[BITS_CACHE_HASH_KEY(tail->id)];
        for (;;) {
            spice_assert(*now);
            if (*now == tail) {
                *now = tail->next;
                break;
            }
            now = &(*now)->next;
        }
        ring_remove(&tail->lru_link);
        cache->items--;
        cache->available += tail->size;
        cache->sync[dcc->id] = serial;
        dcc_push_release(dcc, SPICE_RES_TYPE_PIXMAP, tail->id, tail->sync);
        free(tail);
    }
    ++cache->items;
    item->next = cache->hash_table[(key = BITS_CACHE_HASH_KEY(id))];
    cache->hash_table[key] = item;
    ring_item_init(&item->lru_link);
    ring_add(&cache->lru, &item->lru_link);
    item->id = id;
    item->size = size;
    item->lossy = lossy;
    memset(item->sync, 0, sizeof(item->sync));
    item->sync[dcc->id] = serial;
    cache->sync[dcc->id] = serial;
    return TRUE;
}

static int dcc_handle_init(DisplayChannelClient *dcc, SpiceMsgcDisplayInit *init)
{
    spice_return_val_if_fail(dcc->expect_init, FALSE);
    dcc->expect_init = FALSE;

    spice_return_val_if_fail(!dcc->pixmap_cache, FALSE);
    dcc->pixmap_cache = pixmap_cache_get(RED_CHANNEL_CLIENT(dcc)->client,
                                         init->pixmap_cache_id,
                                         init->pixmap_cache_size);
    spice_return_val_if_fail(dcc->pixmap_cache, FALSE);

    spice_return_val_if_fail(!dcc->glz_dict, FALSE);
    ring_init(&dcc->glz_drawables);
    ring_init(&dcc->glz_drawables_inst_to_free);
    pthread_mutex_init(&dcc->glz_drawables_inst_to_free_lock, NULL);
    dcc->glz_dict = dcc_get_glz_dictionary(dcc,
                                           init->glz_dictionary_id,
                                           init->glz_dictionary_window_size);
    spice_return_val_if_fail(dcc->glz_dict, FALSE);

    return TRUE;
}

static int dcc_handle_stream_report(DisplayChannelClient *dcc,
                                    SpiceMsgcDisplayStreamReport *report)
{
    StreamAgent *agent;

    if (report->stream_id >= NUM_STREAMS) {
        spice_warning("stream_report: invalid stream id %u",
                      report->stream_id);
        return FALSE;
    }

    agent = &dcc->stream_agents[report->stream_id];
    if (!agent->mjpeg_encoder) {
        spice_info("stream_report: no encoder for stream id %u. "
                   "The stream has probably been destroyed",
                   report->stream_id);
        return TRUE;
    }

    if (report->unique_id != agent->report_id) {
        spice_warning("stream_report: unique id mismatch: local (%u) != msg (%u) "
                      "The old stream was probably replaced by a new one",
                      agent->report_id, report->unique_id);
        return TRUE;
    }

    mjpeg_encoder_client_stream_report(agent->mjpeg_encoder,
                                       report->num_frames,
                                       report->num_drops,
                                       report->start_frame_mm_time,
                                       report->end_frame_mm_time,
                                       report->last_frame_delay,
                                       report->audio_delay);
    return TRUE;
}

static int dcc_handle_preferred_compression(DisplayChannelClient *dcc,
        SpiceMsgcDisplayPreferredCompression *pc)
{
    switch (pc->image_compression) {
    case SPICE_IMAGE_COMPRESSION_AUTO_LZ:
    case SPICE_IMAGE_COMPRESSION_AUTO_GLZ:
    case SPICE_IMAGE_COMPRESSION_QUIC:
#ifdef USE_LZ4
    case SPICE_IMAGE_COMPRESSION_LZ4:
#endif
    case SPICE_IMAGE_COMPRESSION_LZ:
    case SPICE_IMAGE_COMPRESSION_GLZ:
    case SPICE_IMAGE_COMPRESSION_OFF:
        dcc->image_compression = pc->image_compression;
        break;
    default:
        spice_warning("preferred-compression: unsupported image compression setting");
    }
    return TRUE;
}

static int dcc_handle_gl_draw_done(DisplayChannelClient *dcc)
{
    DisplayChannel *display = DCC_TO_DC(dcc);

    if (G_UNLIKELY(!dcc->gl_draw_ongoing)) {
        g_warning("unexpected DRAW_DONE received\n");
        /* close client connection */
        return FALSE;
    }

    dcc->gl_draw_ongoing = FALSE;
    display_channel_gl_draw_done(display);

    return TRUE;
}

int dcc_handle_message(RedChannelClient *rcc, uint32_t size, uint16_t type, void *msg)
{
    DisplayChannelClient *dcc = RCC_TO_DCC(rcc);

    switch (type) {
    case SPICE_MSGC_DISPLAY_INIT:
        return dcc_handle_init(dcc, (SpiceMsgcDisplayInit *)msg);
    case SPICE_MSGC_DISPLAY_STREAM_REPORT:
        return dcc_handle_stream_report(dcc, (SpiceMsgcDisplayStreamReport *)msg);
    case SPICE_MSGC_DISPLAY_PREFERRED_COMPRESSION:
        return dcc_handle_preferred_compression(dcc,
            (SpiceMsgcDisplayPreferredCompression *)msg);
    case SPICE_MSGC_DISPLAY_GL_DRAW_DONE:
        return dcc_handle_gl_draw_done(dcc);
    default:
        return red_channel_client_handle_message(rcc, size, type, msg);
    }
}

static int dcc_handle_migrate_glz_dictionary(DisplayChannelClient *dcc,
                                             SpiceMigrateDataDisplay *migrate)
{
    spice_return_val_if_fail(!dcc->glz_dict, FALSE);

    ring_init(&dcc->glz_drawables);
    ring_init(&dcc->glz_drawables_inst_to_free);
    pthread_mutex_init(&dcc->glz_drawables_inst_to_free_lock, NULL);
    dcc->glz_dict = dcc_restore_glz_dictionary(dcc,
                                               migrate->glz_dict_id,
                                               &migrate->glz_dict_data);
    return dcc->glz_dict != NULL;
}

static int restore_surface(DisplayChannelClient *dcc, uint32_t surface_id)
{
    /* we don't process commands till we receive the migration data, thus,
     * we should have not sent any surface to the client. */
    if (dcc->surface_client_created[surface_id]) {
        spice_warning("surface %u is already marked as client_created", surface_id);
        return FALSE;
    }
    dcc->surface_client_created[surface_id] = TRUE;
    return TRUE;
}

static int restore_surfaces_lossless(DisplayChannelClient *dcc,
                                         MigrateDisplaySurfacesAtClientLossless *mig_surfaces)
{
    uint32_t i;

    spice_debug(NULL);
    for (i = 0; i < mig_surfaces->num_surfaces; i++) {
        uint32_t surface_id = mig_surfaces->surfaces[i].id;

        if (!restore_surface(dcc, surface_id))
            return FALSE;
    }
    return TRUE;
}

static int restore_surfaces_lossy(DisplayChannelClient *dcc,
                                  MigrateDisplaySurfacesAtClientLossy *mig_surfaces)
{
    uint32_t i;

    spice_debug(NULL);
    for (i = 0; i < mig_surfaces->num_surfaces; i++) {
        uint32_t surface_id = mig_surfaces->surfaces[i].id;
        SpiceMigrateDataRect *mig_lossy_rect;
        SpiceRect lossy_rect;

        if (!restore_surface(dcc, surface_id))
            return FALSE;

        mig_lossy_rect = &mig_surfaces->surfaces[i].lossy_rect;
        lossy_rect.left = mig_lossy_rect->left;
        lossy_rect.top = mig_lossy_rect->top;
        lossy_rect.right = mig_lossy_rect->right;
        lossy_rect.bottom = mig_lossy_rect->bottom;
        region_init(&dcc->surface_client_lossy_region[surface_id]);
        region_add(&dcc->surface_client_lossy_region[surface_id], &lossy_rect);
    }
    return TRUE;
}

int dcc_handle_migrate_data(DisplayChannelClient *dcc, uint32_t size, void *message)
{
    DisplayChannel *display = DCC_TO_DC(dcc);
    int surfaces_restored = FALSE;
    SpiceMigrateDataHeader *header = (SpiceMigrateDataHeader *)message;
    SpiceMigrateDataDisplay *migrate_data = (SpiceMigrateDataDisplay *)(header + 1);
    uint8_t *surfaces;
    int i;

    spice_return_val_if_fail(
        size >= (sizeof(*migrate_data) + sizeof(SpiceMigrateDataHeader)), FALSE);
    spice_return_val_if_fail(
         migration_protocol_validate_header(header,
             SPICE_MIGRATE_DATA_DISPLAY_MAGIC, SPICE_MIGRATE_DATA_DISPLAY_VERSION), FALSE);

    /* size is set to -1 in order to keep the cache frozen until the original
     * channel client that froze the cache on the src size receives the migrate
     * data and unfreezes the cache by setting its size > 0 and by triggering
     * pixmap_cache_reset */
    dcc->pixmap_cache = pixmap_cache_get(RED_CHANNEL_CLIENT(dcc)->client,
                                         migrate_data->pixmap_cache_id, -1);
    spice_return_val_if_fail(dcc->pixmap_cache, FALSE);

    pthread_mutex_lock(&dcc->pixmap_cache->lock);
    for (i = 0; i < MAX_CACHE_CLIENTS; i++) {
        dcc->pixmap_cache->sync[i] = MAX(dcc->pixmap_cache->sync[i],
                                         migrate_data->pixmap_cache_clients[i]);
    }
    pthread_mutex_unlock(&dcc->pixmap_cache->lock);

    if (migrate_data->pixmap_cache_freezer) {
        /* activating the cache. The cache will start to be active after
         * pixmap_cache_reset is called, when handling RED_PIPE_ITEM_TYPE_PIXMAP_RESET */
        dcc->pixmap_cache->size = migrate_data->pixmap_cache_size;
        red_channel_client_pipe_add_type(RED_CHANNEL_CLIENT(dcc), RED_PIPE_ITEM_TYPE_PIXMAP_RESET);
    }

    if (dcc_handle_migrate_glz_dictionary(dcc, migrate_data)) {
        dcc->glz =
            glz_encoder_create(dcc->id, dcc->glz_dict->dict, &dcc->glz_data.usr);
    } else {
        spice_critical("restoring global lz dictionary failed");
    }

    dcc->common.is_low_bandwidth = migrate_data->low_bandwidth_setting;

    if (migrate_data->low_bandwidth_setting) {
        red_channel_client_ack_set_client_window(RED_CHANNEL_CLIENT(dcc), WIDE_CLIENT_ACK_WINDOW);
        if (dcc->jpeg_state == SPICE_WAN_COMPRESSION_AUTO) {
            display->enable_jpeg = TRUE;
        }
        if (dcc->zlib_glz_state == SPICE_WAN_COMPRESSION_AUTO) {
            display->enable_zlib_glz_wrap = TRUE;
        }
    }

    surfaces = (uint8_t *)message + migrate_data->surfaces_at_client_ptr;
    surfaces_restored = display->enable_jpeg ?
        restore_surfaces_lossy(dcc, (MigrateDisplaySurfacesAtClientLossy *)surfaces) :
        restore_surfaces_lossless(dcc, (MigrateDisplaySurfacesAtClientLossless*)surfaces);

    spice_return_val_if_fail(surfaces_restored, FALSE);

    red_channel_client_pipe_add_type(RED_CHANNEL_CLIENT(dcc), RED_PIPE_ITEM_TYPE_INVAL_PALETTE_CACHE);
    /* enable sending messages */
    red_channel_client_ack_zero_messages_window(RED_CHANNEL_CLIENT(dcc));
    return TRUE;
}

static void release_item_after_push(RedPipeItem *item)
{
    switch (item->type) {
    case RED_PIPE_ITEM_TYPE_DRAW:
    case RED_PIPE_ITEM_TYPE_IMAGE:
    case RED_PIPE_ITEM_TYPE_STREAM_CLIP:
    case RED_PIPE_ITEM_TYPE_MONITORS_CONFIG:
    case RED_PIPE_ITEM_TYPE_UPGRADE:
        red_pipe_item_unref(item);
        break;
    case RED_PIPE_ITEM_TYPE_GL_SCANOUT:
    case RED_PIPE_ITEM_TYPE_GL_DRAW:
    case RED_PIPE_ITEM_TYPE_VERB:
        free(item);
        break;
    default:
        spice_critical("invalid item type");
    }
}

// TODO: share code between before/after_push since most of the items need the same
// release
static void release_item_before_push(DisplayChannelClient *dcc, RedPipeItem *item)
{
    DisplayChannel *display = DCC_TO_DC(dcc);

    spice_debug("item.type: %d", item->type);
    switch (item->type) {
    case RED_PIPE_ITEM_TYPE_DRAW: {
        RedDrawablePipeItem *dpi = SPICE_CONTAINEROF(item, RedDrawablePipeItem, dpi_pipe_item);
        ring_remove(&dpi->base);
        red_pipe_item_unref(item);
        break;
    }
    case RED_PIPE_ITEM_TYPE_STREAM_CREATE: {
        StreamAgent *agent = SPICE_CONTAINEROF(item, StreamAgent, create_item);
        stream_agent_unref(display, agent);
        break;
    }
    case RED_PIPE_ITEM_TYPE_STREAM_DESTROY: {
        StreamAgent *agent = SPICE_CONTAINEROF(item, StreamAgent, destroy_item);
        stream_agent_unref(display, agent);
        break;
    }
    case RED_PIPE_ITEM_TYPE_STREAM_CLIP:
    case RED_PIPE_ITEM_TYPE_UPGRADE:
    case RED_PIPE_ITEM_TYPE_IMAGE:
    case RED_PIPE_ITEM_TYPE_MONITORS_CONFIG:
        red_pipe_item_unref(item);
        break;
    case RED_PIPE_ITEM_TYPE_CREATE_SURFACE: {
        RedSurfaceCreateItem *surface_create = SPICE_CONTAINEROF(item, RedSurfaceCreateItem,
                                                                 pipe_item);
        free(surface_create);
        break;
    }
    case RED_PIPE_ITEM_TYPE_DESTROY_SURFACE: {
        RedSurfaceDestroyItem *surface_destroy = SPICE_CONTAINEROF(item, RedSurfaceDestroyItem,
                                                                   pipe_item);
        free(surface_destroy);
        break;
    }
    case RED_PIPE_ITEM_TYPE_INVAL_ONE:
    case RED_PIPE_ITEM_TYPE_VERB:
    case RED_PIPE_ITEM_TYPE_MIGRATE_DATA:
    case RED_PIPE_ITEM_TYPE_PIXMAP_SYNC:
    case RED_PIPE_ITEM_TYPE_PIXMAP_RESET:
    case RED_PIPE_ITEM_TYPE_INVAL_PALETTE_CACHE:
    case RED_PIPE_ITEM_TYPE_STREAM_ACTIVATE_REPORT:
    case RED_PIPE_ITEM_TYPE_GL_SCANOUT:
    case RED_PIPE_ITEM_TYPE_GL_DRAW:
        free(item);
        break;
    default:
        spice_critical("invalid item type");
    }
}

void dcc_release_item(DisplayChannelClient *dcc, RedPipeItem *item, int item_pushed)
{
    if (item_pushed)
        release_item_after_push(item);
    else
        release_item_before_push(dcc, item);
}
