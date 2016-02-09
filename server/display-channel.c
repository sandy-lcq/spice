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

#include "display-channel.h"

static void drawable_draw(DisplayChannel *display, Drawable *drawable);

uint32_t display_channel_generate_uid(DisplayChannel *display)
{
    spice_return_val_if_fail(display != NULL, 0);

    return ++display->bits_unique;
}

#define stat_start(stat, var)                                        \
    stat_start_time_t var; stat_start_time_init(&var, stat);

void display_channel_compress_stats_reset(DisplayChannel *display)
{
    spice_return_if_fail(display);

    stat_reset(&display->quic_stat);
    stat_reset(&display->lz_stat);
    stat_reset(&display->glz_stat);
    stat_reset(&display->jpeg_stat);
    stat_reset(&display->zlib_glz_stat);
    stat_reset(&display->jpeg_alpha_stat);
    stat_reset(&display->lz4_stat);
}

void display_channel_compress_stats_print(const DisplayChannel *display_channel)
{
    spice_return_if_fail(display_channel);

#ifdef COMPRESS_STAT
    uint64_t glz_enc_size;

    glz_enc_size = display_channel->enable_zlib_glz_wrap ?
                       display_channel->zlib_glz_stat.comp_size :
                       display_channel->glz_stat.comp_size;

    spice_info("==> Compression stats for display %u", display_channel->common.base.id);
    spice_info("Method   \t  count  \torig_size(MB)\tenc_size(MB)\tenc_time(s)");
    spice_info("QUIC     \t%8d\t%13.2f\t%12.2f\t%12.2f",
               display_channel->quic_stat.count,
               stat_byte_to_mega(display_channel->quic_stat.orig_size),
               stat_byte_to_mega(display_channel->quic_stat.comp_size),
               stat_cpu_time_to_sec(display_channel->quic_stat.total)
               );
    spice_info("GLZ      \t%8d\t%13.2f\t%12.2f\t%12.2f",
               display_channel->glz_stat.count,
               stat_byte_to_mega(display_channel->glz_stat.orig_size),
               stat_byte_to_mega(display_channel->glz_stat.comp_size),
               stat_cpu_time_to_sec(display_channel->glz_stat.total)
               );
    spice_info("ZLIB GLZ \t%8d\t%13.2f\t%12.2f\t%12.2f",
               display_channel->zlib_glz_stat.count,
               stat_byte_to_mega(display_channel->zlib_glz_stat.orig_size),
               stat_byte_to_mega(display_channel->zlib_glz_stat.comp_size),
               stat_cpu_time_to_sec(display_channel->zlib_glz_stat.total)
               );
    spice_info("LZ       \t%8d\t%13.2f\t%12.2f\t%12.2f",
               display_channel->lz_stat.count,
               stat_byte_to_mega(display_channel->lz_stat.orig_size),
               stat_byte_to_mega(display_channel->lz_stat.comp_size),
               stat_cpu_time_to_sec(display_channel->lz_stat.total)
               );
    spice_info("JPEG     \t%8d\t%13.2f\t%12.2f\t%12.2f",
               display_channel->jpeg_stat.count,
               stat_byte_to_mega(display_channel->jpeg_stat.orig_size),
               stat_byte_to_mega(display_channel->jpeg_stat.comp_size),
               stat_cpu_time_to_sec(display_channel->jpeg_stat.total)
               );
    spice_info("JPEG-RGBA\t%8d\t%13.2f\t%12.2f\t%12.2f",
               display_channel->jpeg_alpha_stat.count,
               stat_byte_to_mega(display_channel->jpeg_alpha_stat.orig_size),
               stat_byte_to_mega(display_channel->jpeg_alpha_stat.comp_size),
               stat_cpu_time_to_sec(display_channel->jpeg_alpha_stat.total)
               );
    spice_info("LZ4      \t%8d\t%13.2f\t%12.2f\t%12.2f",
               display_channel->lz4_stat.count,
               stat_byte_to_mega(display_channel->lz4_stat.orig_size),
               stat_byte_to_mega(display_channel->lz4_stat.comp_size),
               stat_cpu_time_to_sec(display_channel->lz4_stat.total)
               );
    spice_info("-------------------------------------------------------------------");
    spice_info("Total    \t%8d\t%13.2f\t%12.2f\t%12.2f",
               display_channel->lz_stat.count + display_channel->glz_stat.count +
                                                display_channel->quic_stat.count +
                                                display_channel->jpeg_stat.count +
                                                display_channel->lz4_stat.count +
                                                display_channel->jpeg_alpha_stat.count,
               stat_byte_to_mega(display_channel->lz_stat.orig_size +
                                 display_channel->glz_stat.orig_size +
                                 display_channel->quic_stat.orig_size +
                                 display_channel->jpeg_stat.orig_size +
                                 display_channel->lz4_stat.orig_size +
                                 display_channel->jpeg_alpha_stat.orig_size),
               stat_byte_to_mega(display_channel->lz_stat.comp_size +
                                 glz_enc_size +
                                 display_channel->quic_stat.comp_size +
                                 display_channel->jpeg_stat.comp_size +
                                 display_channel->lz4_stat.comp_size +
                                 display_channel->jpeg_alpha_stat.comp_size),
               stat_cpu_time_to_sec(display_channel->lz_stat.total +
                                    display_channel->glz_stat.total +
                                    display_channel->zlib_glz_stat.total +
                                    display_channel->quic_stat.total +
                                    display_channel->jpeg_stat.total +
                                    display_channel->lz4_stat.total +
                                    display_channel->jpeg_alpha_stat.total)
               );
#endif
}

MonitorsConfig* monitors_config_ref(MonitorsConfig *monitors_config)
{
    monitors_config->refs++;

    return monitors_config;
}

void monitors_config_unref(MonitorsConfig *monitors_config)
{
    if (!monitors_config) {
        return;
    }
    if (--monitors_config->refs != 0) {
        return;
    }

    spice_debug("freeing monitors config");
    free(monitors_config);
}

static void monitors_config_debug(MonitorsConfig *mc)
{
    int i;

    spice_debug("monitors config count:%d max:%d", mc->count, mc->max_allowed);
    for (i = 0; i < mc->count; i++)
        spice_debug("+%d+%d %dx%d",
                    mc->heads[i].x, mc->heads[i].y,
                    mc->heads[i].width, mc->heads[i].height);
}

MonitorsConfig* monitors_config_new(QXLHead *heads, ssize_t nheads, ssize_t max)
{
    MonitorsConfig *mc;

    mc = spice_malloc(sizeof(MonitorsConfig) + nheads * sizeof(QXLHead));
    mc->refs = 1;
    mc->count = nheads;
    mc->max_allowed = max;
    memcpy(mc->heads, heads, nheads * sizeof(QXLHead));
    monitors_config_debug(mc);

    return mc;
}

int display_channel_get_streams_timeout(DisplayChannel *display)
{
    int timeout = INT_MAX;
    Ring *ring = &display->streams;
    RingItem *item = ring;

    red_time_t now = spice_get_monotonic_time_ns();
    while ((item = ring_next(ring, item))) {
        Stream *stream;

        stream = SPICE_CONTAINEROF(item, Stream, link);
        red_time_t delta = (stream->last_time + RED_STREAM_TIMEOUT) - now;

        if (delta < 1000 * 1000) {
            return 0;
        }
        timeout = MIN(timeout, (unsigned int)(delta / (1000 * 1000)));
    }
    return timeout;
}

void display_channel_set_stream_video(DisplayChannel *display, int stream_video)
{
    spice_return_if_fail(display);
    spice_return_if_fail(stream_video != SPICE_STREAM_VIDEO_INVALID);

    switch (stream_video) {
    case SPICE_STREAM_VIDEO_ALL:
        spice_info("sv all");
        break;
    case SPICE_STREAM_VIDEO_FILTER:
        spice_info("sv filter");
        break;
    case SPICE_STREAM_VIDEO_OFF:
        spice_info("sv off");
        break;
    default:
        spice_warn_if_reached();
        return;
    }

    display->stream_video = stream_video;
}

static void stop_streams(DisplayChannel *display)
{
    Ring *ring = &display->streams;
    RingItem *item = ring_get_head(ring);

    while (item) {
        Stream *stream = SPICE_CONTAINEROF(item, Stream, link);
        item = ring_next(ring, item);
        if (!stream->current) {
            stream_stop(display, stream);
        } else {
            spice_info("attached stream");
        }
    }

    display->next_item_trace = 0;
    memset(display->items_trace, 0, sizeof(display->items_trace));
}

void display_channel_surface_unref(DisplayChannel *display, uint32_t surface_id)
{
    RedSurface *surface = &display->surfaces[surface_id];
    RedWorker *worker = COMMON_CHANNEL(display)->worker;
    QXLInstance *qxl = red_worker_get_qxl(worker);
    DisplayChannelClient *dcc;
    RingItem *link, *next;

    if (--surface->refs != 0) {
        return;
    }

    // only primary surface streams are supported
    if (is_primary_surface(display, surface_id)) {
        stop_streams(display);
    }
    spice_assert(surface->context.canvas);

    surface->context.canvas->ops->destroy(surface->context.canvas);
    if (surface->create.info) {
        qxl->st->qif->release_resource(qxl, surface->create);
    }
    if (surface->destroy.info) {
        qxl->st->qif->release_resource(qxl, surface->destroy);
    }

    region_destroy(&surface->draw_dirty_region);
    surface->context.canvas = NULL;
    FOREACH_DCC(display, link, next, dcc) {
        dcc_destroy_surface(dcc, surface_id);
    }

    spice_warn_if_fail(ring_is_empty(&surface->depend_on_me));
}

/* TODO: perhaps rename to "ready" or "realized" ? */
bool display_channel_surface_has_canvas(DisplayChannel *display,
                                        uint32_t surface_id)
{
    return display->surfaces[surface_id].context.canvas != NULL;
}

static void streams_update_visible_region(DisplayChannel *display, Drawable *drawable)
{
    Ring *ring;
    RingItem *item;
    RingItem *dcc_ring_item, *next;
    DisplayChannelClient *dcc;

    if (!red_channel_is_connected(RED_CHANNEL(display))) {
        return;
    }

    if (!is_primary_surface(display, drawable->surface_id)) {
        return;
    }

    ring = &display->streams;
    item = ring_get_head(ring);

    while (item) {
        Stream *stream = SPICE_CONTAINEROF(item, Stream, link);
        StreamAgent *agent;

        item = ring_next(ring, item);

        if (stream->current == drawable) {
            continue;
        }

        FOREACH_DCC(display, dcc_ring_item, next, dcc) {
            agent = &dcc->stream_agents[get_stream_id(display, stream)];

            if (region_intersects(&agent->vis_region, &drawable->tree_item.base.rgn)) {
                region_exclude(&agent->vis_region, &drawable->tree_item.base.rgn);
                region_exclude(&agent->clip, &drawable->tree_item.base.rgn);
                dcc_stream_agent_clip(dcc, agent);
            }
        }
    }
}

static void pipes_add_drawable(DisplayChannel *display, Drawable *drawable)
{
    DisplayChannelClient *dcc;
    RingItem *dcc_ring_item, *next;

    spice_warn_if_fail(ring_is_empty(&drawable->pipes));
    FOREACH_DCC(display, dcc_ring_item, next, dcc) {
        dcc_prepend_drawable(dcc, drawable);
    }
}

static void pipes_add_drawable_after(DisplayChannel *display,
                                     Drawable *drawable, Drawable *pos_after)
{
    DrawablePipeItem *dpi_pos_after;
    RingItem *dpi_link, *dpi_next;
    DisplayChannelClient *dcc;
    int num_other_linked = 0;

    DRAWABLE_FOREACH_DPI_SAFE(pos_after, dpi_link, dpi_next, dpi_pos_after) {
        num_other_linked++;
        dcc_add_drawable_after(dpi_pos_after->dcc, drawable, &dpi_pos_after->dpi_pipe_item);
    }
    if (num_other_linked == 0) {
        pipes_add_drawable(display, drawable);
        return;
    }
    if (num_other_linked != display->common.base.clients_num) {
        RingItem *item, *next;
        spice_debug("TODO: not O(n^2)");
        FOREACH_DCC(display, item, next, dcc) {
            int sent = 0;
            DRAWABLE_FOREACH_DPI_SAFE(pos_after, dpi_link, dpi_next, dpi_pos_after) {
                if (dpi_pos_after->dcc == dcc) {
                    sent = 1;
                    break;
                }
            }
            if (!sent) {
                dcc_prepend_drawable(dcc, drawable);
            }
        }
    }
}

static void current_add_drawable(DisplayChannel *display,
                                 Drawable *drawable, RingItem *pos)
{
    RedSurface *surface;
    uint32_t surface_id = drawable->surface_id;

    surface = &display->surfaces[surface_id];
    ring_add_after(&drawable->tree_item.base.siblings_link, pos);
    ring_add(&display->current_list, &drawable->list_link);
    ring_add(&surface->current_list, &drawable->surface_list_link);
    display->current_size++;
    drawable->refs++;
}

static void current_remove_drawable(DisplayChannel *display, Drawable *item)
{
    /* todo: move all to unref? */
    stream_trace_add_drawable(display, item);
    draw_item_remove_shadow(&item->tree_item);
    ring_remove(&item->tree_item.base.siblings_link);
    ring_remove(&item->list_link);
    ring_remove(&item->surface_list_link);
    display_channel_drawable_unref(display, item);
    display->current_size--;
}

static void drawable_remove_from_pipes(Drawable *drawable)
{
    DrawablePipeItem *dpi;
    RingItem *item, *next;

    RING_FOREACH_SAFE(item, next, &drawable->pipes) {
        dpi = SPICE_CONTAINEROF(item, DrawablePipeItem, base);
        if (pipe_item_is_linked(&dpi->dpi_pipe_item)) {
            red_channel_client_pipe_remove_and_release(RED_CHANNEL_CLIENT(dpi->dcc),
                                                       &dpi->dpi_pipe_item);
        }
    }
}

static void current_remove(DisplayChannel *display, TreeItem *item)
{
    TreeItem *now = item;

    /* depth-first tree traversal, TODO: do a to tree_foreach()? */
    for (;;) {
        Container *container = now->container;
        RingItem *ring_item;

        if (now->type == TREE_ITEM_TYPE_DRAWABLE) {
            Drawable *drawable = SPICE_CONTAINEROF(now, Drawable, tree_item.base);
            ring_item = now->siblings_link.prev;
            drawable_remove_from_pipes(drawable);
            current_remove_drawable(display, drawable);
        } else {
            Container *container = (Container *)now;

            spice_assert(now->type == TREE_ITEM_TYPE_CONTAINER);

            if ((ring_item = ring_get_head(&container->items))) {
                now = SPICE_CONTAINEROF(ring_item, TreeItem, siblings_link);
                continue;
            }
            ring_item = now->siblings_link.prev;
            container_free(container);
        }
        if (now == item) {
            return;
        }

        if ((ring_item = ring_next(&container->items, ring_item))) {
            now = SPICE_CONTAINEROF(ring_item, TreeItem, siblings_link);
        } else {
            now = (TreeItem *)container;
        }
    }
}

static void current_remove_all(DisplayChannel *display, int surface_id)
{
    Ring *ring = &display->surfaces[surface_id].current;
    RingItem *ring_item;

    while ((ring_item = ring_get_head(ring))) {
        TreeItem *now = SPICE_CONTAINEROF(ring_item, TreeItem, siblings_link);
        current_remove(display, now);
    }
}

static int current_add_equal(DisplayChannel *display, DrawItem *item, TreeItem *other)
{
    DrawItem *other_draw_item;
    Drawable *drawable;
    Drawable *other_drawable;

    if (other->type != TREE_ITEM_TYPE_DRAWABLE) {
        return FALSE;
    }
    other_draw_item = (DrawItem *)other;

    if (item->shadow || other_draw_item->shadow || item->effect != other_draw_item->effect) {
        return FALSE;
    }

    drawable = SPICE_CONTAINEROF(item, Drawable, tree_item);
    other_drawable = SPICE_CONTAINEROF(other_draw_item, Drawable, tree_item);

    if (item->effect == QXL_EFFECT_OPAQUE) {
        int add_after = !!other_drawable->stream &&
                        is_drawable_independent_from_surfaces(drawable);
        stream_maintenance(display, drawable, other_drawable);
        current_add_drawable(display, drawable, &other->siblings_link);
        other_drawable->refs++;
        current_remove_drawable(display, other_drawable);
        if (add_after) {
            pipes_add_drawable_after(display, drawable, other_drawable);
        } else {
            pipes_add_drawable(display, drawable);
        }
        drawable_remove_from_pipes(other_drawable);
        display_channel_drawable_unref(display, other_drawable);
        return TRUE;
    }

    switch (item->effect) {
    case QXL_EFFECT_REVERT_ON_DUP:
        if (is_same_drawable(drawable, other_drawable)) {

            DisplayChannelClient *dcc;
            DrawablePipeItem *dpi;
            RingItem *worker_ring_item, *dpi_ring_item;

            other_drawable->refs++;
            current_remove_drawable(display, other_drawable);

            /* sending the drawable to clients that already received
             * (or will receive) other_drawable */
            worker_ring_item = ring_get_head(&RED_CHANNEL(display)->clients);
            dpi_ring_item = ring_get_head(&other_drawable->pipes);
            /* dpi contains a sublist of dcc's, ordered the same */
            while (worker_ring_item) {
                dcc = SPICE_CONTAINEROF(worker_ring_item, DisplayChannelClient,
                                        common.base.channel_link);
                dpi = SPICE_CONTAINEROF(dpi_ring_item, DrawablePipeItem, base);
                while (worker_ring_item && (!dpi || dcc != dpi->dcc)) {
                    dcc_prepend_drawable(dcc, drawable);
                    worker_ring_item = ring_next(&RED_CHANNEL(display)->clients,
                                                 worker_ring_item);
                    dcc = SPICE_CONTAINEROF(worker_ring_item, DisplayChannelClient,
                                            common.base.channel_link);
                }

                if (dpi_ring_item) {
                    dpi_ring_item = ring_next(&other_drawable->pipes, dpi_ring_item);
                }
                if (worker_ring_item) {
                    worker_ring_item = ring_next(&RED_CHANNEL(display)->clients,
                                                 worker_ring_item);
                }
            }
            /* not sending other_drawable where possible */
            drawable_remove_from_pipes(other_drawable);

            display_channel_drawable_unref(display, other_drawable);
            return TRUE;
        }
        break;
    case QXL_EFFECT_OPAQUE_BRUSH:
        if (is_same_geometry(drawable, other_drawable)) {
            current_add_drawable(display, drawable, &other->siblings_link);
            drawable_remove_from_pipes(other_drawable);
            current_remove_drawable(display, other_drawable);
            pipes_add_drawable(display, drawable);
            return TRUE;
        }
        break;
    case QXL_EFFECT_NOP_ON_DUP:
        if (is_same_drawable(drawable, other_drawable)) {
            return TRUE;
        }
        break;
    }
    return FALSE;
}

static void __exclude_region(DisplayChannel *display, Ring *ring, TreeItem *item, QRegion *rgn,
                             Ring **top_ring, Drawable *frame_candidate)
{
    QRegion and_rgn;
    stat_start(&display->__exclude_stat, start_time);

    region_clone(&and_rgn, rgn);
    region_and(&and_rgn, &item->rgn);
    if (!region_is_empty(&and_rgn)) {
        if (IS_DRAW_ITEM(item)) {
            DrawItem *draw = (DrawItem *)item;

            if (draw->effect == QXL_EFFECT_OPAQUE) {
                region_exclude(rgn, &and_rgn);
            }

            if (draw->shadow) {
                Shadow *shadow;
                int32_t x = item->rgn.extents.x1;
                int32_t y = item->rgn.extents.y1;

                region_exclude(&draw->base.rgn, &and_rgn);
                shadow = draw->shadow;
                region_offset(&and_rgn, shadow->base.rgn.extents.x1 - x,
                              shadow->base.rgn.extents.y1 - y);
                region_exclude(&shadow->base.rgn, &and_rgn);
                region_and(&and_rgn, &shadow->on_hold);
                if (!region_is_empty(&and_rgn)) {
                    region_exclude(&shadow->on_hold, &and_rgn);
                    region_or(rgn, &and_rgn);
                    // in flat representation of current, shadow is always his owner next
                    if (!tree_item_contained_by((TreeItem*)shadow, *top_ring)) {
                        *top_ring = tree_item_container_items((TreeItem*)shadow, ring);
                    }
                }
            } else {
                if (frame_candidate) {
                    Drawable *drawable = SPICE_CONTAINEROF(draw, Drawable, tree_item);
                    stream_maintenance(display, frame_candidate, drawable);
                }
                region_exclude(&draw->base.rgn, &and_rgn);
            }
        } else if (item->type == TREE_ITEM_TYPE_CONTAINER) {
            region_exclude(&item->rgn, &and_rgn);

            if (region_is_empty(&item->rgn)) {  //assume container removal will follow
                Shadow *shadow;

                region_exclude(rgn, &and_rgn);
                if ((shadow = tree_item_find_shadow(item))) {
                    region_or(rgn, &shadow->on_hold);
                    if (!tree_item_contained_by((TreeItem*)shadow, *top_ring)) {
                        *top_ring = tree_item_container_items((TreeItem*)shadow, ring);
                    }
                }
            }
        } else {
            Shadow *shadow;

            spice_assert(item->type == TREE_ITEM_TYPE_SHADOW);
            shadow = (Shadow *)item;
            region_exclude(rgn, &and_rgn);
            region_or(&shadow->on_hold, &and_rgn);
        }
    }
    region_destroy(&and_rgn);
    stat_add(&display->__exclude_stat, start_time);
}

static void exclude_region(DisplayChannel *display, Ring *ring, RingItem *ring_item,
                           QRegion *rgn, TreeItem **last, Drawable *frame_candidate)
{
    Ring *top_ring;
    stat_start(&display->exclude_stat, start_time);

    if (!ring_item) {
        return;
    }

    top_ring = ring;

    for (;;) {
        TreeItem *now = SPICE_CONTAINEROF(ring_item, TreeItem, siblings_link);
        Container *container = now->container;

        spice_assert(!region_is_empty(&now->rgn));

        if (region_intersects(rgn, &now->rgn)) {
            __exclude_region(display, ring, now, rgn, &top_ring, frame_candidate);

            if (region_is_empty(&now->rgn)) {
                spice_assert(now->type != TREE_ITEM_TYPE_SHADOW);
                ring_item = now->siblings_link.prev;
                current_remove(display, now);
                if (last && *last == now) {
                    *last = (TreeItem *)ring_next(ring, ring_item);
                }
            } else if (now->type == TREE_ITEM_TYPE_CONTAINER) {
                Container *container = (Container *)now;
                if ((ring_item = ring_get_head(&container->items))) {
                    ring = &container->items;
                    spice_assert(((TreeItem *)ring_item)->container);
                    continue;
                }
                ring_item = &now->siblings_link;
            }

            if (region_is_empty(rgn)) {
                stat_add(&display->exclude_stat, start_time);
                return;
            }
        }

        while ((last && *last == (TreeItem *)ring_item) ||
               !(ring_item = ring_next(ring, ring_item))) {
            if (ring == top_ring) {
                stat_add(&display->exclude_stat, start_time);
                return;
            }
            ring_item = &container->base.siblings_link;
            container = container->base.container;
            ring = (container) ? &container->items : top_ring;
        }
    }
}

static int current_add_with_shadow(DisplayChannel *display, Ring *ring, Drawable *item)
{
    stat_start(&display->add_stat, start_time);
#ifdef RED_WORKER_STAT
    ++display->add_with_shadow_count;
#endif

    RedDrawable *red_drawable = item->red_drawable;
    SpicePoint delta = {
        .x = red_drawable->u.copy_bits.src_pos.x - red_drawable->bbox.left,
        .y = red_drawable->u.copy_bits.src_pos.y - red_drawable->bbox.top
    };

    Shadow *shadow = shadow_new(&item->tree_item, &delta);
    if (!shadow) {
        stat_add(&display->add_stat, start_time);
        return FALSE;
    }
    // item and his shadow must initially be placed in the same container.
    // for now putting them on root.

    // only primary surface streams are supported
    if (is_primary_surface(display, item->surface_id)) {
        stream_detach_behind(display, &shadow->base.rgn, NULL);
    }

    ring_add(ring, &shadow->base.siblings_link);
    current_add_drawable(display, item, ring);
    if (item->tree_item.effect == QXL_EFFECT_OPAQUE) {
        QRegion exclude_rgn;
        region_clone(&exclude_rgn, &item->tree_item.base.rgn);
        exclude_region(display, ring, &shadow->base.siblings_link, &exclude_rgn, NULL, NULL);
        region_destroy(&exclude_rgn);
        streams_update_visible_region(display, item);
    } else {
        if (is_primary_surface(display, item->surface_id)) {
            stream_detach_behind(display, &item->tree_item.base.rgn, item);
        }
    }
    stat_add(&display->add_stat, start_time);
    return TRUE;
}

static int current_add(DisplayChannel *display, Ring *ring, Drawable *drawable)
{
    DrawItem *item = &drawable->tree_item;
    RingItem *now;
    QRegion exclude_rgn;
    RingItem *exclude_base = NULL;
    stat_start(&display->add_stat, start_time);

    spice_assert(!region_is_empty(&item->base.rgn));
    region_init(&exclude_rgn);
    now = ring_next(ring, ring);

    while (now) {
        TreeItem *sibling = SPICE_CONTAINEROF(now, TreeItem, siblings_link);
        int test_res;

        if (!region_bounds_intersects(&item->base.rgn, &sibling->rgn)) {
            now = ring_next(ring, now);
            continue;
        }
        test_res = region_test(&item->base.rgn, &sibling->rgn, REGION_TEST_ALL);
        if (!(test_res & REGION_TEST_SHARED)) {
            now = ring_next(ring, now);
            continue;
        } else if (sibling->type != TREE_ITEM_TYPE_SHADOW) {
            if (!(test_res & REGION_TEST_RIGHT_EXCLUSIVE) &&
                                                   !(test_res & REGION_TEST_LEFT_EXCLUSIVE) &&
                                                   current_add_equal(display, item, sibling)) {
                stat_add(&display->add_stat, start_time);
                return FALSE;
            }

            if (!(test_res & REGION_TEST_RIGHT_EXCLUSIVE) && item->effect == QXL_EFFECT_OPAQUE) {
                Shadow *shadow;
                int skip = now == exclude_base;

                if ((shadow = tree_item_find_shadow(sibling))) {
                    if (exclude_base) {
                        TreeItem *next = sibling;
                        exclude_region(display, ring, exclude_base, &exclude_rgn, &next, NULL);
                        if (next != sibling) {
                            now = next ? &next->siblings_link : NULL;
                            exclude_base = NULL;
                            continue;
                        }
                    }
                    region_or(&exclude_rgn, &shadow->on_hold);
                }
                now = now->prev;
                current_remove(display, sibling);
                now = ring_next(ring, now);
                if (shadow || skip) {
                    exclude_base = now;
                }
                continue;
            }

            if (!(test_res & REGION_TEST_LEFT_EXCLUSIVE) && is_opaque_item(sibling)) {
                Container *container;

                if (exclude_base) {
                    exclude_region(display, ring, exclude_base, &exclude_rgn, NULL, NULL);
                    region_clear(&exclude_rgn);
                    exclude_base = NULL;
                }
                if (sibling->type == TREE_ITEM_TYPE_CONTAINER) {
                    container = (Container *)sibling;
                    ring = &container->items;
                    item->base.container = container;
                    now = ring_next(ring, ring);
                    continue;
                }
                spice_assert(IS_DRAW_ITEM(sibling));
                if (!DRAW_ITEM(sibling)->container_root) {
                    container = container_new(DRAW_ITEM(sibling));
                    if (!container) {
                        spice_warning("create new container failed");
                        region_destroy(&exclude_rgn);
                        return FALSE;
                    }
                    item->base.container = container;
                    ring = &container->items;
                }
            }
        }
        if (!exclude_base) {
            exclude_base = now;
        }
        break;
    }
    if (item->effect == QXL_EFFECT_OPAQUE) {
        region_or(&exclude_rgn, &item->base.rgn);
        exclude_region(display, ring, exclude_base, &exclude_rgn, NULL, drawable);
        stream_trace_update(display, drawable);
        streams_update_visible_region(display, drawable);
        /*
         * Performing the insertion after exclude_region for
         * safety (todo: Not sure if exclude_region can affect the drawable
         * if it is added to the tree before calling exclude_region).
         */
        current_add_drawable(display, drawable, ring);
    } else {
        /*
         * stream_detach_behind can affect the current tree since
         * it may trigger calls to display_channel_draw. Thus, the
         * drawable should be added to the tree before calling
         * stream_detach_behind
         */
        current_add_drawable(display, drawable, ring);
        if (is_primary_surface(display, drawable->surface_id)) {
            stream_detach_behind(display, &drawable->tree_item.base.rgn, drawable);
        }
    }
    region_destroy(&exclude_rgn);
    stat_add(&display->add_stat, start_time);
    return TRUE;
}

static bool drawable_can_stream(DisplayChannel *display, Drawable *drawable)
{
    RedDrawable *red_drawable = drawable->red_drawable;
    SpiceImage *image;

    if (display->stream_video == SPICE_STREAM_VIDEO_OFF) {
        return FALSE;
    }

    if (!is_primary_surface(display, drawable->surface_id)) {
        return FALSE;
    }

    if (drawable->tree_item.effect != QXL_EFFECT_OPAQUE ||
        red_drawable->type != QXL_DRAW_COPY ||
        red_drawable->u.copy.rop_descriptor != SPICE_ROPD_OP_PUT) {
        return FALSE;
    }

    image = red_drawable->u.copy.src_bitmap;
    if (image == NULL ||
        image->descriptor.type != SPICE_IMAGE_TYPE_BITMAP) {
        return FALSE;
    }

    if (display->stream_video == SPICE_STREAM_VIDEO_FILTER) {
        SpiceRect* rect;
        int size;

        rect = &drawable->red_drawable->u.copy.src_area;
        size = (rect->right - rect->left) * (rect->bottom - rect->top);
        if (size < RED_STREAM_MIN_SIZE) {
            return FALSE;
        }
    }

    return TRUE;
}

void display_channel_print_stats(DisplayChannel *display)
{
#ifdef RED_WORKER_STAT
    stat_time_t total = display->add_stat.total;
    spice_info("add with shadow count %u",
               display->add_with_shadow_count);
    display->add_with_shadow_count = 0;
    spice_info("add[%u] %f exclude[%u] %f __exclude[%u] %f",
               display->add_stat.count,
               stat_cpu_time_to_sec(total),
               display->exclude_stat.count,
               stat_cpu_time_to_sec(display->exclude_stat.total),
               display->__exclude_stat.count,
               stat_cpu_time_to_sec(display->__exclude_stat.total));
    spice_info("add %f%% exclude %f%% exclude2 %f%% __exclude %f%%",
               (double)(total - display->exclude_stat.total) / total * 100,
               (double)(display->exclude_stat.total) / total * 100,
               (double)(display->exclude_stat.total -
                        display->__exclude_stat.total) / display->exclude_stat.total * 100,
               (double)(display->__exclude_stat.total) / display->exclude_stat.total * 100);
    stat_reset(&display->add_stat);
    stat_reset(&display->exclude_stat);
    stat_reset(&display->__exclude_stat);
#endif
}

static void drawable_ref_surface_deps(DisplayChannel *display, Drawable *drawable)
{
    int x;
    int surface_id;
    RedSurface *surface;

    for (x = 0; x < 3; ++x) {
        surface_id = drawable->surface_deps[x];
        if (surface_id == -1) {
            continue;
        }
        surface = &display->surfaces[surface_id];
        surface->refs++;
    }
}

static void surface_read_bits(DisplayChannel *display, int surface_id,
                              const SpiceRect *area, uint8_t *dest, int dest_stride)
{
    SpiceCanvas *canvas;
    RedSurface *surface = &display->surfaces[surface_id];

    canvas = surface->context.canvas;
    canvas->ops->read_bits(canvas, dest, dest_stride, area);
}

static void handle_self_bitmap(DisplayChannel *display, Drawable *drawable)
{
    RedDrawable *red_drawable = drawable->red_drawable;
    SpiceImage *image;
    int32_t width;
    int32_t height;
    uint8_t *dest;
    int dest_stride;
    RedSurface *surface;
    int bpp;
    int all_set;

    surface = &display->surfaces[drawable->surface_id];

    bpp = SPICE_SURFACE_FMT_DEPTH(surface->context.format) / 8;
    width = red_drawable->self_bitmap_area.right - red_drawable->self_bitmap_area.left;
    height = red_drawable->self_bitmap_area.bottom - red_drawable->self_bitmap_area.top;
    dest_stride = SPICE_ALIGN(width * bpp, 4);

    image = spice_new0(SpiceImage, 1);
    image->descriptor.type = SPICE_IMAGE_TYPE_BITMAP;
    image->descriptor.flags = 0;

    QXL_SET_IMAGE_ID(image, QXL_IMAGE_GROUP_RED, display_channel_generate_uid(display));
    image->u.bitmap.flags = surface->context.top_down ? SPICE_BITMAP_FLAGS_TOP_DOWN : 0;
    image->u.bitmap.format = spice_bitmap_from_surface_type(surface->context.format);
    image->u.bitmap.stride = dest_stride;
    image->descriptor.width = image->u.bitmap.x = width;
    image->descriptor.height = image->u.bitmap.y = height;
    image->u.bitmap.palette = NULL;

    dest = (uint8_t *)spice_malloc_n(height, dest_stride);
    image->u.bitmap.data = spice_chunks_new_linear(dest, height * dest_stride);
    image->u.bitmap.data->flags |= SPICE_CHUNKS_FLAGS_FREE;

    display_channel_draw(display, &red_drawable->self_bitmap_area, drawable->surface_id);
    surface_read_bits(display, drawable->surface_id,
        &red_drawable->self_bitmap_area, dest, dest_stride);

    /* For 32bit non-primary surfaces we need to keep any non-zero
       high bytes as the surface may be used as source to an alpha_blend */
    if (!is_primary_surface(display, drawable->surface_id) &&
        image->u.bitmap.format == SPICE_BITMAP_FMT_32BIT &&
        rgb32_data_has_alpha(width, height, dest_stride, dest, &all_set)) {
        if (all_set) {
            image->descriptor.flags |= SPICE_IMAGE_FLAGS_HIGH_BITS_SET;
        } else {
            image->u.bitmap.format = SPICE_BITMAP_FMT_RGBA;
        }
    }

    red_drawable->self_bitmap_image = image;
}

static void surface_add_reverse_dependency(DisplayChannel *display, int surface_id,
                                             DependItem *depend_item, Drawable *drawable)
{
    RedSurface *surface;

    if (surface_id == -1) {
        depend_item->drawable = NULL;
        return;
    }

    surface = &display->surfaces[surface_id];

    depend_item->drawable = drawable;
    ring_add(&surface->depend_on_me, &depend_item->ring_item);
}

static int handle_surface_deps(DisplayChannel *display, Drawable *drawable)
{
    int x;

    for (x = 0; x < 3; ++x) {
        // surface self dependency is handled by shadows in "current", or by
        // handle_self_bitmap
        if (drawable->surface_deps[x] != drawable->surface_id) {
            surface_add_reverse_dependency(display, drawable->surface_deps[x],
                                      &drawable->depend_items[x], drawable);

            if (drawable->surface_deps[x] == 0) {
                QRegion depend_region;
                region_init(&depend_region);
                region_add(&depend_region, &drawable->red_drawable->surfaces_rects[x]);
                stream_detach_behind(display, &depend_region, NULL);
            }
        }
    }

    return TRUE;
}

static void draw_depend_on_me(DisplayChannel *display, uint32_t surface_id)
{
    RedSurface *surface;
    RingItem *ring_item;

    surface = &display->surfaces[surface_id];

    while ((ring_item = ring_get_tail(&surface->depend_on_me))) {
        Drawable *drawable;
        DependItem *depended_item = SPICE_CONTAINEROF(ring_item, DependItem, ring_item);
        drawable = depended_item->drawable;
        display_channel_draw(display, &drawable->red_drawable->bbox, drawable->surface_id);
    }
}

static int validate_drawable_bbox(DisplayChannel *display, RedDrawable *drawable)
{
        DrawContext *context;
        uint32_t surface_id = drawable->surface_id;

        /* surface_id must be validated before calling into
         * validate_drawable_bbox
         */
        if (!validate_surface(display, drawable->surface_id)) {
            return FALSE;
        }
        context = &display->surfaces[surface_id].context;

        if (drawable->bbox.top < 0)
                return FALSE;
        if (drawable->bbox.left < 0)
                return FALSE;
        if (drawable->bbox.bottom < 0)
                return FALSE;
        if (drawable->bbox.right < 0)
                return FALSE;
        if (drawable->bbox.bottom > context->height)
                return FALSE;
        if (drawable->bbox.right > context->width)
                return FALSE;

        return TRUE;
}

/**
 * @brief Get a new Drawable
 *
 * The Drawable returned is fully initialized.
 *
 * @return initialized Drawable or NULL on failure
 */
static Drawable *display_channel_get_drawable(DisplayChannel *display, uint8_t effect,
                                              RedDrawable *red_drawable, uint32_t group_id,
                                              uint32_t process_commands_generation)
{
    Drawable *drawable;
    int x;

    /* Validate all surface ids before updating counters
     * to avoid invalid updates if we find an invalid id.
     */
    if (!validate_drawable_bbox(display, red_drawable)) {
        return NULL;
    }
    for (x = 0; x < 3; ++x) {
        if (red_drawable->surface_deps[x] != -1
            && !validate_surface(display, red_drawable->surface_deps[x])) {
            return NULL;
        }
    }

    drawable = display_channel_drawable_try_new(display, group_id, process_commands_generation);
    if (!drawable) {
        return NULL;
    }

    drawable->tree_item.effect = effect;
    drawable->red_drawable = red_drawable_ref(red_drawable);

    drawable->surface_id = red_drawable->surface_id;
    display->surfaces[drawable->surface_id].refs++;

    memcpy(drawable->surface_deps, red_drawable->surface_deps, sizeof(drawable->surface_deps));
    /*
        surface->refs is affected by a drawable (that is
        dependent on the surface) as long as the drawable is alive.
        However, surface->depend_on_me is affected by a drawable only
        as long as it is in the current tree (hasn't been rendered yet).
    */
    drawable_ref_surface_deps(display, drawable);

    return drawable;
}

/**
 * Add a Drawable to the items to draw.
 * On failure the Drawable is not added.
 */
static void display_channel_add_drawable(DisplayChannel *display, Drawable *drawable)
{
    int surface_id = drawable->surface_id;
    RedDrawable *red_drawable = drawable->red_drawable;

    red_drawable->mm_time = reds_get_mm_time();

    region_add(&drawable->tree_item.base.rgn, &red_drawable->bbox);

    if (red_drawable->clip.type == SPICE_CLIP_TYPE_RECTS) {
        QRegion rgn;

        region_init(&rgn);
        region_add_clip_rects(&rgn, red_drawable->clip.rects);
        region_and(&drawable->tree_item.base.rgn, &rgn);
        region_destroy(&rgn);
    }

    if (region_is_empty(&drawable->tree_item.base.rgn)) {
        return;
    }

    if (red_drawable->self_bitmap) {
        handle_self_bitmap(display, drawable);
    }

    draw_depend_on_me(display, surface_id);

    if (!handle_surface_deps(display, drawable)) {
        return;
    }

    Ring *ring = &display->surfaces[surface_id].current;
    int add_to_pipe;
    if (has_shadow(red_drawable)) {
        add_to_pipe = current_add_with_shadow(display, ring, drawable);
    } else {
        drawable->streamable = drawable_can_stream(display, drawable);
        add_to_pipe = current_add(display, ring, drawable);
    }

    if (add_to_pipe)
        pipes_add_drawable(display, drawable);

#ifdef RED_WORKER_STAT
    if ((++display->add_count % 100) == 0)
        display_channel_print_stats(display);
#endif
}

void display_channel_process_draw(DisplayChannel *display, RedDrawable *red_drawable,
                                  uint32_t group_id, int process_commands_generation)
{
    Drawable *drawable =
        display_channel_get_drawable(display, red_drawable->effect, red_drawable, group_id,
                                     process_commands_generation);

    if (!drawable) {
        return;
    }

    display_channel_add_drawable(display, drawable);

    display_channel_drawable_unref(display, drawable);
}

int display_channel_wait_for_migrate_data(DisplayChannel *display)
{
    uint64_t end_time = spice_get_monotonic_time_ns() + DISPLAY_CLIENT_MIGRATE_DATA_TIMEOUT;
    RedChannel *channel = &display->common.base;
    RedChannelClient *rcc;
    int ret = FALSE;

    if (!red_channel_is_waiting_for_migrate_data(&display->common.base)) {
        return FALSE;
    }

    spice_debug(NULL);
    spice_warn_if_fail(channel->clients_num == 1);

    rcc = SPICE_CONTAINEROF(ring_get_head(&channel->clients), RedChannelClient, channel_link);

    red_channel_client_ref(rcc);
    for (;;) {
        red_channel_client_receive(rcc);
        if (!red_channel_client_is_connected(rcc)) {
            break;
        }

        if (!red_channel_client_is_waiting_for_migrate_data(rcc)) {
            ret = TRUE;
            break;
        }
        if (spice_get_monotonic_time_ns() > end_time) {
            spice_warning("timeout");
            red_channel_client_disconnect(rcc);
            break;
        }
        usleep(DISPLAY_CLIENT_RETRY_INTERVAL);
    }
    red_channel_client_unref(rcc);
    return ret;
}

void display_channel_flush_all_surfaces(DisplayChannel *display)
{
    int x;

    for (x = 0; x < NUM_SURFACES; ++x) {
        if (display->surfaces[x].context.canvas) {
            display_channel_current_flush(display, x);
        }
    }
}

void display_channel_free_glz_drawables_to_free(DisplayChannel *display)
{
    RingItem *link, *next;
    DisplayChannelClient *dcc;

    spice_return_if_fail(display);

    DCC_FOREACH_SAFE(link, next, dcc, RED_CHANNEL(display)) {
        dcc_free_glz_drawables_to_free(dcc);
    }
}

void display_channel_free_glz_drawables(DisplayChannel *display)
{
    RingItem *link, *next;
    DisplayChannelClient *dcc;

    spice_return_if_fail(display);

    DCC_FOREACH_SAFE(link, next, dcc, RED_CHANNEL(display)) {
        dcc_free_glz_drawables(dcc);
    }
}

static bool free_one_drawable(DisplayChannel *display, int force_glz_free)
{
    RingItem *ring_item = ring_get_tail(&display->current_list);
    Drawable *drawable;
    Container *container;

    if (!ring_item) {
        return FALSE;
    }

    drawable = SPICE_CONTAINEROF(ring_item, Drawable, list_link);
    if (force_glz_free) {
        RingItem *glz_item, *next_item;
        RedGlzDrawable *glz;
        DRAWABLE_FOREACH_GLZ_SAFE(drawable, glz_item, next_item, glz) {
            dcc_free_glz_drawable(glz->dcc, glz);
        }
    }
    drawable_draw(display, drawable);
    container = drawable->tree_item.base.container;

    current_remove_drawable(display, drawable);
    container_cleanup(container);
    return TRUE;
}

void display_channel_current_flush(DisplayChannel *display, int surface_id)
{
    while (!ring_is_empty(&display->surfaces[surface_id].current_list)) {
        free_one_drawable(display, FALSE);
    }
    current_remove_all(display, surface_id);
}

void display_channel_free_some(DisplayChannel *display)
{
    int n = 0;
    DisplayChannelClient *dcc;
    RingItem *item, *next;

    spice_debug("#draw=%d, #red_draw=%d, #glz_draw=%d", display->drawable_count,
                display->red_drawable_count, display->glz_drawable_count);
    FOREACH_DCC(display, item, next, dcc) {
        GlzSharedDictionary *glz_dict = dcc ? dcc->glz_dict : NULL;

        if (glz_dict) {
            // encoding using the dictionary is prevented since the following operations might
            // change the dictionary
            pthread_rwlock_wrlock(&glz_dict->encode_lock);
            n = dcc_free_some_independent_glz_drawables(dcc);
        }
    }

    while (!ring_is_empty(&display->current_list) && n++ < RED_RELEASE_BUNCH_SIZE) {
        free_one_drawable(display, TRUE);
    }

    FOREACH_DCC(display, item, next, dcc) {
        GlzSharedDictionary *glz_dict = dcc ? dcc->glz_dict : NULL;

        if (glz_dict) {
            pthread_rwlock_unlock(&glz_dict->encode_lock);
        }
    }
}

static Drawable* drawable_try_new(DisplayChannel *display)
{
    Drawable *drawable;

    if (!display->free_drawables)
        return NULL;

    drawable = &display->free_drawables->u.drawable;
    display->free_drawables = display->free_drawables->u.next;
    display->drawable_count++;

    return drawable;
}

static void drawable_free(DisplayChannel *display, Drawable *drawable)
{
    ((_Drawable *)drawable)->u.next = display->free_drawables;
    display->free_drawables = (_Drawable *)drawable;
}

static void drawables_init(DisplayChannel *display)
{
    int i;

    display->free_drawables = NULL;
    for (i = 0; i < NUM_DRAWABLES; i++) {
        drawable_free(display, &display->drawables[i].u.drawable);
    }
}

/**
 * Allocate a Drawable
 *
 * @return pointer to uninitialized Drawable or NULL on failure
 */
Drawable *display_channel_drawable_try_new(DisplayChannel *display,
                                           int group_id, int process_commands_generation)
{
    Drawable *drawable;

    while (!(drawable = drawable_try_new(display))) {
        if (!free_one_drawable(display, FALSE))
            return NULL;
    }

    bzero(drawable, sizeof(Drawable));
    drawable->refs = 1;
    drawable->creation_time = drawable->first_frame_time = spice_get_monotonic_time_ns();
    ring_item_init(&drawable->list_link);
    ring_item_init(&drawable->surface_list_link);
    ring_item_init(&drawable->tree_item.base.siblings_link);
    drawable->tree_item.base.type = TREE_ITEM_TYPE_DRAWABLE;
    region_init(&drawable->tree_item.base.rgn);
    ring_init(&drawable->pipes);
    ring_init(&drawable->glz_ring);
    drawable->process_commands_generation = process_commands_generation;
    drawable->group_id = group_id;

    return drawable;
}

static void depended_item_remove(DependItem *item)
{
    spice_return_if_fail(item->drawable);
    spice_return_if_fail(ring_item_is_linked(&item->ring_item));

    item->drawable = NULL;
    ring_remove(&item->ring_item);
}

static void drawable_remove_dependencies(DisplayChannel *display, Drawable *drawable)
{
    int x;
    int surface_id;

    for (x = 0; x < 3; ++x) {
        surface_id = drawable->surface_deps[x];
        if (surface_id != -1 && drawable->depend_items[x].drawable) {
            depended_item_remove(&drawable->depend_items[x]);
        }
    }
}

static void drawable_unref_surface_deps(DisplayChannel *display, Drawable *drawable)
{
    int x;
    int surface_id;

    for (x = 0; x < 3; ++x) {
        surface_id = drawable->surface_deps[x];
        if (surface_id == -1) {
            continue;
        }
        display_channel_surface_unref(display, surface_id);
    }
}

void display_channel_drawable_unref(DisplayChannel *display, Drawable *drawable)
{
    RingItem *item, *next;

    if (--drawable->refs != 0)
        return;

    spice_warn_if_fail(!drawable->tree_item.shadow);
    spice_warn_if_fail(ring_is_empty(&drawable->pipes));

    if (drawable->stream) {
        detach_stream(display, drawable->stream, TRUE);
    }
    region_destroy(&drawable->tree_item.base.rgn);

    drawable_remove_dependencies(display, drawable);
    drawable_unref_surface_deps(display, drawable);
    display_channel_surface_unref(display, drawable->surface_id);

    RING_FOREACH_SAFE(item, next, &drawable->glz_ring) {
        SPICE_CONTAINEROF(item, RedGlzDrawable, drawable_link)->drawable = NULL;
        ring_remove(item);
    }
    if (drawable->red_drawable) {
        red_drawable_unref(COMMON_CHANNEL(display)->worker, drawable->red_drawable, drawable->group_id);
    }
    drawable_free(display, drawable);
    display->drawable_count--;
}

static void drawable_deps_draw(DisplayChannel *display, Drawable *drawable)
{
    int x;
    int surface_id;

    for (x = 0; x < 3; ++x) {
        surface_id = drawable->surface_deps[x];
        if (surface_id != -1 && drawable->depend_items[x].drawable) {
            depended_item_remove(&drawable->depend_items[x]);
            display_channel_draw(display, &drawable->red_drawable->surfaces_rects[x], surface_id);
        }
    }
}

static void drawable_draw(DisplayChannel *display, Drawable *drawable)
{
    RedSurface *surface;
    SpiceCanvas *canvas;
    SpiceClip clip = drawable->red_drawable->clip;

    drawable_deps_draw(display, drawable);

    surface = &display->surfaces[drawable->surface_id];
    canvas = surface->context.canvas;
    spice_return_if_fail(canvas);

    image_cache_aging(&display->image_cache);

    region_add(&surface->draw_dirty_region, &drawable->red_drawable->bbox);

    switch (drawable->red_drawable->type) {
    case QXL_DRAW_FILL: {
        SpiceFill fill = drawable->red_drawable->u.fill;
        SpiceImage img1, img2;
        image_cache_localize_brush(&display->image_cache, &fill.brush, &img1);
        image_cache_localize_mask(&display->image_cache, &fill.mask, &img2);
        canvas->ops->draw_fill(canvas, &drawable->red_drawable->bbox,
                               &clip, &fill);
        break;
    }
    case QXL_DRAW_OPAQUE: {
        SpiceOpaque opaque = drawable->red_drawable->u.opaque;
        SpiceImage img1, img2, img3;
        image_cache_localize_brush(&display->image_cache, &opaque.brush, &img1);
        image_cache_localize(&display->image_cache, &opaque.src_bitmap, &img2, drawable);
        image_cache_localize_mask(&display->image_cache, &opaque.mask, &img3);
        canvas->ops->draw_opaque(canvas, &drawable->red_drawable->bbox, &clip, &opaque);
        break;
    }
    case QXL_DRAW_COPY: {
        SpiceCopy copy = drawable->red_drawable->u.copy;
        SpiceImage img1, img2;
        image_cache_localize(&display->image_cache, &copy.src_bitmap, &img1, drawable);
        image_cache_localize_mask(&display->image_cache, &copy.mask, &img2);
        canvas->ops->draw_copy(canvas, &drawable->red_drawable->bbox,
                               &clip, &copy);
        break;
    }
    case QXL_DRAW_TRANSPARENT: {
        SpiceTransparent transparent = drawable->red_drawable->u.transparent;
        SpiceImage img1;
        image_cache_localize(&display->image_cache, &transparent.src_bitmap, &img1, drawable);
        canvas->ops->draw_transparent(canvas,
                                      &drawable->red_drawable->bbox, &clip, &transparent);
        break;
    }
    case QXL_DRAW_ALPHA_BLEND: {
        SpiceAlphaBlend alpha_blend = drawable->red_drawable->u.alpha_blend;
        SpiceImage img1;
        image_cache_localize(&display->image_cache, &alpha_blend.src_bitmap, &img1, drawable);
        canvas->ops->draw_alpha_blend(canvas,
                                      &drawable->red_drawable->bbox, &clip, &alpha_blend);
        break;
    }
    case QXL_COPY_BITS: {
        canvas->ops->copy_bits(canvas, &drawable->red_drawable->bbox,
                               &clip, &drawable->red_drawable->u.copy_bits.src_pos);
        break;
    }
    case QXL_DRAW_BLEND: {
        SpiceBlend blend = drawable->red_drawable->u.blend;
        SpiceImage img1, img2;
        image_cache_localize(&display->image_cache, &blend.src_bitmap, &img1, drawable);
        image_cache_localize_mask(&display->image_cache, &blend.mask, &img2);
        canvas->ops->draw_blend(canvas, &drawable->red_drawable->bbox,
                                &clip, &blend);
        break;
    }
    case QXL_DRAW_BLACKNESS: {
        SpiceBlackness blackness = drawable->red_drawable->u.blackness;
        SpiceImage img1;
        image_cache_localize_mask(&display->image_cache, &blackness.mask, &img1);
        canvas->ops->draw_blackness(canvas,
                                    &drawable->red_drawable->bbox, &clip, &blackness);
        break;
    }
    case QXL_DRAW_WHITENESS: {
        SpiceWhiteness whiteness = drawable->red_drawable->u.whiteness;
        SpiceImage img1;
        image_cache_localize_mask(&display->image_cache, &whiteness.mask, &img1);
        canvas->ops->draw_whiteness(canvas,
                                    &drawable->red_drawable->bbox, &clip, &whiteness);
        break;
    }
    case QXL_DRAW_INVERS: {
        SpiceInvers invers = drawable->red_drawable->u.invers;
        SpiceImage img1;
        image_cache_localize_mask(&display->image_cache, &invers.mask, &img1);
        canvas->ops->draw_invers(canvas,
                                 &drawable->red_drawable->bbox, &clip, &invers);
        break;
    }
    case QXL_DRAW_ROP3: {
        SpiceRop3 rop3 = drawable->red_drawable->u.rop3;
        SpiceImage img1, img2, img3;
        image_cache_localize_brush(&display->image_cache, &rop3.brush, &img1);
        image_cache_localize(&display->image_cache, &rop3.src_bitmap, &img2, drawable);
        image_cache_localize_mask(&display->image_cache, &rop3.mask, &img3);
        canvas->ops->draw_rop3(canvas, &drawable->red_drawable->bbox,
                               &clip, &rop3);
        break;
    }
    case QXL_DRAW_COMPOSITE: {
        SpiceComposite composite = drawable->red_drawable->u.composite;
        SpiceImage src, mask;
        image_cache_localize(&display->image_cache, &composite.src_bitmap, &src, drawable);
        if (composite.mask_bitmap)
            image_cache_localize(&display->image_cache, &composite.mask_bitmap, &mask, drawable);
        canvas->ops->draw_composite(canvas, &drawable->red_drawable->bbox,
                                    &clip, &composite);
        break;
    }
    case QXL_DRAW_STROKE: {
        SpiceStroke stroke = drawable->red_drawable->u.stroke;
        SpiceImage img1;
        image_cache_localize_brush(&display->image_cache, &stroke.brush, &img1);
        canvas->ops->draw_stroke(canvas,
                                 &drawable->red_drawable->bbox, &clip, &stroke);
        break;
    }
    case QXL_DRAW_TEXT: {
        SpiceText text = drawable->red_drawable->u.text;
        SpiceImage img1, img2;
        image_cache_localize_brush(&display->image_cache, &text.fore_brush, &img1);
        image_cache_localize_brush(&display->image_cache, &text.back_brush, &img2);
        canvas->ops->draw_text(canvas, &drawable->red_drawable->bbox,
                               &clip, &text);
        break;
    }
    default:
        spice_warning("invalid type");
    }
}

static void surface_update_dest(RedSurface *surface, const SpiceRect *area)
{
    SpiceCanvas *canvas = surface->context.canvas;
    int stride = surface->context.stride;
    uint8_t *line_0 = surface->context.line_0;

    if (surface->context.canvas_draws_on_surface)
        return;

    int h = area->bottom - area->top;
    if (h == 0)
        return;

    spice_return_if_fail(stride < 0);

    uint8_t *dest = line_0 + (area->top * stride) + area->left * sizeof(uint32_t);
    dest += (h - 1) * stride;
    canvas->ops->read_bits(canvas, dest, -stride, area);
}

static void draw_until(DisplayChannel *display, RedSurface *surface, Drawable *last)
{
    RingItem *ring_item;
    Container *container;
    Drawable *now;

    do {
        ring_item = ring_get_tail(&surface->current_list);
        now = SPICE_CONTAINEROF(ring_item, Drawable, surface_list_link);
        now->refs++;
        container = now->tree_item.base.container;
        current_remove_drawable(display, now);
        container_cleanup(container);
        /* drawable_draw may call display_channel_draw for the surfaces 'now' depends on. Notice,
           that it is valid to call display_channel_draw in this case and not display_channel_draw_till:
           It is impossible that there was newer item then 'last' in one of the surfaces
           that display_channel_draw is called for, Otherwise, 'now' would have already been rendered.
           See the call for red_handle_depends_on_target_surface in red_process_draw */
        drawable_draw(display, now);
        display_channel_drawable_unref(display, now);
    } while (now != last);
}

static Drawable* current_find_intersects_rect(Ring *current, RingItem *from,
                                              const SpiceRect *area)
{
    RingItem *it;
    QRegion rgn;
    Drawable *last = NULL;

    region_init(&rgn);
    region_add(&rgn, area);

    for (it = from ? from : ring_next(current, current); it != NULL; it = ring_next(current, it)) {
        Drawable *now = SPICE_CONTAINEROF(it, Drawable, surface_list_link);
        if (region_intersects(&rgn, &now->tree_item.base.rgn)) {
            last = now;
            break;
        }
    }

    region_destroy(&rgn);
    return last;
}

/*
 * Renders drawables for updating the requested area, but only drawables that are older
 * than 'last' (exclusive).
 * FIXME: merge with display_channel_draw()?
 */
void display_channel_draw_until(DisplayChannel *display, const SpiceRect *area, int surface_id,
                               Drawable *last)
{
    RedSurface *surface;
    Drawable *surface_last = NULL;
    Ring *ring;
    RingItem *ring_item;
    Drawable *now;

    spice_return_if_fail(last);
    spice_return_if_fail(ring_item_is_linked(&last->list_link));

    surface = &display->surfaces[surface_id];

    if (surface_id != last->surface_id) {
        // find the nearest older drawable from the appropriate surface
        ring = &display->current_list;
        ring_item = &last->list_link;
        while ((ring_item = ring_next(ring, ring_item))) {
            now = SPICE_CONTAINEROF(ring_item, Drawable, list_link);
            if (now->surface_id == surface_id) {
                surface_last = now;
                break;
            }
        }
    } else {
        ring_item = ring_next(&surface->current_list, &last->surface_list_link);
        if (ring_item) {
            surface_last = SPICE_CONTAINEROF(ring_item, Drawable, surface_list_link);
        }
    }

    if (!surface_last)
        return;

    last = current_find_intersects_rect(&surface->current_list,
                                        &surface_last->surface_list_link, area);
    if (!last)
        return;

    draw_until(display, surface, last);
    surface_update_dest(surface, area);
}

void display_channel_draw(DisplayChannel *display, const SpiceRect *area, int surface_id)
{
    RedSurface *surface;
    Drawable *last;

    spice_debug("surface %d: area ==>", surface_id);
    rect_debug(area);

    spice_return_if_fail(surface_id >= 0 && surface_id < NUM_SURFACES);
    spice_return_if_fail(area);
    spice_return_if_fail(area->left >= 0 && area->top >= 0 &&
                         area->left < area->right && area->top < area->bottom);

    surface = &display->surfaces[surface_id];

    last = current_find_intersects_rect(&surface->current_list, NULL, area);
    if (last)
        draw_until(display, surface, last);

    surface_update_dest(surface, area);
}

static void region_to_qxlrects(QRegion *region, QXLRect *qxl_rects, uint32_t num_rects)
{
    SpiceRect *rects;
    int i;

    rects = spice_new0(SpiceRect, num_rects);
    region_ret_rects(region, rects, num_rects);
    for (i = 0; i < num_rects; i++) {
        qxl_rects[i].top    = rects[i].top;
        qxl_rects[i].left   = rects[i].left;
        qxl_rects[i].bottom = rects[i].bottom;
        qxl_rects[i].right  = rects[i].right;
    }
    free(rects);
}

void display_channel_update(DisplayChannel *display,
                            uint32_t surface_id, const QXLRect *area, uint32_t clear_dirty,
                            QXLRect **qxl_dirty_rects, uint32_t *num_dirty_rects)
{
    SpiceRect rect;
    RedSurface *surface;

    spice_return_if_fail(validate_surface(display, surface_id));

    red_get_rect_ptr(&rect, area);
    display_channel_draw(display, &rect, surface_id);

    surface = &display->surfaces[surface_id];
    if (*qxl_dirty_rects == NULL) {
        *num_dirty_rects = pixman_region32_n_rects(&surface->draw_dirty_region);
        *qxl_dirty_rects = spice_new0(QXLRect, *num_dirty_rects);
    }

    region_to_qxlrects(&surface->draw_dirty_region, *qxl_dirty_rects, *num_dirty_rects);
    if (clear_dirty)
        region_clear(&surface->draw_dirty_region);
}

static void clear_surface_drawables_from_pipes(DisplayChannel *display, int surface_id,
                                               int wait_if_used)
{
    RingItem *item, *next;
    DisplayChannelClient *dcc;

    FOREACH_DCC(display, item, next, dcc) {
        if (!dcc_clear_surface_drawables_from_pipe(dcc, surface_id, wait_if_used)) {
            red_channel_client_disconnect(RED_CHANNEL_CLIENT(dcc));
        }
    }
}

/* TODO: cleanup/refactor destroy functions */
void display_channel_destroy_surface(DisplayChannel *display, uint32_t surface_id)
{
    draw_depend_on_me(display, surface_id);
    /* note that draw_depend_on_me must be called before current_remove_all.
       otherwise "current" will hold items that other drawables may depend on, and then
       current_remove_all will remove them from the pipe. */
    current_remove_all(display, surface_id);
    clear_surface_drawables_from_pipes(display, surface_id, FALSE);
    display_channel_surface_unref(display, surface_id);
}

void display_channel_destroy_surface_wait(DisplayChannel *display, uint32_t surface_id)
{
    if (!validate_surface(display, surface_id))
        return;
    if (!display->surfaces[surface_id].context.canvas)
        return;

    draw_depend_on_me(display, surface_id);
    /* note that draw_depend_on_me must be called before current_remove_all.
       otherwise "current" will hold items that other drawables may depend on, and then
       current_remove_all will remove them from the pipe. */
    current_remove_all(display, surface_id);
    clear_surface_drawables_from_pipes(display, surface_id, TRUE);
}

/* called upon device reset */
/* TODO: split me*/
void display_channel_destroy_surfaces(DisplayChannel *display)
{
    int i;

    spice_debug(NULL);
    //to handle better
    for (i = 0; i < NUM_SURFACES; ++i) {
        if (display->surfaces[i].context.canvas) {
            display_channel_destroy_surface_wait(display, i);
            if (display->surfaces[i].context.canvas) {
                display_channel_surface_unref(display, i);
            }
            spice_assert(!display->surfaces[i].context.canvas);
        }
    }
    spice_warn_if_fail(ring_is_empty(&display->streams));

    if (red_channel_is_connected(RED_CHANNEL(display))) {
        red_channel_pipes_add_type(RED_CHANNEL(display), PIPE_ITEM_TYPE_INVAL_PALETTE_CACHE);
        red_pipes_add_verb(RED_CHANNEL(display), SPICE_MSG_DISPLAY_STREAM_DESTROY_ALL);
    }

    display_channel_free_glz_drawables(display);
}

static void send_create_surface(DisplayChannel *display, int surface_id, int image_ready)
{
    DisplayChannelClient *dcc;
    RingItem *item, *next;

    FOREACH_DCC(display, item, next, dcc) {
        dcc_create_surface(dcc, surface_id);
        if (image_ready)
            dcc_push_surface_image(dcc, surface_id);
    }
}

static SpiceCanvas*
create_canvas_for_surface(DisplayChannel *display, RedSurface *surface, uint32_t renderer)
{
    SpiceCanvas *canvas;

    switch (renderer) {
    case RED_RENDERER_SW:
        canvas = canvas_create_for_data(surface->context.width, surface->context.height, surface->context.format,
                                        surface->context.line_0, surface->context.stride,
                                        &display->image_cache.base,
                                        &display->image_surfaces, NULL, NULL, NULL);
        surface->context.top_down = TRUE;
        surface->context.canvas_draws_on_surface = TRUE;
        return canvas;
    default:
        spice_warn_if_reached();
    };

    return NULL;
}

void display_channel_create_surface(DisplayChannel *display, uint32_t surface_id, uint32_t width,
                                    uint32_t height, int32_t stride, uint32_t format,
                                    void *line_0, int data_is_valid, int send_client)
{
    RedSurface *surface = &display->surfaces[surface_id];

    spice_warn_if_fail(!surface->context.canvas);

    surface->context.canvas_draws_on_surface = FALSE;
    surface->context.width = width;
    surface->context.height = height;
    surface->context.format = format;
    surface->context.stride = stride;
    surface->context.line_0 = line_0;
    if (!data_is_valid) {
        char *data = line_0;
        if (stride < 0) {
            data -= abs(stride) * (height - 1);
        }
        memset(data, 0, height*abs(stride));
    }
    surface->create.info = NULL;
    surface->destroy.info = NULL;
    ring_init(&surface->current);
    ring_init(&surface->current_list);
    ring_init(&surface->depend_on_me);
    region_init(&surface->draw_dirty_region);
    surface->refs = 1;

    if (display->renderer == RED_RENDERER_INVALID) {
        int i;
        for (i = 0; i < display->num_renderers; i++) {
            surface->context.canvas = create_canvas_for_surface(display, surface, display->renderers[i]);
            if (surface->context.canvas) {
                display->renderer = display->renderers[i];
                break;
            }
        }
    } else {
        surface->context.canvas = create_canvas_for_surface(display, surface, display->renderer);
    }

    spice_return_if_fail(surface->context.canvas);
    if (send_client)
        send_create_surface(display, surface_id, data_is_valid);
}

static void on_disconnect(RedChannelClient *rcc)
{
    DisplayChannel *display;
    DisplayChannelClient *dcc;

    spice_info(NULL);
    spice_return_if_fail(rcc != NULL);

    dcc = RCC_TO_DCC(rcc);
    display = DCC_TO_DC(dcc);

    dcc_stop(dcc); // TODO: start/stop -> connect/disconnect?
    display_channel_compress_stats_print(display);

    // this was the last channel client
    spice_debug("#draw=%d, #red_draw=%d, #glz_draw=%d",
                display->drawable_count, display->red_drawable_count,
                display->glz_drawable_count);
}

static void send_item(RedChannelClient *rcc, PipeItem *item)
{
    dcc_send_item(RCC_TO_DCC(rcc), item);
}

static void hold_item(RedChannelClient *rcc, PipeItem *item)
{
    spice_return_if_fail(item);

    switch (item->type) {
    case PIPE_ITEM_TYPE_DRAW:
        drawable_pipe_item_ref(SPICE_CONTAINEROF(item, DrawablePipeItem, dpi_pipe_item));
        break;
    case PIPE_ITEM_TYPE_STREAM_CLIP:
        ((StreamClipItem *)item)->refs++;
        break;
    case PIPE_ITEM_TYPE_UPGRADE:
        ((UpgradeItem *)item)->refs++;
        break;
    case PIPE_ITEM_TYPE_IMAGE:
        ((ImageItem *)item)->refs++;
        break;
    default:
        spice_warn_if_reached();
    }
}

static void release_item(RedChannelClient *rcc, PipeItem *item, int item_pushed)
{
    DisplayChannelClient *dcc = RCC_TO_DCC(rcc);

    spice_return_if_fail(item != NULL);
    dcc_release_item(dcc, item, item_pushed);
}

static int handle_migrate_flush_mark(RedChannelClient *rcc)
{
    DisplayChannel *display_channel = SPICE_CONTAINEROF(rcc->channel, DisplayChannel, common.base);
    RedChannel *channel = RED_CHANNEL(display_channel);

    red_channel_pipes_add_type(channel, PIPE_ITEM_TYPE_MIGRATE_DATA);
    return TRUE;
}

static uint64_t handle_migrate_data_get_serial(RedChannelClient *rcc, uint32_t size, void *message)
{
    SpiceMigrateDataDisplay *migrate_data;

    migrate_data = (SpiceMigrateDataDisplay *)((uint8_t *)message + sizeof(SpiceMigrateDataHeader));

    return migrate_data->message_serial;
}

static int handle_migrate_data(RedChannelClient *rcc, uint32_t size, void *message)
{
    return dcc_handle_migrate_data(RCC_TO_DCC(rcc), size, message);
}

static SpiceCanvas *image_surfaces_get(SpiceImageSurfaces *surfaces, uint32_t surface_id)
{
    DisplayChannel *display = SPICE_CONTAINEROF(surfaces, DisplayChannel, image_surfaces);

    spice_return_val_if_fail(validate_surface(display, surface_id), NULL);

    return display->surfaces[surface_id].context.canvas;
}

DisplayChannel* display_channel_new(RedWorker *worker, int migrate, int stream_video,
                                    uint32_t n_surfaces)
{
    DisplayChannel *display;
    ChannelCbs cbs = {
        .on_disconnect = on_disconnect,
        .send_item = send_item,
        .hold_item = hold_item,
        .release_item = release_item,
        .handle_migrate_flush_mark = handle_migrate_flush_mark,
        .handle_migrate_data = handle_migrate_data,
        .handle_migrate_data_get_serial = handle_migrate_data_get_serial
    };
    static SpiceImageSurfacesOps image_surfaces_ops = {
        image_surfaces_get,
    };

    spice_return_val_if_fail(num_renderers > 0, NULL);

    spice_info("create display channel");
    display = (DisplayChannel *)red_worker_new_channel(
        worker, sizeof(*display), "display_channel",
        SPICE_CHANNEL_DISPLAY,
        SPICE_MIGRATE_NEED_FLUSH | SPICE_MIGRATE_NEED_DATA_TRANSFER,
        &cbs, dcc_handle_message);
    spice_return_val_if_fail(display, NULL);

    clockid_t stat_clock = CLOCK_THREAD_CPUTIME_ID;
    stat_init(&display->add_stat, "add", stat_clock);
    stat_init(&display->exclude_stat, "exclude", stat_clock);
    stat_init(&display->__exclude_stat, "__exclude", stat_clock);
#ifdef RED_STATISTICS
    RedChannel *channel = RED_CHANNEL(display);
    display->cache_hits_counter = stat_add_counter(channel->stat,
                                                           "cache_hits", TRUE);
    display->add_to_cache_counter = stat_add_counter(channel->stat,
                                                             "add_to_cache", TRUE);
    display->non_cache_counter = stat_add_counter(channel->stat,
                                                          "non_cache", TRUE);
#endif
    stat_compress_init(&display->lz_stat, "lz", stat_clock);
    stat_compress_init(&display->glz_stat, "glz", stat_clock);
    stat_compress_init(&display->quic_stat, "quic", stat_clock);
    stat_compress_init(&display->jpeg_stat, "jpeg", stat_clock);
    stat_compress_init(&display->zlib_glz_stat, "zlib", stat_clock);
    stat_compress_init(&display->jpeg_alpha_stat, "jpeg_alpha", stat_clock);
    stat_compress_init(&display->lz4_stat, "lz4", stat_clock);

    display->n_surfaces = n_surfaces;
    display->num_renderers = num_renderers;
    memcpy(display->renderers, renderers, sizeof(display->renderers));
    display->renderer = RED_RENDERER_INVALID;

    ring_init(&display->current_list);
    display->image_surfaces.ops = &image_surfaces_ops;
    drawables_init(display);
    image_cache_init(&display->image_cache);
    display->stream_video = stream_video;
    display_channel_init_streams(display);

    return display;
}

static inline void set_surface_release_info(QXLReleaseInfoExt *release_info_ext,
                                            QXLReleaseInfo *release_info, uint32_t group_id)
{
    release_info_ext->info = release_info;
    release_info_ext->group_id = group_id;
}

void display_channel_process_surface_cmd(DisplayChannel *display, RedSurfaceCmd *surface,
                                         uint32_t group_id, int loadvm)
{
    uint32_t surface_id;
    RedSurface *red_surface;
    uint8_t *data;

    surface_id = surface->surface_id;
    if SPICE_UNLIKELY(surface_id >= display->n_surfaces) {
        return;
    }

    red_surface = &display->surfaces[surface_id];

    switch (surface->type) {
    case QXL_SURFACE_CMD_CREATE: {
        uint32_t height = surface->u.surface_create.height;
        int32_t stride = surface->u.surface_create.stride;
        int reloaded_surface = loadvm || (surface->flags & QXL_SURF_FLAG_KEEP_DATA);

        if (red_surface->refs) {
            spice_warning("avoiding creating a surface twice");
            break;
        }
        data = surface->u.surface_create.data;
        if (stride < 0) {
            data -= (int32_t)(stride * (height - 1));
        }
        display_channel_create_surface(display, surface_id, surface->u.surface_create.width,
                                       height, stride, surface->u.surface_create.format, data,
                                       reloaded_surface,
                                       // reloaded surfaces will be sent on demand
                                       !reloaded_surface);
        set_surface_release_info(&red_surface->create, surface->release_info, group_id);
        break;
    }
    case QXL_SURFACE_CMD_DESTROY:
        if (!red_surface->refs) {
            spice_warning("avoiding destroying a surface twice");
            break;
        }
        set_surface_release_info(&red_surface->destroy, surface->release_info, group_id);
        display_channel_destroy_surface(display, surface_id);
        break;
    default:
        spice_warn_if_reached();
    };
}

void display_channel_update_compression(DisplayChannel *display, DisplayChannelClient *dcc)
{
    if (dcc->jpeg_state == SPICE_WAN_COMPRESSION_AUTO) {
        display->enable_jpeg = dcc->common.is_low_bandwidth;
    } else {
        display->enable_jpeg = (dcc->jpeg_state == SPICE_WAN_COMPRESSION_ALWAYS);
    }

    if (dcc->zlib_glz_state == SPICE_WAN_COMPRESSION_AUTO) {
        display->enable_zlib_glz_wrap = dcc->common.is_low_bandwidth;
    } else {
        display->enable_zlib_glz_wrap = (dcc->zlib_glz_state == SPICE_WAN_COMPRESSION_ALWAYS);
    }
    spice_info("jpeg %s", display->enable_jpeg ? "enabled" : "disabled");
    spice_info("zlib-over-glz %s", display->enable_zlib_glz_wrap ? "enabled" : "disabled");
}

void display_channel_gl_scanout(DisplayChannel *display)
{
    red_channel_pipes_new_add_push(RED_CHANNEL(display), dcc_gl_scanout_item_new, NULL);
}
