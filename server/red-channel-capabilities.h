/* -*- Mode: C; c-basic-offset: 4; indent-tabs-mode: nil -*- */
/*
   Copyright (C) 2009-2017 Red Hat, Inc.

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

#ifndef RED_CHANNEL_CAPABILITIES_H_
#define RED_CHANNEL_CAPABILITIES_H_

#include <stdint.h>
#include <glib-object.h>

G_BEGIN_DECLS

/* Holds channel capabilities.
 * Channel capabilities are composed by a common part
 * and a specific one. */
typedef struct RedChannelCapabilities {
    int num_common_caps;
    uint32_t *common_caps;
    int num_caps;
    uint32_t *caps;
} RedChannelCapabilities;

/* Initialize the structure based on a previous one. */
void red_channel_capabilities_init(RedChannelCapabilities *dest,
                                   const RedChannelCapabilities *caps);

/* Reset capabilities.
 * All resources are freed by this function. */
void red_channel_capabilities_reset(RedChannelCapabilities *caps);

/* GObject type that can be used to box RedChannelCapabilities */
extern GType red_channel_capabilities_type;
#define RED_TYPE_CHANNEL_CAPABILITIES red_channel_capabilities_type

G_END_DECLS

#endif /* RED_CHANNEL_CAPABILITIES_H_ */
