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

#ifndef __DUMMY_CHANNEL_H__
#define __DUMMY_CHANNEL_H__

#include <glib-object.h>

#include "red-channel.h"

G_BEGIN_DECLS

// TODO: tmp, for channels that don't use RedChannel yet (e.g., snd channel), but
// do use the client callbacks. So the channel clients are not connected (the channel doesn't
// have list of them, but they do have a link to the channel, and the client has a list of them)

#define TYPE_DUMMY_CHANNEL dummy_channel_get_type()

#define DUMMY_CHANNEL(obj) (G_TYPE_CHECK_INSTANCE_CAST((obj), TYPE_DUMMY_CHANNEL, DummyChannel))
#define DUMMY_CHANNEL_CLASS(klass) \
    (G_TYPE_CHECK_CLASS_CAST((klass), TYPE_DUMMY_CHANNEL, DummyChannelClass))
#define _IS_DUMMY_CHANNEL(obj) (G_TYPE_CHECK_INSTANCE_TYPE((obj), TYPE_DUMMY_CHANNEL))
#define _IS_DUMMY_CHANNEL_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE((klass), TYPE_DUMMY_CHANNEL))
#define DUMMY_CHANNEL_GET_CLASS(obj) \
    (G_TYPE_INSTANCE_GET_CLASS((obj), TYPE_DUMMY_CHANNEL, DummyChannelClass))

typedef struct DummyChannel DummyChannel;
typedef struct DummyChannelClass DummyChannelClass;

struct DummyChannel
{
    RedChannel parent;
};

struct DummyChannelClass
{
    RedChannelClass parent_class;
};

GType dummy_channel_get_type(void) G_GNUC_CONST;

RedChannel *dummy_channel_new(RedsState *reds, uint32_t type, uint32_t id);

G_END_DECLS

#endif /* __DUMMY_CHANNEL_H__ */
