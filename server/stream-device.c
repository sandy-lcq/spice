/* spice-server character device to handle a video stream

   Copyright (C) 2017 Red Hat, Inc.

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

#include "char-device.h"

#define TYPE_STREAM_DEVICE stream_device_get_type()

#define STREAM_DEVICE(obj) \
    (G_TYPE_CHECK_INSTANCE_CAST((obj), TYPE_STREAM_DEVICE, StreamDevice))
#define STREAM_DEVICE_CLASS(klass) \
    (G_TYPE_CHECK_CLASS_CAST((klass), TYPE_STREAM_DEVICE, StreamDeviceClass))
#define STREAM_DEVICE_GET_CLASS(obj) \
    (G_TYPE_INSTANCE_GET_CLASS((obj), TYPE_STREAM_DEVICE, StreamDeviceClass))

typedef struct StreamDevice StreamDevice;
typedef struct StreamDeviceClass StreamDeviceClass;

struct StreamDevice {
    RedCharDevice parent;
};

struct StreamDeviceClass {
    RedCharDeviceClass parent_class;
};

static GType stream_device_get_type(void) G_GNUC_CONST;
static StreamDevice *stream_device_new(SpiceCharDeviceInstance *sin, RedsState *reds);

G_DEFINE_TYPE(StreamDevice, stream_device, RED_TYPE_CHAR_DEVICE)

static RedPipeItem *
stream_device_read_msg_from_dev(RedCharDevice *self, SpiceCharDeviceInstance *sin)
{
    SpiceCharDeviceInterface *sif;
    int n;

    sif = spice_char_device_get_interface(sin);

    do {
        uint8_t buf[256];
        n = sif->read(sin, buf, sizeof(buf));
        spice_debug("read %d bytes from device", n);
    } while (n > 0);

    return NULL;
}

static void
stream_device_send_msg_to_client(RedCharDevice *self, RedPipeItem *msg, RedClient *client)
{
}

static void
stream_device_send_tokens_to_client(RedCharDevice *self, RedClient *client, uint32_t tokens)
{
    spice_printerr("Not implemented!");
}

static void
stream_device_remove_client(RedCharDevice *self, RedClient *client)
{
}

RedCharDevice *
stream_device_connect(RedsState *reds, SpiceCharDeviceInstance *sin)
{
    SpiceCharDeviceInterface *sif;

    StreamDevice *dev = stream_device_new(sin, reds);

    sif = spice_char_device_get_interface(sin);
    if (sif->state) {
        sif->state(sin, 1);
    }

    return RED_CHAR_DEVICE(dev);
}

static void
stream_device_dispose(GObject *object)
{
}

static void
stream_device_class_init(StreamDeviceClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS(klass);
    RedCharDeviceClass *char_dev_class = RED_CHAR_DEVICE_CLASS(klass);

    object_class->dispose = stream_device_dispose;

    char_dev_class->read_one_msg_from_device = stream_device_read_msg_from_dev;
    char_dev_class->send_msg_to_client = stream_device_send_msg_to_client;
    char_dev_class->send_tokens_to_client = stream_device_send_tokens_to_client;
    char_dev_class->remove_client = stream_device_remove_client;
}

static void
stream_device_init(StreamDevice *self)
{
}

static StreamDevice *
stream_device_new(SpiceCharDeviceInstance *sin, RedsState *reds)
{
    return g_object_new(TYPE_STREAM_DEVICE,
                        "sin", sin,
                        "spice-server", reds,
                        "client-tokens-interval", 0ULL,
                        "self-tokens", ~0ULL,
                        NULL);
}
