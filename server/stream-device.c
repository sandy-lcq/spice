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

#include <spice/stream-device.h>

#include "char-device.h"
#include "stream-channel.h"
#include "reds.h"

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

    StreamDevHeader hdr;
    uint8_t hdr_pos;
    bool has_error;
    bool opened;
    StreamChannel *stream_channel;
};

struct StreamDeviceClass {
    RedCharDeviceClass parent_class;
};

static GType stream_device_get_type(void) G_GNUC_CONST;
static StreamDevice *stream_device_new(SpiceCharDeviceInstance *sin, RedsState *reds);

G_DEFINE_TYPE(StreamDevice, stream_device, RED_TYPE_CHAR_DEVICE)

typedef bool StreamMsgHandler(StreamDevice *dev, SpiceCharDeviceInstance *sin)
    SPICE_GNUC_WARN_UNUSED_RESULT;

static StreamMsgHandler handle_msg_format, handle_msg_data;

static bool handle_msg_invalid(StreamDevice *dev, SpiceCharDeviceInstance *sin,
                               const char *error_msg) SPICE_GNUC_WARN_UNUSED_RESULT;

static RedPipeItem *
stream_device_read_msg_from_dev(RedCharDevice *self, SpiceCharDeviceInstance *sin)
{
    StreamDevice *dev = STREAM_DEVICE(self);
    SpiceCharDeviceInterface *sif;
    int n;
    bool handled = false;

    if (dev->has_error) {
        return NULL;
    }

    sif = spice_char_device_get_interface(sin);

    /* read header */
    while (dev->hdr_pos < sizeof(dev->hdr)) {
        n = sif->read(sin, (uint8_t *) &dev->hdr, sizeof(dev->hdr) - dev->hdr_pos);
        if (n <= 0) {
            return NULL;
        }
        dev->hdr_pos += n;
        if (dev->hdr_pos >= sizeof(dev->hdr)) {
            dev->hdr.type = GUINT16_FROM_LE(dev->hdr.type);
            dev->hdr.size = GUINT32_FROM_LE(dev->hdr.size);
        }
    }

    switch ((StreamMsgType) dev->hdr.type) {
    case STREAM_TYPE_FORMAT:
        if (dev->hdr.size != sizeof(StreamMsgFormat)) {
            handled = handle_msg_invalid(dev, sin, "Wrong size for StreamMsgFormat");
        } else {
            handled = handle_msg_format(dev, sin);
        }
        break;
    case STREAM_TYPE_DATA:
        handled = handle_msg_data(dev, sin);
        break;
    case STREAM_TYPE_CAPABILITIES:
        /* FIXME */
    default:
        handled = handle_msg_invalid(dev, sin, "Invalid message type");
        break;
    }

    /* current message has been handled, so reset state and get ready to parse
     * the next message */
    if (handled) {
        dev->hdr_pos = 0;
    }

    return NULL;
}

static bool
handle_msg_invalid(StreamDevice *dev, SpiceCharDeviceInstance *sin, const char *error_msg)
{
    static const char default_error_msg[] = "Protocol error";

    if (!error_msg) {
        error_msg = default_error_msg;
    }

    int msg_size = sizeof(StreamMsgNotifyError) + strlen(error_msg) + 1;
    int total_size = sizeof(StreamDevHeader) + msg_size;

    RedCharDevice *char_dev = RED_CHAR_DEVICE(dev);
    RedCharDeviceWriteBuffer *buf =
        red_char_device_write_buffer_get_server_no_token(char_dev, total_size);
    buf->buf_used = total_size;

    StreamDevHeader *const hdr = (StreamDevHeader *)buf->buf;
    hdr->protocol_version = STREAM_DEVICE_PROTOCOL;
    hdr->padding = 0;
    hdr->type = GUINT16_TO_LE(STREAM_TYPE_NOTIFY_ERROR);
    hdr->size = GUINT32_TO_LE(msg_size);

    StreamMsgNotifyError *const error = (StreamMsgNotifyError *)(hdr+1);
    error->error_code = GUINT32_TO_LE(0);
    strcpy((char *) error->msg, error_msg);

    red_char_device_write_buffer_add(char_dev, buf);

    dev->has_error = true;
    return false;
}

static bool
handle_msg_format(StreamDevice *dev, SpiceCharDeviceInstance *sin)
{
    StreamMsgFormat fmt;
    SpiceCharDeviceInterface *sif = spice_char_device_get_interface(sin);
    int n = sif->read(sin, (uint8_t *) &fmt, sizeof(fmt));
    if (n == 0) {
        return false;
    }
    if (n != sizeof(fmt)) {
        return handle_msg_invalid(dev, sin, NULL);
    }
    fmt.width = GUINT32_FROM_LE(fmt.width);
    fmt.height = GUINT32_FROM_LE(fmt.height);
    stream_channel_change_format(dev->stream_channel, &fmt);

    return true;
}

static bool
handle_msg_data(StreamDevice *dev, SpiceCharDeviceInstance *sin)
{
    SpiceCharDeviceInterface *sif = spice_char_device_get_interface(sin);
    int n;
    while (1) {
        uint8_t buf[16 * 1024];
        n = sif->read(sin, buf, sizeof(buf));
        if (n <= 0) {
            break;
        }
        stream_channel_send_data(dev->stream_channel, buf, n, reds_get_mm_time());
        dev->hdr.size -= n;
    }

    return dev->hdr.size == 0;
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

static void
stream_device_stream_start(void *opaque, StreamMsgStartStop *start,
                           StreamChannel *stream_channel G_GNUC_UNUSED)
{
    StreamDevice *dev = (StreamDevice *) opaque;

    if (!dev->opened) {
        return;
    }

    int msg_size = sizeof(*start) + sizeof(start->codecs[0]) * start->num_codecs;
    int total_size = sizeof(StreamDevHeader) + msg_size;

    RedCharDevice *char_dev = RED_CHAR_DEVICE(dev);
    RedCharDeviceWriteBuffer *buf =
        red_char_device_write_buffer_get_server_no_token(char_dev, total_size);
    buf->buf_used = total_size;

    StreamDevHeader *hdr = (StreamDevHeader *)buf->buf;
    hdr->protocol_version = STREAM_DEVICE_PROTOCOL;
    hdr->padding = 0;
    hdr->type = GUINT16_TO_LE(STREAM_TYPE_START_STOP);
    hdr->size = GUINT32_TO_LE(msg_size);

    memcpy(&hdr[1], start, msg_size);

    red_char_device_write_buffer_add(char_dev, buf);
}

RedCharDevice *
stream_device_connect(RedsState *reds, SpiceCharDeviceInstance *sin)
{
    SpiceCharDeviceInterface *sif;

    StreamChannel *stream_channel = stream_channel_new(reds, 1); // TODO id

    StreamDevice *dev = stream_device_new(sin, reds);
    dev->stream_channel = stream_channel;
    stream_channel_register_start_cb(stream_channel, stream_device_stream_start, dev);

    sif = spice_char_device_get_interface(sin);
    if (sif->state) {
        sif->state(sin, 1);
    }

    return RED_CHAR_DEVICE(dev);
}

static void
stream_device_dispose(GObject *object)
{
    StreamDevice *dev = STREAM_DEVICE(object);

    if (dev->stream_channel) {
        // close all current connections and drop the reference
        red_channel_destroy(RED_CHANNEL(dev->stream_channel));
        dev->stream_channel = NULL;
    }
}

static void
stream_device_port_event(RedCharDevice *char_dev, uint8_t event)
{
    if (event != SPICE_PORT_EVENT_OPENED && event != SPICE_PORT_EVENT_CLOSED) {
        return;
    }

    StreamDevice *dev = STREAM_DEVICE(char_dev);

    // reset device and channel on close/open
    dev->opened = (event == SPICE_PORT_EVENT_OPENED);
    dev->hdr_pos = 0;
    dev->has_error = false;
    red_char_device_reset(char_dev);
    stream_channel_reset(dev->stream_channel);
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
    char_dev_class->port_event = stream_device_port_event;
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
