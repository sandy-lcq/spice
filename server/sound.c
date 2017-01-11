/* -*- Mode: C; c-basic-offset: 4; indent-tabs-mode: nil -*- */
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

#include <fcntl.h>
#include <errno.h>
#include <limits.h>
#include <sys/socket.h>
#include <netinet/ip.h>
#include <netinet/tcp.h>

#include <common/marshaller.h>
#include <common/generated_server_marshallers.h>
#include <common/snd_codec.h>

#include "spice.h"
#include "red-common.h"
#include "dummy-channel-client.h"
#include "main-channel.h"
#include "reds.h"
#include "red-qxl.h"
#include "red-channel-client.h"
/* FIXME: for now, allow sound channel access to private RedChannelClient data */
#include "red-channel-client-private.h"
#include "red-client.h"
#include "sound.h"
#include "demarshallers.h"
#include "main-channel-client.h"

#ifndef IOV_MAX
#define IOV_MAX 1024
#endif

#define SND_RECEIVE_BUF_SIZE     (16 * 1024 * 2)
#define RECORD_SAMPLES_SIZE (SND_RECEIVE_BUF_SIZE >> 2)

enum SndCommand {
    SND_MIGRATE,
    SND_CTRL,
    SND_VOLUME,
    SND_MUTE,
    SND_END_COMMAND,
};

enum PlaybackCommand {
    SND_PLAYBACK_MODE = SND_END_COMMAND,
    SND_PLAYBACK_PCM,
    SND_PLAYBACK_LATENCY,
};

#define SND_MIGRATE_MASK (1 << SND_MIGRATE)
#define SND_CTRL_MASK (1 << SND_CTRL)
#define SND_VOLUME_MASK (1 << SND_VOLUME)
#define SND_MUTE_MASK (1 << SND_MUTE)
#define SND_VOLUME_MUTE_MASK (SND_VOLUME_MASK|SND_MUTE_MASK)

#define SND_PLAYBACK_MODE_MASK (1 << SND_PLAYBACK_MODE)
#define SND_PLAYBACK_PCM_MASK (1 << SND_PLAYBACK_PCM)
#define SND_PLAYBACK_LATENCY_MASK ( 1 << SND_PLAYBACK_LATENCY)

typedef struct SndChannelClient SndChannelClient;
typedef struct SndChannel SndChannel;
typedef struct PlaybackChannelClient PlaybackChannelClient;
typedef struct RecordChannelClient RecordChannelClient;
typedef struct AudioFrame AudioFrame;
typedef struct AudioFrameContainer AudioFrameContainer;
typedef struct SpicePlaybackState PlaybackChannel;
typedef struct SpiceRecordState RecordChannel;

typedef void (*snd_channel_send_messages_proc)(void *in_channel);
typedef int (*snd_channel_handle_message_proc)(SndChannelClient *client, size_t size, uint32_t type, void *message);
typedef void (*snd_channel_on_message_done_proc)(SndChannelClient *client);
typedef void (*snd_channel_cleanup_channel_proc)(SndChannelClient *client);

#define SND_CHANNEL_CLIENT(obj) (&(obj)->base)

/* Connects an audio client to a Spice client */
struct SndChannelClient {
    RedsStream *stream;
    SndChannel *channel;
    spice_parse_channel_func_t parser;
    int refs;

    RedChannelClient *channel_client;

    int active;
    int client_active;
    int blocked;

    uint32_t command;

    struct {
        uint64_t serial;
        SpiceMarshaller *marshaller;
        uint32_t size;
        uint32_t pos;
    } send_data;

    struct {
        uint8_t buf[SND_RECEIVE_BUF_SIZE];
        uint8_t *message_start;
        uint8_t *now;
        uint8_t *end;
    } receive_data;

    snd_channel_send_messages_proc send_messages;
    snd_channel_handle_message_proc handle_message;
    snd_channel_on_message_done_proc on_message_done;
    snd_channel_cleanup_channel_proc cleanup;
};

struct AudioFrame {
    uint32_t time;
    uint32_t samples[SND_CODEC_MAX_FRAME_SIZE];
    PlaybackChannelClient *client;
    AudioFrame *next;
    AudioFrameContainer *container;
    gboolean allocated;
};

#define NUM_AUDIO_FRAMES 3
struct AudioFrameContainer
{
    int refs;
    AudioFrame items[NUM_AUDIO_FRAMES];
};

struct PlaybackChannelClient {
    SndChannelClient base;

    AudioFrameContainer *frames;
    AudioFrame *free_frames;
    AudioFrame *in_progress;   /* Frame being sent to the client */
    AudioFrame *pending_frame; /* Next frame to send to the client */
    uint32_t mode;
    uint32_t latency;
    SndCodec codec;
    uint8_t  encode_buf[SND_CODEC_MAX_COMPRESSED_BYTES];
};

typedef struct SpiceVolumeState {
    uint16_t *volume;
    uint8_t volume_nchannels;
    int mute;
} SpiceVolumeState;

#define TYPE_SND_CHANNEL snd_channel_get_type()
#define SND_CHANNEL(obj) (G_TYPE_CHECK_INSTANCE_CAST((obj), TYPE_SND_CHANNEL, SndChannel))
GType snd_channel_get_type(void) G_GNUC_CONST;

/* Base class for SpicePlaybackState and SpiceRecordState */
struct SndChannel {
    RedChannel parent;

    SndChannelClient *connection; /* Only one client is supported */
    SndChannel *next; /* For the global SndChannel list */

    int active;
    SpiceVolumeState volume;
    uint32_t frequency;
};

typedef struct SndChannelClass {
    RedChannelClass parent_class;
} SndChannelClass;

G_DEFINE_TYPE(SndChannel, snd_channel, RED_TYPE_CHANNEL)


#define TYPE_PLAYBACK_CHANNEL playback_channel_get_type()
#define PLAYBACK_CHANNEL(obj) \
    (G_TYPE_CHECK_INSTANCE_CAST((obj), TYPE_PLAYBACK_CHANNEL, PlaybackChannel))
GType playback_channel_get_type(void) G_GNUC_CONST;

struct SpicePlaybackState {
    SndChannel channel;
};

typedef struct PlaybackChannelClass {
    SndChannelClass parent_class;
} PlaybackChannelClass;

G_DEFINE_TYPE(PlaybackChannel, playback_channel, TYPE_SND_CHANNEL)


#define TYPE_RECORD_CHANNEL record_channel_get_type()
#define RECORD_CHANNEL(obj) (G_TYPE_CHECK_INSTANCE_CAST((obj), TYPE_RECORD_CHANNEL, RecordChannel))
GType record_channel_get_type(void) G_GNUC_CONST;

struct SpiceRecordState {
    SndChannel channel;
};

typedef struct RecordChannelClass {
    SndChannelClass parent_class;
} RecordChannelClass;

G_DEFINE_TYPE(RecordChannel, record_channel, TYPE_SND_CHANNEL)


struct RecordChannelClient {
    SndChannelClient base;
    uint32_t samples[RECORD_SAMPLES_SIZE];
    uint32_t write_pos;
    uint32_t read_pos;
    uint32_t mode;
    uint32_t mode_time;
    uint32_t start_time;
    SndCodec codec;
    uint8_t  decode_buf[SND_CODEC_MAX_FRAME_BYTES];
};

/* A list of all Spice{Playback,Record}State objects */
static SndChannel *snd_channels;

static void snd_receive(SndChannelClient *client);
static void snd_playback_start(SndChannel *channel);
static void snd_record_start(SndChannel *channel);
static void snd_playback_alloc_frames(PlaybackChannelClient *playback);

static SndChannelClient *snd_channel_unref(SndChannelClient *client)
{
    if (!--client->refs) {
        spice_printerr("SndChannelClient=%p freed", client);
        free(client);
        return NULL;
    }
    return client;
}

static RedsState* snd_channel_get_server(SndChannelClient *client)
{
    g_return_val_if_fail(client != NULL, NULL);
    return red_channel_get_server(RED_CHANNEL(client->channel));
}

static void snd_disconnect_channel(SndChannelClient *client)
{
    SndChannel *channel;
    RedsState *reds;
    RedChannel *red_channel;
    uint32_t type;

    if (!client || !client->stream) {
        spice_debug("not connected");
        return;
    }
    red_channel = red_channel_client_get_channel(client->channel_client);
    reds = snd_channel_get_server(client);
    g_object_get(red_channel, "channel-type", &type, NULL);
    spice_debug("SndChannelClient=%p rcc=%p type=%d",
                 client, client->channel_client, type);
    channel = client->channel;
    client->cleanup(client);
    red_channel_client_disconnect(channel->connection->channel_client);
    channel->connection->channel_client = NULL;
    reds_core_watch_remove(reds, client->stream->watch);
    client->stream->watch = NULL;
    reds_stream_free(client->stream);
    client->stream = NULL;
    spice_marshaller_destroy(client->send_data.marshaller);
    snd_channel_unref(client);
    channel->connection = NULL;
}

static void snd_playback_free_frame(PlaybackChannelClient *playback_client, AudioFrame *frame)
{
    frame->client = playback_client;
    frame->next = playback_client->free_frames;
    playback_client->free_frames = frame;
}

static void snd_playback_on_message_done(SndChannelClient *client)
{
    PlaybackChannelClient *playback_client = (PlaybackChannelClient *)client;
    if (playback_client->in_progress) {
        snd_playback_free_frame(playback_client, playback_client->in_progress);
        playback_client->in_progress = NULL;
        if (playback_client->pending_frame) {
            client->command |= SND_PLAYBACK_PCM_MASK;
        }
    }
}

static void snd_record_on_message_done(SndChannelClient *client)
{
}

static int snd_send_data(SndChannelClient *client)
{
    uint32_t n;

    if (!client) {
        return FALSE;
    }

    if (!(n = client->send_data.size - client->send_data.pos)) {
        return TRUE;
    }

    RedsState *reds = snd_channel_get_server(client);
    for (;;) {
        struct iovec vec[IOV_MAX];
        int vec_size;

        if (!n) {
            client->on_message_done(client);

            if (client->blocked) {
                client->blocked = FALSE;
                reds_core_watch_update_mask(reds, client->stream->watch, SPICE_WATCH_EVENT_READ);
            }
            break;
        }

        vec_size = spice_marshaller_fill_iovec(client->send_data.marshaller,
                                               vec, IOV_MAX, client->send_data.pos);
        n = reds_stream_writev(client->stream, vec, vec_size);
        if (n == -1) {
            switch (errno) {
            case EAGAIN:
                client->blocked = TRUE;
                reds_core_watch_update_mask(reds, client->stream->watch, SPICE_WATCH_EVENT_READ |
                                                                 SPICE_WATCH_EVENT_WRITE);
                return FALSE;
            case EINTR:
                break;
            case EPIPE:
                snd_disconnect_channel(client);
                return FALSE;
            default:
                spice_printerr("%s", strerror(errno));
                snd_disconnect_channel(client);
                return FALSE;
            }
        } else {
            client->send_data.pos += n;
        }
        n = client->send_data.size - client->send_data.pos;
    }
    return TRUE;
}

static int snd_record_handle_write(RecordChannelClient *record_client, size_t size, void *message)
{
    SpiceMsgcRecordPacket *packet;
    uint32_t write_pos;
    uint32_t* data;
    uint32_t len;
    uint32_t now;

    if (!record_client) {
        return FALSE;
    }

    packet = (SpiceMsgcRecordPacket *)message;

    if (record_client->mode == SPICE_AUDIO_DATA_MODE_RAW) {
        data = (uint32_t *)packet->data;
        size = packet->data_size >> 2;
        size = MIN(size, RECORD_SAMPLES_SIZE);
     } else {
        int decode_size;
        decode_size = sizeof(record_client->decode_buf);
        if (snd_codec_decode(record_client->codec, packet->data, packet->data_size,
                    record_client->decode_buf, &decode_size) != SND_CODEC_OK)
            return FALSE;
        data = (uint32_t *) record_client->decode_buf;
        size = decode_size >> 2;
    }

    write_pos = record_client->write_pos % RECORD_SAMPLES_SIZE;
    record_client->write_pos += size;
    len = RECORD_SAMPLES_SIZE - write_pos;
    now = MIN(len, size);
    size -= now;
    memcpy(record_client->samples + write_pos, data, now << 2);

    if (size) {
        memcpy(record_client->samples, data + now, size << 2);
    }

    if (record_client->write_pos - record_client->read_pos > RECORD_SAMPLES_SIZE) {
        record_client->read_pos = record_client->write_pos - RECORD_SAMPLES_SIZE;
    }
    return TRUE;
}

static int snd_playback_handle_message(SndChannelClient *client, size_t size, uint32_t type, void *message)
{
    if (!client) {
        return FALSE;
    }

    switch (type) {
    case SPICE_MSGC_DISCONNECTING:
        break;
    default:
        spice_printerr("invalid message type %u", type);
        return FALSE;
    }
    return TRUE;
}

static int snd_record_handle_message(SndChannelClient *client, size_t size, uint32_t type, void *message)
{
    RecordChannelClient *record_client = (RecordChannelClient *)client;

    if (!client) {
        return FALSE;
    }
    switch (type) {
    case SPICE_MSGC_RECORD_DATA:
        return snd_record_handle_write(record_client, size, message);
    case SPICE_MSGC_RECORD_MODE: {
        SpiceMsgcRecordMode *mode = (SpiceMsgcRecordMode *)message;
        SndChannel *channel = client->channel;
        record_client->mode_time = mode->time;
        if (mode->mode != SPICE_AUDIO_DATA_MODE_RAW) {
            if (snd_codec_is_capable(mode->mode, channel->frequency)) {
                if (snd_codec_create(&record_client->codec, mode->mode, channel->frequency,
                                     SND_CODEC_DECODE) == SND_CODEC_OK) {
                    record_client->mode = mode->mode;
                } else {
                    spice_printerr("create decoder failed");
                    return FALSE;
                }
            }
            else {
                spice_printerr("unsupported mode %d", record_client->mode);
                return FALSE;
            }
        }
        else
            record_client->mode = mode->mode;
        break;
    }

    case SPICE_MSGC_RECORD_START_MARK: {
        SpiceMsgcRecordStartMark *mark = (SpiceMsgcRecordStartMark *)message;
        record_client->start_time = mark->time;
        break;
    }
    case SPICE_MSGC_DISCONNECTING:
        break;
    default:
        spice_printerr("invalid message type %u", type);
        return FALSE;
    }
    return TRUE;
}

static void snd_receive(SndChannelClient *client)
{
    SpiceDataHeaderOpaque *header;

    if (!client) {
        return;
    }

    header = &client->channel_client->incoming.header;

    for (;;) {
        ssize_t n;
        n = client->receive_data.end - client->receive_data.now;
        spice_warn_if_fail(n > 0);
        n = reds_stream_read(client->stream, client->receive_data.now, n);
        if (n <= 0) {
            if (n == 0) {
                snd_disconnect_channel(client);
                return;
            }
            spice_assert(n == -1);
            switch (errno) {
            case EAGAIN:
                return;
            case EINTR:
                break;
            case EPIPE:
                snd_disconnect_channel(client);
                return;
            default:
                spice_printerr("%s", strerror(errno));
                snd_disconnect_channel(client);
                return;
            }
        } else {
            client->receive_data.now += n;
            for (;;) {
                uint8_t *msg_start = client->receive_data.message_start;
                uint8_t *data = msg_start + header->header_size;
                size_t parsed_size;
                uint8_t *parsed;
                message_destructor_t parsed_free;

                header->data = msg_start;
                n = client->receive_data.now - msg_start;

                if (n < header->header_size ||
                    n < header->header_size + header->get_msg_size(header)) {
                    break;
                }
                parsed = client->parser((void *)data, data + header->get_msg_size(header),
                                         header->get_msg_type(header),
                                         SPICE_VERSION_MINOR, &parsed_size, &parsed_free);
                if (parsed == NULL) {
                    spice_printerr("failed to parse message type %d", header->get_msg_type(header));
                    snd_disconnect_channel(client);
                    return;
                }
                if (!client->handle_message(client, parsed_size,
                                             header->get_msg_type(header), parsed)) {
                    free(parsed);
                    snd_disconnect_channel(client);
                    return;
                }
                parsed_free(parsed);
                client->receive_data.message_start = msg_start + header->header_size +
                                                     header->get_msg_size(header);
            }
            if (client->receive_data.now == client->receive_data.message_start) {
                client->receive_data.now = client->receive_data.buf;
                client->receive_data.message_start = client->receive_data.buf;
            } else if (client->receive_data.now == client->receive_data.end) {
                memcpy(client->receive_data.buf, client->receive_data.message_start, n);
                client->receive_data.now = client->receive_data.buf + n;
                client->receive_data.message_start = client->receive_data.buf;
            }
        }
    }
}

static void snd_event(int fd, int event, void *data)
{
    SndChannelClient *client = data;

    if (event & SPICE_WATCH_EVENT_READ) {
        snd_receive(client);
    }
    if (event & SPICE_WATCH_EVENT_WRITE) {
        client->send_messages(client);
    }
}

static inline int snd_reset_send_data(SndChannelClient *client, uint16_t verb)
{
    SpiceDataHeaderOpaque *header;

    if (!client) {
        return FALSE;
    }

    header = &client->channel_client->priv->send_data.header;
    spice_marshaller_reset(client->send_data.marshaller);
    header->data = spice_marshaller_reserve_space(client->send_data.marshaller,
                                                  header->header_size);
    spice_marshaller_set_base(client->send_data.marshaller,
                              header->header_size);
    client->send_data.pos = 0;
    header->set_msg_size(header, 0);
    header->set_msg_type(header, verb);
    client->send_data.serial++;
    if (!client->channel_client->priv->is_mini_header) {
        header->set_msg_serial(header, client->send_data.serial);
        header->set_msg_sub_list(header, 0);
    }

    return TRUE;
}

static int snd_begin_send_message(SndChannelClient *client)
{
    SpiceDataHeaderOpaque *header = &client->channel_client->priv->send_data.header;

    spice_marshaller_flush(client->send_data.marshaller);
    client->send_data.size = spice_marshaller_get_total_size(client->send_data.marshaller);
    header->set_msg_size(header, client->send_data.size - header->header_size);
    return snd_send_data(client);
}

static int snd_channel_send_migrate(SndChannelClient *client)
{
    SpiceMarshaller *m = client->send_data.marshaller;
    SpiceMsgMigrate migrate;

    if (!snd_reset_send_data(client, SPICE_MSG_MIGRATE)) {
        return FALSE;
    }
    spice_debug(NULL);
    migrate.flags = 0;
    spice_marshall_msg_migrate(m, &migrate);

    return snd_begin_send_message(client);
}

static int snd_playback_send_migrate(PlaybackChannelClient *client)
{
    return snd_channel_send_migrate(SND_CHANNEL_CLIENT(client));
}

static int snd_send_volume(SndChannelClient *client, uint32_t cap, int msg)
{
    SpiceMsgAudioVolume *vol;
    uint8_t c;
    SpiceVolumeState *st = &client->channel->volume;
    SpiceMarshaller *m = client->send_data.marshaller;

    if (!red_channel_client_test_remote_cap(client->channel_client, cap)) {
        return TRUE;
    }

    vol = alloca(sizeof (SpiceMsgAudioVolume) +
                 st->volume_nchannels * sizeof (uint16_t));
    if (!snd_reset_send_data(client, msg)) {
        return FALSE;
    }
    vol->nchannels = st->volume_nchannels;
    for (c = 0; c < st->volume_nchannels; ++c) {
        vol->volume[c] = st->volume[c];
    }
    spice_marshall_SpiceMsgAudioVolume(m, vol);

    return snd_begin_send_message(client);
}

static int snd_playback_send_volume(PlaybackChannelClient *playback_client)
{
    return snd_send_volume(SND_CHANNEL_CLIENT(playback_client), SPICE_PLAYBACK_CAP_VOLUME,
                           SPICE_MSG_PLAYBACK_VOLUME);
}

static int snd_send_mute(SndChannelClient *client, uint32_t cap, int msg)
{
    SpiceMsgAudioMute mute;
    SpiceVolumeState *st = &client->channel->volume;
    SpiceMarshaller *m = client->send_data.marshaller;

    if (!red_channel_client_test_remote_cap(client->channel_client, cap)) {
        return TRUE;
    }

    if (!snd_reset_send_data(client, msg)) {
        return FALSE;
    }
    mute.mute = st->mute;
    spice_marshall_SpiceMsgAudioMute(m, &mute);

    return snd_begin_send_message(client);
}

static int snd_playback_send_mute(PlaybackChannelClient *playback_client)
{
    return snd_send_mute(SND_CHANNEL_CLIENT(playback_client), SPICE_PLAYBACK_CAP_VOLUME,
                         SPICE_MSG_PLAYBACK_MUTE);
}

static int snd_playback_send_latency(PlaybackChannelClient *playback_client)
{
    SndChannelClient *client = SND_CHANNEL_CLIENT(playback_client);
    SpiceMarshaller *m = client->send_data.marshaller;
    SpiceMsgPlaybackLatency latency_msg;

    spice_debug("latency %u", playback_client->latency);
    if (!snd_reset_send_data(client, SPICE_MSG_PLAYBACK_LATENCY)) {
        return FALSE;
    }
    latency_msg.latency_ms = playback_client->latency;
    spice_marshall_msg_playback_latency(m, &latency_msg);

    return snd_begin_send_message(client);
}
static int snd_playback_send_start(PlaybackChannelClient *playback_client)
{
    SndChannelClient *client = (SndChannelClient *)playback_client;
    SpiceMarshaller *m = client->send_data.marshaller;
    SpiceMsgPlaybackStart start;

    if (!snd_reset_send_data(client, SPICE_MSG_PLAYBACK_START)) {
        return FALSE;
    }

    start.channels = SPICE_INTERFACE_PLAYBACK_CHAN;
    start.frequency = client->channel->frequency;
    spice_assert(SPICE_INTERFACE_PLAYBACK_FMT == SPICE_INTERFACE_AUDIO_FMT_S16);
    start.format = SPICE_AUDIO_FMT_S16;
    start.time = reds_get_mm_time();
    spice_marshall_msg_playback_start(m, &start);

    return snd_begin_send_message(client);
}

static int snd_playback_send_stop(PlaybackChannelClient *playback_client)
{
    SndChannelClient *client = (SndChannelClient *)playback_client;

    if (!snd_reset_send_data(client, SPICE_MSG_PLAYBACK_STOP)) {
        return FALSE;
    }

    return snd_begin_send_message(client);
}

static int snd_playback_send_ctl(PlaybackChannelClient *playback_client)
{
    SndChannelClient *client = SND_CHANNEL_CLIENT(playback_client);

    if ((client->client_active = client->active)) {
        return snd_playback_send_start(playback_client);
    } else {
        return snd_playback_send_stop(playback_client);
    }
}

static int snd_record_send_start(RecordChannelClient *record_client)
{
    SndChannelClient *client = (SndChannelClient *)record_client;
    SpiceMarshaller *m = client->send_data.marshaller;
    SpiceMsgRecordStart start;

    if (!snd_reset_send_data(client, SPICE_MSG_RECORD_START)) {
        return FALSE;
    }

    start.channels = SPICE_INTERFACE_RECORD_CHAN;
    start.frequency = client->channel->frequency;
    spice_assert(SPICE_INTERFACE_RECORD_FMT == SPICE_INTERFACE_AUDIO_FMT_S16);
    start.format = SPICE_AUDIO_FMT_S16;
    spice_marshall_msg_record_start(m, &start);

    return snd_begin_send_message(client);
}

static int snd_record_send_stop(RecordChannelClient *record_client)
{
    SndChannelClient *client = (SndChannelClient *)record_client;

    if (!snd_reset_send_data(client, SPICE_MSG_RECORD_STOP)) {
        return FALSE;
    }

    return snd_begin_send_message(client);
}

static int snd_record_send_ctl(RecordChannelClient *record_client)
{
    SndChannelClient *client = SND_CHANNEL_CLIENT(record_client);

    if ((client->client_active = client->active)) {
        return snd_record_send_start(record_client);
    } else {
        return snd_record_send_stop(record_client);
    }
}

static int snd_record_send_volume(RecordChannelClient *record_client)
{
    return snd_send_volume(SND_CHANNEL_CLIENT(record_client), SPICE_RECORD_CAP_VOLUME,
                           SPICE_MSG_RECORD_VOLUME);
}

static int snd_record_send_mute(RecordChannelClient *record_client)
{
    return snd_send_mute(SND_CHANNEL_CLIENT(record_client), SPICE_RECORD_CAP_VOLUME,
                         SPICE_MSG_RECORD_MUTE);
}

static int snd_record_send_migrate(RecordChannelClient *record_client)
{
    /* No need for migration data: if recording has started before migration,
     * the client receives RECORD_STOP from the src before the migration completion
     * notification (when the vm is stopped).
     * Afterwards, when the vm starts on the dest, the client receives RECORD_START. */
    return snd_channel_send_migrate(SND_CHANNEL_CLIENT(record_client));
}

static int snd_playback_send_write(PlaybackChannelClient *playback_client)
{
    SndChannelClient *client = (SndChannelClient *)playback_client;
    SpiceMarshaller *m = client->send_data.marshaller;
    AudioFrame *frame;
    SpiceMsgPlaybackPacket msg;

    if (!snd_reset_send_data(client, SPICE_MSG_PLAYBACK_DATA)) {
        return FALSE;
    }

    frame = playback_client->in_progress;
    msg.time = frame->time;

    spice_marshall_msg_playback_data(m, &msg);

    if (playback_client->mode == SPICE_AUDIO_DATA_MODE_RAW) {
        spice_marshaller_add_by_ref(m, (uint8_t *)frame->samples,
                                    snd_codec_frame_size(playback_client->codec) *
                                    sizeof(frame->samples[0]));
    }
    else {
        int n = sizeof(playback_client->encode_buf);
        if (snd_codec_encode(playback_client->codec, (uint8_t *) frame->samples,
                                    snd_codec_frame_size(playback_client->codec) * sizeof(frame->samples[0]),
                                    playback_client->encode_buf, &n) != SND_CODEC_OK) {
            spice_printerr("encode failed");
            snd_disconnect_channel(client);
            return FALSE;
        }
        spice_marshaller_add_by_ref(m, playback_client->encode_buf, n);
    }

    return snd_begin_send_message(client);
}

static int playback_send_mode(PlaybackChannelClient *playback_client)
{
    SndChannelClient *client = (SndChannelClient *)playback_client;
    SpiceMarshaller *m = client->send_data.marshaller;
    SpiceMsgPlaybackMode mode;

    if (!snd_reset_send_data(client, SPICE_MSG_PLAYBACK_MODE)) {
        return FALSE;
    }
    mode.time = reds_get_mm_time();
    mode.mode = playback_client->mode;
    spice_marshall_msg_playback_mode(m, &mode);

    return snd_begin_send_message(client);
}

static void snd_playback_send(void* data)
{
    PlaybackChannelClient *playback_client = (PlaybackChannelClient*)data;
    SndChannelClient *client = SND_CHANNEL_CLIENT(playback_client);

    if (!playback_client || !snd_send_data(data)) {
        return;
    }

    while (client->command) {
        if (client->command & SND_PLAYBACK_MODE_MASK) {
            if (!playback_send_mode(playback_client)) {
                return;
            }
            client->command &= ~SND_PLAYBACK_MODE_MASK;
        }
        if (client->command & SND_PLAYBACK_PCM_MASK) {
            spice_assert(!playback_client->in_progress && playback_client->pending_frame);
            playback_client->in_progress = playback_client->pending_frame;
            playback_client->pending_frame = NULL;
            client->command &= ~SND_PLAYBACK_PCM_MASK;
            if (!snd_playback_send_write(playback_client)) {
                spice_printerr("snd_send_playback_write failed");
                return;
            }
        }
        if (client->command & SND_CTRL_MASK) {
            if (!snd_playback_send_ctl(playback_client)) {
                return;
            }
            client->command &= ~SND_CTRL_MASK;
        }
        if (client->command & SND_VOLUME_MASK) {
            if (!snd_playback_send_volume(playback_client)) {
                return;
            }
            client->command &= ~SND_VOLUME_MASK;
        }
        if (client->command & SND_MUTE_MASK) {
            if (!snd_playback_send_mute(playback_client)) {
                return;
            }
            client->command &= ~SND_MUTE_MASK;
        }
        if (client->command & SND_MIGRATE_MASK) {
            if (!snd_playback_send_migrate(playback_client)) {
                return;
            }
            client->command &= ~SND_MIGRATE_MASK;
        }
        if (client->command & SND_PLAYBACK_LATENCY_MASK) {
            if (!snd_playback_send_latency(playback_client)) {
                return;
            }
            client->command &= ~SND_PLAYBACK_LATENCY_MASK;
        }
    }
}

static void snd_record_send(void* data)
{
    RecordChannelClient *record_client = (RecordChannelClient*)data;
    SndChannelClient *client = SND_CHANNEL_CLIENT(record_client);

    if (!record_client || !snd_send_data(data)) {
        return;
    }

    while (client->command) {
        if (client->command & SND_CTRL_MASK) {
            if (!snd_record_send_ctl(record_client)) {
                return;
            }
            client->command &= ~SND_CTRL_MASK;
        }
        if (client->command & SND_VOLUME_MASK) {
            if (!snd_record_send_volume(record_client)) {
                return;
            }
            client->command &= ~SND_VOLUME_MASK;
        }
        if (client->command & SND_MUTE_MASK) {
            if (!snd_record_send_mute(record_client)) {
                return;
            }
            client->command &= ~SND_MUTE_MASK;
        }
        if (client->command & SND_MIGRATE_MASK) {
            if (!snd_record_send_migrate(record_client)) {
                return;
            }
            client->command &= ~SND_MIGRATE_MASK;
        }
    }
}

static SndChannelClient *__new_channel(SndChannel *channel, int size, uint32_t channel_id,
                                       RedClient *red_client,
                                       RedsStream *stream,
                                       snd_channel_send_messages_proc send_messages,
                                       snd_channel_handle_message_proc handle_message,
                                       snd_channel_on_message_done_proc on_message_done,
                                       snd_channel_cleanup_channel_proc cleanup,
                                       uint32_t *common_caps, int num_common_caps,
                                       uint32_t *caps, int num_caps)
{
    SndChannelClient *client;
    int delay_val;
    int flags;
#ifdef SO_PRIORITY
    int priority;
#endif
    int tos;
    MainChannelClient *mcc = red_client_get_main(red_client);
    RedsState *reds = red_channel_get_server(RED_CHANNEL(channel));

    spice_assert(stream);
    if ((flags = fcntl(stream->socket, F_GETFL)) == -1) {
        spice_printerr("accept failed, %s", strerror(errno));
        goto error1;
    }

#ifdef SO_PRIORITY
    priority = 6;
    if (setsockopt(stream->socket, SOL_SOCKET, SO_PRIORITY, (void*)&priority,
                   sizeof(priority)) == -1) {
        if (errno != ENOTSUP) {
            spice_printerr("setsockopt failed, %s", strerror(errno));
        }
    }
#endif

    tos = IPTOS_LOWDELAY;
    if (setsockopt(stream->socket, IPPROTO_IP, IP_TOS, (void*)&tos, sizeof(tos)) == -1) {
        if (errno != ENOTSUP) {
            spice_printerr("setsockopt failed, %s", strerror(errno));
        }
    }

    delay_val = main_channel_client_is_low_bandwidth(mcc) ? 0 : 1;
    if (setsockopt(stream->socket, IPPROTO_TCP, TCP_NODELAY, &delay_val, sizeof(delay_val)) == -1) {
        if (errno != ENOTSUP) {
            spice_printerr("setsockopt failed, %s", strerror(errno));
        }
    }

    if (fcntl(stream->socket, F_SETFL, flags | O_NONBLOCK) == -1) {
        spice_printerr("accept failed, %s", strerror(errno));
        goto error1;
    }

    spice_assert(size >= sizeof(*client));
    client = spice_malloc0(size);
    client->refs = 1;
    client->parser = spice_get_client_channel_parser(channel_id, NULL);
    client->stream = stream;
    client->channel = channel;
    client->receive_data.message_start = client->receive_data.buf;
    client->receive_data.now = client->receive_data.buf;
    client->receive_data.end = client->receive_data.buf + sizeof(client->receive_data.buf);
    client->send_data.marshaller = spice_marshaller_new();

    stream->watch = reds_core_watch_add(reds, stream->socket, SPICE_WATCH_EVENT_READ,
                                        snd_event, client);
    if (stream->watch == NULL) {
        spice_printerr("watch_add failed, %s", strerror(errno));
        goto error2;
    }

    client->send_messages = send_messages;
    client->handle_message = handle_message;
    client->on_message_done = on_message_done;
    client->cleanup = cleanup;

    client->channel_client =
        dummy_channel_client_create(RED_CHANNEL(channel), red_client,
                                    num_common_caps, common_caps, num_caps, caps);
    if (!client->channel_client) {
        goto error2;
    }
    return client;

error2:
    free(client);

error1:
    reds_stream_free(stream);
    return NULL;
}

static int snd_channel_config_socket(RedChannelClient *rcc)
{
    g_assert_not_reached();
}

static void snd_channel_on_disconnect(RedChannelClient *rcc)
{
    g_assert_not_reached();
}

static uint8_t*
snd_channel_client_alloc_recv_buf(RedChannelClient *rcc, uint16_t type, uint32_t size)
{
    g_assert_not_reached();
}

static void
snd_channel_client_release_recv_buf(RedChannelClient *rcc, uint16_t type, uint32_t size,
                                    uint8_t *msg)
{
    g_assert_not_reached();
}

static void snd_disconnect_channel_client(RedChannelClient *rcc)
{
    SndChannel *channel;
    RedChannel *red_channel = red_channel_client_get_channel(rcc);
    uint32_t type;

    channel = SND_CHANNEL(red_channel);
    spice_assert(channel);
    g_object_get(red_channel, "channel-type", &type, NULL);

    spice_debug("channel-type=%d", type);
    if (channel->connection) {
        spice_assert(channel->connection->channel_client == rcc);
        snd_disconnect_channel(channel->connection);
    }
}

static void snd_set_command(SndChannelClient *client, uint32_t command)
{
    if (!client) {
        return;
    }
    client->command |= command;
}

SPICE_GNUC_VISIBLE void spice_server_playback_set_volume(SpicePlaybackInstance *sin,
                                                  uint8_t nchannels,
                                                  uint16_t *volume)
{
    SpiceVolumeState *st = &sin->st->channel.volume;
    SndChannelClient *client = sin->st->channel.connection;
    PlaybackChannelClient *playback_client = SPICE_CONTAINEROF(client, PlaybackChannelClient, base);

    st->volume_nchannels = nchannels;
    free(st->volume);
    st->volume = spice_memdup(volume, sizeof(uint16_t) * nchannels);

    if (!client || nchannels == 0)
        return;

    snd_playback_send_volume(playback_client);
}

SPICE_GNUC_VISIBLE void spice_server_playback_set_mute(SpicePlaybackInstance *sin, uint8_t mute)
{
    SpiceVolumeState *st = &sin->st->channel.volume;
    SndChannelClient *client = sin->st->channel.connection;
    PlaybackChannelClient *playback_client = SPICE_CONTAINEROF(client, PlaybackChannelClient, base);

    st->mute = mute;

    if (!client)
        return;

    snd_playback_send_mute(playback_client);
}

static void snd_playback_start(SndChannel *channel)
{
    SndChannelClient *client = channel->connection;

    channel->active = 1;
    if (!client)
        return;
    spice_assert(!client->active);
    reds_disable_mm_time(snd_channel_get_server(client));
    client->active = TRUE;
    if (!client->client_active) {
        snd_set_command(client, SND_CTRL_MASK);
        snd_playback_send(client);
    } else {
        client->command &= ~SND_CTRL_MASK;
    }
}

SPICE_GNUC_VISIBLE void spice_server_playback_start(SpicePlaybackInstance *sin)
{
    return snd_playback_start(&sin->st->channel);
}

SPICE_GNUC_VISIBLE void spice_server_playback_stop(SpicePlaybackInstance *sin)
{
    SndChannelClient *client = sin->st->channel.connection;

    sin->st->channel.active = 0;
    if (!client)
        return;
    PlaybackChannelClient *playback_client = SPICE_CONTAINEROF(client, PlaybackChannelClient, base);
    spice_assert(client->active);
    reds_enable_mm_time(snd_channel_get_server(client));
    client->active = FALSE;
    if (client->client_active) {
        snd_set_command(client, SND_CTRL_MASK);
        snd_playback_send(client);
    } else {
        client->command &= ~SND_CTRL_MASK;
        client->command &= ~SND_PLAYBACK_PCM_MASK;

        if (playback_client->pending_frame) {
            spice_assert(!playback_client->in_progress);
            snd_playback_free_frame(playback_client,
                                    playback_client->pending_frame);
            playback_client->pending_frame = NULL;
        }
    }
}

SPICE_GNUC_VISIBLE void spice_server_playback_get_buffer(SpicePlaybackInstance *sin,
                                                         uint32_t **frame, uint32_t *num_samples)
{
    SndChannelClient *client = sin->st->channel.connection;
    PlaybackChannelClient *playback_client = SPICE_CONTAINEROF(client, PlaybackChannelClient, base);

    if (!client || !playback_client->free_frames) {
        *frame = NULL;
        *num_samples = 0;
        return;
    }
    spice_assert(client->active);
    if (!playback_client->free_frames->allocated) {
        playback_client->free_frames->allocated = TRUE;
        ++playback_client->frames->refs;
    }

    *frame = playback_client->free_frames->samples;
    playback_client->free_frames = playback_client->free_frames->next;
    *num_samples = snd_codec_frame_size(playback_client->codec);
}

SPICE_GNUC_VISIBLE void spice_server_playback_put_samples(SpicePlaybackInstance *sin, uint32_t *samples)
{
    PlaybackChannelClient *playback_client;
    AudioFrame *frame;

    frame = SPICE_CONTAINEROF(samples, AudioFrame, samples[0]);
    if (frame->allocated) {
        frame->allocated = FALSE;
        if (--frame->container->refs == 0) {
            free(frame->container);
            return;
        }
    }
    playback_client = frame->client;
    if (!playback_client || sin->st->channel.connection != SND_CHANNEL_CLIENT(playback_client)) {
        /* lost last reference, client has been destroyed previously */
        spice_info("audio samples belong to a disconnected client");
        return;
    }
    spice_assert(SND_CHANNEL_CLIENT(playback_client)->active);

    if (playback_client->pending_frame) {
        snd_playback_free_frame(playback_client, playback_client->pending_frame);
    }
    frame->time = reds_get_mm_time();
    playback_client->pending_frame = frame;
    snd_set_command(SND_CHANNEL_CLIENT(playback_client), SND_PLAYBACK_PCM_MASK);
    snd_playback_send(SND_CHANNEL_CLIENT(playback_client));
}

void snd_set_playback_latency(RedClient *client, uint32_t latency)
{
    SndChannel *now = snd_channels;

    for (; now; now = now->next) {
        uint32_t type;
        g_object_get(RED_CHANNEL(now), "channel-type", &type, NULL);
        if (type == SPICE_CHANNEL_PLAYBACK && now->connection &&
            red_channel_client_get_client(now->connection->channel_client) == client) {

            if (red_channel_client_test_remote_cap(now->connection->channel_client,
                SPICE_PLAYBACK_CAP_LATENCY)) {
                PlaybackChannelClient* playback = (PlaybackChannelClient*)now->connection;

                playback->latency = latency;
                snd_set_command(now->connection, SND_PLAYBACK_LATENCY_MASK);
                snd_playback_send(now->connection);
            } else {
                spice_debug("client doesn't not support SPICE_PLAYBACK_CAP_LATENCY");
            }
        }
    }
}

static int snd_desired_audio_mode(int playback_compression, int frequency,
                                  int client_can_celt, int client_can_opus)
{
    if (! playback_compression)
        return SPICE_AUDIO_DATA_MODE_RAW;

    if (client_can_opus && snd_codec_is_capable(SPICE_AUDIO_DATA_MODE_OPUS, frequency))
        return SPICE_AUDIO_DATA_MODE_OPUS;

    if (client_can_celt && snd_codec_is_capable(SPICE_AUDIO_DATA_MODE_CELT_0_5_1, frequency))
        return SPICE_AUDIO_DATA_MODE_CELT_0_5_1;

    return SPICE_AUDIO_DATA_MODE_RAW;
}

static void on_new_playback_channel_client(SndChannel *channel, SndChannelClient *client)
{
    RedsState *reds = red_channel_get_server(RED_CHANNEL(channel));

    spice_assert(client);

    channel->connection = client;
    snd_set_command(client, SND_PLAYBACK_MODE_MASK);
    if (client->active) {
        snd_set_command(client, SND_CTRL_MASK);
    }
    if (channel->volume.volume_nchannels) {
        snd_set_command(client, SND_VOLUME_MUTE_MASK);
    }
    if (client->active) {
        reds_disable_mm_time(reds);
    }
}

static void snd_playback_cleanup(SndChannelClient *client)
{
    PlaybackChannelClient *playback_client = SPICE_CONTAINEROF(client, PlaybackChannelClient, base);
    int i;

    // free frames, unref them
    for (i = 0; i < NUM_AUDIO_FRAMES; ++i) {
        playback_client->frames->items[i].client = NULL;
    }
    if (--playback_client->frames->refs == 0) {
        free(playback_client->frames);
    }

    if (client->active) {
        reds_enable_mm_time(snd_channel_get_server(client));
    }

    snd_codec_destroy(&playback_client->codec);
}

static void snd_set_playback_peer(RedChannel *red_channel, RedClient *client, RedsStream *stream,
                                  G_GNUC_UNUSED int migration,
                                  int num_common_caps, uint32_t *common_caps,
                                  int num_caps, uint32_t *caps)
{
    SndChannel *channel = SND_CHANNEL(red_channel);
    PlaybackChannelClient *playback_client;

    snd_disconnect_channel(channel->connection);

    if (!(playback_client = (PlaybackChannelClient *)__new_channel(channel,
                                                                   sizeof(*playback_client),
                                                                   SPICE_CHANNEL_PLAYBACK,
                                                                   client,
                                                                   stream,
                                                                   snd_playback_send,
                                                                   snd_playback_handle_message,
                                                                   snd_playback_on_message_done,
                                                                   snd_playback_cleanup,
                                                                   common_caps, num_common_caps,
                                                                   caps, num_caps))) {
        return;
    }

    snd_playback_alloc_frames(playback_client);

    int client_can_celt = red_channel_client_test_remote_cap(playback_client->base.channel_client,
                                          SPICE_PLAYBACK_CAP_CELT_0_5_1);
    int client_can_opus = red_channel_client_test_remote_cap(playback_client->base.channel_client,
                                          SPICE_PLAYBACK_CAP_OPUS);
    int playback_compression =
        reds_config_get_playback_compression(red_channel_get_server(red_channel));
    int desired_mode = snd_desired_audio_mode(playback_compression, channel->frequency,
                                              client_can_celt, client_can_opus);
    playback_client->mode = SPICE_AUDIO_DATA_MODE_RAW;
    if (desired_mode != SPICE_AUDIO_DATA_MODE_RAW) {
        if (snd_codec_create(&playback_client->codec, desired_mode, channel->frequency,
                             SND_CODEC_ENCODE) == SND_CODEC_OK) {
            playback_client->mode = desired_mode;
        } else {
            spice_printerr("create encoder failed");
        }
    }

    if (!red_client_during_migrate_at_target(client)) {
        on_new_playback_channel_client(channel, SND_CHANNEL_CLIENT(playback_client));
    }

    if (channel->active) {
        snd_playback_start(channel);
    }
    snd_playback_send(channel->connection);
}

static void snd_record_migrate_channel_client(RedChannelClient *rcc)
{
    SndChannel *channel;
    RedChannel *red_channel = red_channel_client_get_channel(rcc);

    channel = SND_CHANNEL(red_channel);
    spice_assert(channel);

    if (channel->connection) {
        spice_assert(channel->connection->channel_client == rcc);
        snd_set_command(channel->connection, SND_MIGRATE_MASK);
        snd_record_send(channel->connection);
    }
}

SPICE_GNUC_VISIBLE void spice_server_record_set_volume(SpiceRecordInstance *sin,
                                                uint8_t nchannels,
                                                uint16_t *volume)
{
    SpiceVolumeState *st = &sin->st->channel.volume;
    SndChannelClient *client = sin->st->channel.connection;
    RecordChannelClient *record_client = SPICE_CONTAINEROF(client, RecordChannelClient, base);

    st->volume_nchannels = nchannels;
    free(st->volume);
    st->volume = spice_memdup(volume, sizeof(uint16_t) * nchannels);

    if (!client || nchannels == 0)
        return;

    snd_record_send_volume(record_client);
}

SPICE_GNUC_VISIBLE void spice_server_record_set_mute(SpiceRecordInstance *sin, uint8_t mute)
{
    SpiceVolumeState *st = &sin->st->channel.volume;
    SndChannelClient *client = sin->st->channel.connection;
    RecordChannelClient *record_client = SPICE_CONTAINEROF(client, RecordChannelClient, base);

    st->mute = mute;

    if (!client)
        return;

    snd_record_send_mute(record_client);
}

static void snd_record_start(SndChannel *channel)
{
    SndChannelClient *client = channel->connection;

    channel->active = 1;
    if (!client)
        return;
    RecordChannelClient *record_client = SPICE_CONTAINEROF(client, RecordChannelClient, base);
    spice_assert(!client->active);
    record_client->read_pos = record_client->write_pos = 0;   //todo: improve by
                                                              //stream generation
    client->active = TRUE;
    if (!client->client_active) {
        snd_set_command(client, SND_CTRL_MASK);
        snd_record_send(client);
    } else {
        client->command &= ~SND_CTRL_MASK;
    }
}

SPICE_GNUC_VISIBLE void spice_server_record_start(SpiceRecordInstance *sin)
{
    snd_record_start(&sin->st->channel);
}

SPICE_GNUC_VISIBLE void spice_server_record_stop(SpiceRecordInstance *sin)
{
    SndChannelClient *client = sin->st->channel.connection;

    sin->st->channel.active = 0;
    if (!client)
        return;
    spice_assert(client->active);
    client->active = FALSE;
    if (client->client_active) {
        snd_set_command(client, SND_CTRL_MASK);
        snd_record_send(client);
    } else {
        client->command &= ~SND_CTRL_MASK;
    }
}

SPICE_GNUC_VISIBLE uint32_t spice_server_record_get_samples(SpiceRecordInstance *sin,
                                                            uint32_t *samples, uint32_t bufsize)
{
    SndChannelClient *client = sin->st->channel.connection;
    uint32_t read_pos;
    uint32_t now;
    uint32_t len;

    if (!client)
        return 0;
    RecordChannelClient *record_client = SPICE_CONTAINEROF(client, RecordChannelClient, base);
    spice_assert(client->active);

    if (record_client->write_pos < RECORD_SAMPLES_SIZE / 2) {
        return 0;
    }

    len = MIN(record_client->write_pos - record_client->read_pos, bufsize);

    if (len < bufsize) {
        SndChannel *channel = client->channel;
        snd_receive(client);
        if (!channel->connection) {
            return 0;
        }
        len = MIN(record_client->write_pos - record_client->read_pos, bufsize);
    }

    read_pos = record_client->read_pos % RECORD_SAMPLES_SIZE;
    record_client->read_pos += len;
    now = MIN(len, RECORD_SAMPLES_SIZE - read_pos);
    memcpy(samples, &record_client->samples[read_pos], now * 4);
    if (now < len) {
        memcpy(samples + now, record_client->samples, (len - now) * 4);
    }
    return len;
}

static uint32_t snd_get_best_rate(SndChannelClient *client, uint32_t cap_opus)
{
    int client_can_opus = TRUE;
    if (client) {
        client_can_opus = red_channel_client_test_remote_cap(client->channel_client, cap_opus);
    }

    if (client_can_opus && snd_codec_is_capable(SPICE_AUDIO_DATA_MODE_OPUS, SND_CODEC_ANY_FREQUENCY))
        return SND_CODEC_OPUS_PLAYBACK_FREQ;

    return SND_CODEC_CELT_PLAYBACK_FREQ;
}

static void snd_set_rate(SndChannel *channel, uint32_t frequency, uint32_t cap_opus)
{
    RedChannel *red_channel = RED_CHANNEL(channel);
    channel->frequency = frequency;
    if (red_channel && snd_codec_is_capable(SPICE_AUDIO_DATA_MODE_OPUS, frequency)) {
        red_channel_set_cap(red_channel, cap_opus);
    }
}

SPICE_GNUC_VISIBLE uint32_t spice_server_get_best_playback_rate(SpicePlaybackInstance *sin)
{
    return snd_get_best_rate(sin ? sin->st->channel.connection : NULL, SPICE_PLAYBACK_CAP_OPUS);
}

SPICE_GNUC_VISIBLE void spice_server_set_playback_rate(SpicePlaybackInstance *sin, uint32_t frequency)
{
    snd_set_rate(&sin->st->channel, frequency, SPICE_PLAYBACK_CAP_OPUS);
}

SPICE_GNUC_VISIBLE uint32_t spice_server_get_best_record_rate(SpiceRecordInstance *sin)
{
    return snd_get_best_rate(sin ? sin->st->channel.connection : NULL, SPICE_RECORD_CAP_OPUS);
}

SPICE_GNUC_VISIBLE void spice_server_set_record_rate(SpiceRecordInstance *sin, uint32_t frequency)
{
    snd_set_rate(&sin->st->channel, frequency, SPICE_RECORD_CAP_OPUS);
}

static void on_new_record_channel_client(SndChannel *channel, SndChannelClient *client)
{
    spice_assert(client);

    channel->connection = client;
    if (channel->volume.volume_nchannels) {
        snd_set_command(client, SND_VOLUME_MUTE_MASK);
    }
    if (client->active) {
        snd_set_command(client, SND_CTRL_MASK);
    }
}

static void snd_record_cleanup(SndChannelClient *client)
{
    RecordChannelClient *record_client = SPICE_CONTAINEROF(client, RecordChannelClient, base);
    snd_codec_destroy(&record_client->codec);
}

static void snd_set_record_peer(RedChannel *red_channel, RedClient *client, RedsStream *stream,
                                G_GNUC_UNUSED int migration,
                                int num_common_caps, uint32_t *common_caps,
                                int num_caps, uint32_t *caps)
{
    SndChannel *channel = SND_CHANNEL(red_channel);
    RecordChannelClient *record_client;

    snd_disconnect_channel(channel->connection);

    if (!(record_client = (RecordChannelClient *)__new_channel(channel,
                                                               sizeof(*record_client),
                                                               SPICE_CHANNEL_RECORD,
                                                               client,
                                                               stream,
                                                               snd_record_send,
                                                               snd_record_handle_message,
                                                               snd_record_on_message_done,
                                                               snd_record_cleanup,
                                                               common_caps, num_common_caps,
                                                               caps, num_caps))) {
        return;
    }

    record_client->mode = SPICE_AUDIO_DATA_MODE_RAW;

    on_new_record_channel_client(channel, SND_CHANNEL_CLIENT(record_client));
    if (channel->active) {
        snd_record_start(channel);
    }
    snd_record_send(channel->connection);
}

static void snd_playback_migrate_channel_client(RedChannelClient *rcc)
{
    SndChannel *channel;
    RedChannel *red_channel = red_channel_client_get_channel(rcc);

    channel = SND_CHANNEL(red_channel);
    spice_assert(channel);
    spice_debug(NULL);

    if (channel->connection) {
        spice_assert(channel->connection->channel_client == rcc);
        snd_set_command(channel->connection, SND_MIGRATE_MASK);
        snd_playback_send(channel->connection);
    }
}

static void add_channel(SndChannel *channel)
{
    channel->next = snd_channels;
    snd_channels = channel;
}

static void remove_channel(SndChannel *channel)
{
    SndChannel **now = &snd_channels;
    while (*now) {
        if (*now == channel) {
            *now = channel->next;
            return;
        }
        now = &(*now)->next;
    }
    spice_printerr("not found");
}

static void
snd_channel_init(SndChannel *self)
{
    self->frequency = SND_CODEC_CELT_PLAYBACK_FREQ; /* Default to the legacy rate */
}

static void
snd_channel_class_init(SndChannelClass *klass)
{
    RedChannelClass *channel_class = RED_CHANNEL_CLASS(klass);

    channel_class->config_socket = snd_channel_config_socket;
    channel_class->alloc_recv_buf = snd_channel_client_alloc_recv_buf;
    channel_class->release_recv_buf = snd_channel_client_release_recv_buf;
    channel_class->on_disconnect = snd_channel_on_disconnect;
}

static void
playback_channel_init(PlaybackChannel *self)
{
}

static void
playback_channel_constructed(GObject *object)
{
    ClientCbs client_cbs = { NULL, };
    SndChannel *self = SND_CHANNEL(object);
    RedsState *reds = red_channel_get_server(RED_CHANNEL(self));

    G_OBJECT_CLASS(playback_channel_parent_class)->constructed(object);

    client_cbs.connect = snd_set_playback_peer;
    client_cbs.disconnect = snd_disconnect_channel_client;
    client_cbs.migrate = snd_playback_migrate_channel_client;
    red_channel_register_client_cbs(RED_CHANNEL(self), &client_cbs, self);

    if (snd_codec_is_capable(SPICE_AUDIO_DATA_MODE_CELT_0_5_1, SND_CODEC_ANY_FREQUENCY)) {
        red_channel_set_cap(RED_CHANNEL(self), SPICE_PLAYBACK_CAP_CELT_0_5_1);
    }
    red_channel_set_cap(RED_CHANNEL(self), SPICE_PLAYBACK_CAP_VOLUME);

    add_channel(self);
    reds_register_channel(reds, RED_CHANNEL(self));
}

static void
playback_channel_class_init(PlaybackChannelClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS(klass);

    object_class->constructed = playback_channel_constructed;
}

void snd_attach_playback(RedsState *reds, SpicePlaybackInstance *sin)
{
    sin->st = g_object_new(TYPE_PLAYBACK_CHANNEL,
                           "spice-server", reds,
                           "core-interface", reds_get_core_interface(reds),
                           "channel-type", SPICE_CHANNEL_PLAYBACK,
                           "id", 0,
                           NULL);
}

static void
record_channel_init(RecordChannel *self)
{
}

static void
record_channel_constructed(GObject *object)
{
    ClientCbs client_cbs = { NULL, };
    SndChannel *self = SND_CHANNEL(object);
    RedsState *reds = red_channel_get_server(RED_CHANNEL(self));

    G_OBJECT_CLASS(record_channel_parent_class)->constructed(object);

    client_cbs.connect = snd_set_record_peer;
    client_cbs.disconnect = snd_disconnect_channel_client;
    client_cbs.migrate = snd_record_migrate_channel_client;
    red_channel_register_client_cbs(RED_CHANNEL(self), &client_cbs, self);

    if (snd_codec_is_capable(SPICE_AUDIO_DATA_MODE_CELT_0_5_1, SND_CODEC_ANY_FREQUENCY)) {
        red_channel_set_cap(RED_CHANNEL(self), SPICE_RECORD_CAP_CELT_0_5_1);
    }
    red_channel_set_cap(RED_CHANNEL(self), SPICE_RECORD_CAP_VOLUME);

    add_channel(self);
    reds_register_channel(reds, RED_CHANNEL(self));
}

static void
record_channel_class_init(RecordChannelClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS(klass);

    object_class->constructed = record_channel_constructed;
}

void snd_attach_record(RedsState *reds, SpiceRecordInstance *sin)
{
    sin->st = g_object_new(TYPE_RECORD_CHANNEL,
                           "spice-server", reds,
                           "core-interface", reds_get_core_interface(reds),
                           "channel-type", SPICE_CHANNEL_RECORD,
                           "id", 0,
                           NULL);
}

static void snd_detach_common(SndChannel *channel)
{
    if (!channel) {
        return;
    }
    RedsState *reds = red_channel_get_server(RED_CHANNEL(channel));

    remove_channel(channel);
    snd_disconnect_channel(channel->connection);
    reds_unregister_channel(reds, RED_CHANNEL(channel));
    free(channel->volume.volume);
    channel->volume.volume = NULL;
    red_channel_destroy(RED_CHANNEL(channel));
}

void snd_detach_playback(SpicePlaybackInstance *sin)
{
    snd_detach_common(&sin->st->channel);
}

void snd_detach_record(SpiceRecordInstance *sin)
{
    snd_detach_common(&sin->st->channel);
}

void snd_set_playback_compression(int on)
{
    SndChannel *now = snd_channels;

    for (; now; now = now->next) {
        uint32_t type;
        g_object_get(RED_CHANNEL(now), "channel-type", &type, NULL);
        if (type == SPICE_CHANNEL_PLAYBACK && now->connection) {
            PlaybackChannelClient* playback = (PlaybackChannelClient*)now->connection;
            int client_can_celt = red_channel_client_test_remote_cap(playback->base.channel_client,
                                    SPICE_PLAYBACK_CAP_CELT_0_5_1);
            int client_can_opus = red_channel_client_test_remote_cap(playback->base.channel_client,
                                    SPICE_PLAYBACK_CAP_OPUS);
            int desired_mode = snd_desired_audio_mode(on, now->frequency,
                                                      client_can_opus, client_can_celt);
            if (playback->mode != desired_mode) {
                playback->mode = desired_mode;
                snd_set_command(now->connection, SND_PLAYBACK_MODE_MASK);
            }
        }
    }
}

static void snd_playback_alloc_frames(PlaybackChannelClient *playback)
{
    int i;

    playback->frames = spice_new0(AudioFrameContainer, 1);
    playback->frames->refs = 1;
    for (i = 0; i < NUM_AUDIO_FRAMES; ++i) {
        playback->frames->items[i].container = playback->frames;
        snd_playback_free_frame(playback, &playback->frames->items[i]);
    }
}
