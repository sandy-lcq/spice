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
#include "dummy-channel.h"
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

#define SND_PLAYBACK_MODE_MASK (1 << SND_PLAYBACK_MODE)
#define SND_PLAYBACK_PCM_MASK (1 << SND_PLAYBACK_PCM)
#define SND_PLAYBACK_LATENCY_MASK ( 1 << SND_PLAYBACK_LATENCY)

typedef struct SndChannelClient SndChannelClient;
typedef void (*snd_channel_send_messages_proc)(void *in_channel);
typedef int (*snd_channel_handle_message_proc)(SndChannelClient *client, size_t size, uint32_t type, void *message);
typedef void (*snd_channel_on_message_done_proc)(SndChannelClient *client);
typedef void (*snd_channel_cleanup_channel_proc)(SndChannelClient *client);

typedef struct SndWorker SndWorker;

/* Connects an audio client to a Spice client */
struct SndChannelClient {
    RedsStream *stream;
    SndWorker *worker;
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

typedef struct PlaybackChannel PlaybackChannel;

typedef struct AudioFrame AudioFrame;
struct AudioFrame {
    uint32_t time;
    uint32_t samples[SND_CODEC_MAX_FRAME_SIZE];
    PlaybackChannel *channel;
    AudioFrame *next;
};

struct PlaybackChannel {
    SndChannelClient base;
    AudioFrame frames[3];
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

/* Base class for SpicePlaybackState and SpiceRecordState */
struct SndWorker {
    RedChannel *base_channel;
    SndChannelClient *connection; /* Only one client is supported */
    SndWorker *next; /* For the global SndWorker list */

    int active;
    SpiceVolumeState volume;
    uint32_t frequency;
};

struct SpicePlaybackState {
    struct SndWorker worker;
};

struct SpiceRecordState {
    struct SndWorker worker;
};

typedef struct RecordChannel {
    SndChannelClient base;
    uint32_t samples[RECORD_SAMPLES_SIZE];
    uint32_t write_pos;
    uint32_t read_pos;
    uint32_t mode;
    uint32_t mode_time;
    uint32_t start_time;
    SndCodec codec;
    uint8_t  decode_buf[SND_CODEC_MAX_FRAME_BYTES];
} RecordChannel;

/* A list of all Spice{Playback,Record}State objects */
static SndWorker *workers;

static void snd_receive(SndChannelClient *client);
static void snd_playback_start(SndWorker *worker);
static void snd_record_start(SndWorker *worker);

static SndChannelClient *snd_channel_ref(SndChannelClient *client)
{
    client->refs++;
    return client;
}

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
    return red_channel_get_server(client->worker->base_channel);
}

static void snd_disconnect_channel(SndChannelClient *client)
{
    SndWorker *worker;
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
    worker = client->worker;
    client->cleanup(client);
    red_channel_client_disconnect(worker->connection->channel_client);
    worker->connection->channel_client = NULL;
    reds_core_watch_remove(reds, client->stream->watch);
    client->stream->watch = NULL;
    reds_stream_free(client->stream);
    client->stream = NULL;
    spice_marshaller_destroy(client->send_data.marshaller);
    snd_channel_unref(client);
    worker->connection = NULL;
}

static void snd_playback_free_frame(PlaybackChannel *playback_channel, AudioFrame *frame)
{
    frame->channel = playback_channel;
    frame->next = playback_channel->free_frames;
    playback_channel->free_frames = frame;
}

static void snd_playback_on_message_done(SndChannelClient *client)
{
    PlaybackChannel *playback_channel = (PlaybackChannel *)client;
    if (playback_channel->in_progress) {
        snd_playback_free_frame(playback_channel, playback_channel->in_progress);
        playback_channel->in_progress = NULL;
        if (playback_channel->pending_frame) {
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

static int snd_record_handle_write(RecordChannel *record_channel, size_t size, void *message)
{
    SpiceMsgcRecordPacket *packet;
    uint32_t write_pos;
    uint32_t* data;
    uint32_t len;
    uint32_t now;

    if (!record_channel) {
        return FALSE;
    }

    packet = (SpiceMsgcRecordPacket *)message;

    if (record_channel->mode == SPICE_AUDIO_DATA_MODE_RAW) {
        data = (uint32_t *)packet->data;
        size = packet->data_size >> 2;
        size = MIN(size, RECORD_SAMPLES_SIZE);
     } else {
        int decode_size;
        decode_size = sizeof(record_channel->decode_buf);
        if (snd_codec_decode(record_channel->codec, packet->data, packet->data_size,
                    record_channel->decode_buf, &decode_size) != SND_CODEC_OK)
            return FALSE;
        data = (uint32_t *) record_channel->decode_buf;
        size = decode_size >> 2;
    }

    write_pos = record_channel->write_pos % RECORD_SAMPLES_SIZE;
    record_channel->write_pos += size;
    len = RECORD_SAMPLES_SIZE - write_pos;
    now = MIN(len, size);
    size -= now;
    memcpy(record_channel->samples + write_pos, data, now << 2);

    if (size) {
        memcpy(record_channel->samples, data + now, size << 2);
    }

    if (record_channel->write_pos - record_channel->read_pos > RECORD_SAMPLES_SIZE) {
        record_channel->read_pos = record_channel->write_pos - RECORD_SAMPLES_SIZE;
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
    RecordChannel *record_channel = (RecordChannel *)client;

    if (!client) {
        return FALSE;
    }
    switch (type) {
    case SPICE_MSGC_RECORD_DATA:
        return snd_record_handle_write((RecordChannel *)client, size, message);
    case SPICE_MSGC_RECORD_MODE: {
        SpiceMsgcRecordMode *mode = (SpiceMsgcRecordMode *)message;
        SndWorker *worker = client->worker;
        record_channel->mode_time = mode->time;
        if (mode->mode != SPICE_AUDIO_DATA_MODE_RAW) {
            if (snd_codec_is_capable(mode->mode, worker->frequency)) {
                if (snd_codec_create(&record_channel->codec, mode->mode, worker->frequency,
                                     SND_CODEC_DECODE) == SND_CODEC_OK) {
                    record_channel->mode = mode->mode;
                } else {
                    spice_printerr("create decoder failed");
                    return FALSE;
                }
            }
            else {
                spice_printerr("unsupported mode %d", record_channel->mode);
                return FALSE;
            }
        }
        else
            record_channel->mode = mode->mode;
        break;
    }

    case SPICE_MSGC_RECORD_START_MARK: {
        SpiceMsgcRecordStartMark *mark = (SpiceMsgcRecordStartMark *)message;
        record_channel->start_time = mark->time;
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
    SpiceMsgMigrate migrate;

    if (!snd_reset_send_data(client, SPICE_MSG_MIGRATE)) {
        return FALSE;
    }
    spice_debug(NULL);
    migrate.flags = 0;
    spice_marshall_msg_migrate(client->send_data.marshaller, &migrate);

    return snd_begin_send_message(client);
}

static int snd_playback_send_migrate(PlaybackChannel *channel)
{
    return snd_channel_send_migrate(&channel->base);
}

static int snd_send_volume(SndChannelClient *client, uint32_t cap, int msg)
{
    SpiceMsgAudioVolume *vol;
    uint8_t c;
    SpiceVolumeState *st = &client->worker->volume;

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
    spice_marshall_SpiceMsgAudioVolume(client->send_data.marshaller, vol);

    return snd_begin_send_message(client);
}

static int snd_playback_send_volume(PlaybackChannel *playback_channel)
{
    return snd_send_volume(&playback_channel->base, SPICE_PLAYBACK_CAP_VOLUME,
                           SPICE_MSG_PLAYBACK_VOLUME);
}

static int snd_send_mute(SndChannelClient *client, uint32_t cap, int msg)
{
    SpiceMsgAudioMute mute;
    SpiceVolumeState *st = &client->worker->volume;

    if (!red_channel_client_test_remote_cap(client->channel_client, cap)) {
        return TRUE;
    }

    if (!snd_reset_send_data(client, msg)) {
        return FALSE;
    }
    mute.mute = st->mute;
    spice_marshall_SpiceMsgAudioMute(client->send_data.marshaller, &mute);

    return snd_begin_send_message(client);
}

static int snd_playback_send_mute(PlaybackChannel *playback_channel)
{
    return snd_send_mute(&playback_channel->base, SPICE_PLAYBACK_CAP_VOLUME,
                         SPICE_MSG_PLAYBACK_MUTE);
}

static int snd_playback_send_latency(PlaybackChannel *playback_channel)
{
    SndChannelClient *client = &playback_channel->base;
    SpiceMsgPlaybackLatency latency_msg;

    spice_debug("latency %u", playback_channel->latency);
    if (!snd_reset_send_data(client, SPICE_MSG_PLAYBACK_LATENCY)) {
        return FALSE;
    }
    latency_msg.latency_ms = playback_channel->latency;
    spice_marshall_msg_playback_latency(client->send_data.marshaller, &latency_msg);

    return snd_begin_send_message(client);
}
static int snd_playback_send_start(PlaybackChannel *playback_channel)
{
    SndChannelClient *client = (SndChannelClient *)playback_channel;
    SpiceMsgPlaybackStart start;

    if (!snd_reset_send_data(client, SPICE_MSG_PLAYBACK_START)) {
        return FALSE;
    }

    start.channels = SPICE_INTERFACE_PLAYBACK_CHAN;
    start.frequency = client->worker->frequency;
    spice_assert(SPICE_INTERFACE_PLAYBACK_FMT == SPICE_INTERFACE_AUDIO_FMT_S16);
    start.format = SPICE_AUDIO_FMT_S16;
    start.time = reds_get_mm_time();
    spice_marshall_msg_playback_start(client->send_data.marshaller, &start);

    return snd_begin_send_message(client);
}

static int snd_playback_send_stop(PlaybackChannel *playback_channel)
{
    SndChannelClient *client = (SndChannelClient *)playback_channel;

    if (!snd_reset_send_data(client, SPICE_MSG_PLAYBACK_STOP)) {
        return FALSE;
    }

    return snd_begin_send_message(client);
}

static int snd_playback_send_ctl(PlaybackChannel *playback_channel)
{
    SndChannelClient *client = (SndChannelClient *)playback_channel;

    if ((client->client_active = client->active)) {
        return snd_playback_send_start(playback_channel);
    } else {
        return snd_playback_send_stop(playback_channel);
    }
}

static int snd_record_send_start(RecordChannel *record_channel)
{
    SndChannelClient *client = (SndChannelClient *)record_channel;
    SpiceMsgRecordStart start;

    if (!snd_reset_send_data(client, SPICE_MSG_RECORD_START)) {
        return FALSE;
    }

    start.channels = SPICE_INTERFACE_RECORD_CHAN;
    start.frequency = client->worker->frequency;
    spice_assert(SPICE_INTERFACE_RECORD_FMT == SPICE_INTERFACE_AUDIO_FMT_S16);
    start.format = SPICE_AUDIO_FMT_S16;
    spice_marshall_msg_record_start(client->send_data.marshaller, &start);

    return snd_begin_send_message(client);
}

static int snd_record_send_stop(RecordChannel *record_channel)
{
    SndChannelClient *client = (SndChannelClient *)record_channel;

    if (!snd_reset_send_data(client, SPICE_MSG_RECORD_STOP)) {
        return FALSE;
    }

    return snd_begin_send_message(client);
}

static int snd_record_send_ctl(RecordChannel *record_channel)
{
    SndChannelClient *client = (SndChannelClient *)record_channel;

    if ((client->client_active = client->active)) {
        return snd_record_send_start(record_channel);
    } else {
        return snd_record_send_stop(record_channel);
    }
}

static int snd_record_send_volume(RecordChannel *record_channel)
{
    return snd_send_volume(&record_channel->base, SPICE_RECORD_CAP_VOLUME,
                           SPICE_MSG_RECORD_VOLUME);
}

static int snd_record_send_mute(RecordChannel *record_channel)
{
    return snd_send_mute(&record_channel->base, SPICE_RECORD_CAP_VOLUME,
                         SPICE_MSG_RECORD_MUTE);
}

static int snd_record_send_migrate(RecordChannel *record_channel)
{
    /* No need for migration data: if recording has started before migration,
     * the client receives RECORD_STOP from the src before the migration completion
     * notification (when the vm is stopped).
     * Afterwards, when the vm starts on the dest, the client receives RECORD_START. */
    return snd_channel_send_migrate(&record_channel->base);
}

static int snd_playback_send_write(PlaybackChannel *playback_channel)
{
    SndChannelClient *client = (SndChannelClient *)playback_channel;
    AudioFrame *frame;
    SpiceMsgPlaybackPacket msg;

    if (!snd_reset_send_data(client, SPICE_MSG_PLAYBACK_DATA)) {
        return FALSE;
    }

    frame = playback_channel->in_progress;
    msg.time = frame->time;

    spice_marshall_msg_playback_data(client->send_data.marshaller, &msg);

    if (playback_channel->mode == SPICE_AUDIO_DATA_MODE_RAW) {
        spice_marshaller_add_ref(client->send_data.marshaller,
                                 (uint8_t *)frame->samples,
                                 snd_codec_frame_size(playback_channel->codec) * sizeof(frame->samples[0]));
    }
    else {
        int n = sizeof(playback_channel->encode_buf);
        if (snd_codec_encode(playback_channel->codec, (uint8_t *) frame->samples,
                                    snd_codec_frame_size(playback_channel->codec) * sizeof(frame->samples[0]),
                                    playback_channel->encode_buf, &n) != SND_CODEC_OK) {
            spice_printerr("encode failed");
            snd_disconnect_channel(client);
            return FALSE;
        }
        spice_marshaller_add_ref(client->send_data.marshaller, playback_channel->encode_buf, n);
    }

    return snd_begin_send_message(client);
}

static int playback_send_mode(PlaybackChannel *playback_channel)
{
    SndChannelClient *client = (SndChannelClient *)playback_channel;
    SpiceMsgPlaybackMode mode;

    if (!snd_reset_send_data(client, SPICE_MSG_PLAYBACK_MODE)) {
        return FALSE;
    }
    mode.time = reds_get_mm_time();
    mode.mode = playback_channel->mode;
    spice_marshall_msg_playback_mode(client->send_data.marshaller, &mode);

    return snd_begin_send_message(client);
}

static void snd_playback_send(void* data)
{
    PlaybackChannel *playback_channel = (PlaybackChannel*)data;
    SndChannelClient *client = (SndChannelClient*)playback_channel;

    if (!playback_channel || !snd_send_data(data)) {
        return;
    }

    while (client->command) {
        if (client->command & SND_PLAYBACK_MODE_MASK) {
            if (!playback_send_mode(playback_channel)) {
                return;
            }
            client->command &= ~SND_PLAYBACK_MODE_MASK;
        }
        if (client->command & SND_PLAYBACK_PCM_MASK) {
            spice_assert(!playback_channel->in_progress && playback_channel->pending_frame);
            playback_channel->in_progress = playback_channel->pending_frame;
            playback_channel->pending_frame = NULL;
            client->command &= ~SND_PLAYBACK_PCM_MASK;
            if (!snd_playback_send_write(playback_channel)) {
                spice_printerr("snd_send_playback_write failed");
                return;
            }
        }
        if (client->command & SND_CTRL_MASK) {
            if (!snd_playback_send_ctl(playback_channel)) {
                return;
            }
            client->command &= ~SND_CTRL_MASK;
        }
        if (client->command & SND_VOLUME_MASK) {
            if (!snd_playback_send_volume(playback_channel) ||
                !snd_playback_send_mute(playback_channel)) {
                return;
            }
            client->command &= ~SND_VOLUME_MASK;
        }
        if (client->command & SND_MIGRATE_MASK) {
            if (!snd_playback_send_migrate(playback_channel)) {
                return;
            }
            client->command &= ~SND_MIGRATE_MASK;
        }
        if (client->command & SND_PLAYBACK_LATENCY_MASK) {
            if (!snd_playback_send_latency(playback_channel)) {
                return;
            }
            client->command &= ~SND_PLAYBACK_LATENCY_MASK;
        }
    }
}

static void snd_record_send(void* data)
{
    RecordChannel *record_channel = (RecordChannel*)data;
    SndChannelClient *client = (SndChannelClient*)record_channel;

    if (!record_channel || !snd_send_data(data)) {
        return;
    }

    while (client->command) {
        if (client->command & SND_CTRL_MASK) {
            if (!snd_record_send_ctl(record_channel)) {
                return;
            }
            client->command &= ~SND_CTRL_MASK;
        }
        if (client->command & SND_VOLUME_MASK) {
            if (!snd_record_send_volume(record_channel) ||
                !snd_record_send_mute(record_channel)) {
                return;
            }
            client->command &= ~SND_VOLUME_MASK;
        }
        if (client->command & SND_MIGRATE_MASK) {
            if (!snd_record_send_migrate(record_channel)) {
                return;
            }
            client->command &= ~SND_MIGRATE_MASK;
        }
    }
}

static SndChannelClient *__new_channel(SndWorker *worker, int size, uint32_t channel_id,
                                       RedClient *red_client,
                                       RedsStream *stream,
                                       int migrate,
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
    RedsState *reds = red_channel_get_server(worker->base_channel);

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
    client->worker = worker;
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
        dummy_channel_client_create(worker->base_channel, red_client,
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

static void snd_disconnect_channel_client(RedChannelClient *rcc)
{
    SndWorker *worker;
    RedChannel *channel = red_channel_client_get_channel(rcc);
    uint32_t type;

    spice_assert(channel);
    worker = (SndWorker *)g_object_get_data(G_OBJECT(channel), "sound-worker");
    spice_assert(worker);
    g_object_get(channel, "channel-type", &type, NULL);

    spice_debug("channel-type=%d", type);
    if (worker->connection) {
        spice_assert(worker->connection->channel_client == rcc);
        snd_disconnect_channel(worker->connection);
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
    SpiceVolumeState *st = &sin->st->worker.volume;
    SndChannelClient *client = sin->st->worker.connection;
    PlaybackChannel *playback_channel = SPICE_CONTAINEROF(client, PlaybackChannel, base);

    st->volume_nchannels = nchannels;
    free(st->volume);
    st->volume = spice_memdup(volume, sizeof(uint16_t) * nchannels);

    if (!client || nchannels == 0)
        return;

    snd_playback_send_volume(playback_channel);
}

SPICE_GNUC_VISIBLE void spice_server_playback_set_mute(SpicePlaybackInstance *sin, uint8_t mute)
{
    SpiceVolumeState *st = &sin->st->worker.volume;
    SndChannelClient *client = sin->st->worker.connection;
    PlaybackChannel *playback_channel = SPICE_CONTAINEROF(client, PlaybackChannel, base);

    st->mute = mute;

    if (!client)
        return;

    snd_playback_send_mute(playback_channel);
}

static void snd_playback_start(SndWorker *worker)
{
    SndChannelClient *client = worker->connection;

    worker->active = 1;
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
    return snd_playback_start(&sin->st->worker);
}

SPICE_GNUC_VISIBLE void spice_server_playback_stop(SpicePlaybackInstance *sin)
{
    SndChannelClient *client = sin->st->worker.connection;
    PlaybackChannel *playback_channel = SPICE_CONTAINEROF(client, PlaybackChannel, base);

    sin->st->worker.active = 0;
    if (!client)
        return;
    spice_assert(playback_channel->base.active);
    reds_enable_mm_time(snd_channel_get_server(client));
    playback_channel->base.active = FALSE;
    if (playback_channel->base.client_active) {
        snd_set_command(&playback_channel->base, SND_CTRL_MASK);
        snd_playback_send(&playback_channel->base);
    } else {
        playback_channel->base.command &= ~SND_CTRL_MASK;
        playback_channel->base.command &= ~SND_PLAYBACK_PCM_MASK;

        if (playback_channel->pending_frame) {
            spice_assert(!playback_channel->in_progress);
            snd_playback_free_frame(playback_channel,
                                    playback_channel->pending_frame);
            playback_channel->pending_frame = NULL;
        }
    }
}

SPICE_GNUC_VISIBLE void spice_server_playback_get_buffer(SpicePlaybackInstance *sin,
                                                         uint32_t **frame, uint32_t *num_samples)
{
    SndChannelClient *client = sin->st->worker.connection;
    PlaybackChannel *playback_channel = SPICE_CONTAINEROF(client, PlaybackChannel, base);

    if (!client || !playback_channel->free_frames) {
        *frame = NULL;
        *num_samples = 0;
        return;
    }
    spice_assert(playback_channel->base.active);
    snd_channel_ref(client);

    *frame = playback_channel->free_frames->samples;
    playback_channel->free_frames = playback_channel->free_frames->next;
    *num_samples = snd_codec_frame_size(playback_channel->codec);
}

SPICE_GNUC_VISIBLE void spice_server_playback_put_samples(SpicePlaybackInstance *sin, uint32_t *samples)
{
    PlaybackChannel *playback_channel;
    AudioFrame *frame;

    frame = SPICE_CONTAINEROF(samples, AudioFrame, samples[0]);
    playback_channel = frame->channel;
    spice_assert(playback_channel);
    if (!snd_channel_unref(&playback_channel->base) ||
        sin->st->worker.connection != &playback_channel->base) {
        /* lost last reference, client has been destroyed previously */
        spice_info("audio samples belong to a disconnected client");
        return;
    }
    spice_assert(playback_channel->base.active);

    if (playback_channel->pending_frame) {
        snd_playback_free_frame(playback_channel, playback_channel->pending_frame);
    }
    frame->time = reds_get_mm_time();
    playback_channel->pending_frame = frame;
    snd_set_command(&playback_channel->base, SND_PLAYBACK_PCM_MASK);
    snd_playback_send(&playback_channel->base);
}

void snd_set_playback_latency(RedClient *client, uint32_t latency)
{
    SndWorker *now = workers;

    for (; now; now = now->next) {
        uint32_t type;
        g_object_get(now->base_channel, "channel-type", &type, NULL);
        if (type == SPICE_CHANNEL_PLAYBACK && now->connection &&
            red_channel_client_get_client(now->connection->channel_client) == client) {

            if (red_channel_client_test_remote_cap(now->connection->channel_client,
                SPICE_PLAYBACK_CAP_LATENCY)) {
                PlaybackChannel* playback = (PlaybackChannel*)now->connection;

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

static void on_new_playback_channel(SndWorker *worker, SndChannelClient *snd_channel)
{
    RedsState *reds = red_channel_get_server(worker->base_channel);

    spice_assert(snd_channel);

    worker->connection = snd_channel;
    snd_set_command(snd_channel, SND_PLAYBACK_MODE_MASK);
    if (snd_channel->active) {
        snd_set_command(snd_channel, SND_CTRL_MASK);
    }
    if (worker->volume.volume_nchannels) {
        snd_set_command(snd_channel, SND_VOLUME_MASK);
    }
    if (snd_channel->active) {
        reds_disable_mm_time(reds);
    }
}

static void snd_playback_cleanup(SndChannelClient *client)
{
    PlaybackChannel *playback_channel = SPICE_CONTAINEROF(client, PlaybackChannel, base);

    if (playback_channel->base.active) {
        reds_enable_mm_time(snd_channel_get_server(client));
    }

    snd_codec_destroy(&playback_channel->codec);
}

static void snd_set_playback_peer(RedChannel *channel, RedClient *client, RedsStream *stream,
                                  int migration, int num_common_caps, uint32_t *common_caps,
                                  int num_caps, uint32_t *caps)
{
    SndWorker *worker = g_object_get_data(G_OBJECT(channel), "sound-worker");
    PlaybackChannel *playback_channel;

    snd_disconnect_channel(worker->connection);

    if (!(playback_channel = (PlaybackChannel *)__new_channel(worker,
                                                              sizeof(*playback_channel),
                                                              SPICE_CHANNEL_PLAYBACK,
                                                              client,
                                                              stream,
                                                              migration,
                                                              snd_playback_send,
                                                              snd_playback_handle_message,
                                                              snd_playback_on_message_done,
                                                              snd_playback_cleanup,
                                                              common_caps, num_common_caps,
                                                              caps, num_caps))) {
        return;
    }
    snd_playback_free_frame(playback_channel, &playback_channel->frames[0]);
    snd_playback_free_frame(playback_channel, &playback_channel->frames[1]);
    snd_playback_free_frame(playback_channel, &playback_channel->frames[2]);

    int client_can_celt = red_channel_client_test_remote_cap(playback_channel->base.channel_client,
                                          SPICE_PLAYBACK_CAP_CELT_0_5_1);
    int client_can_opus = red_channel_client_test_remote_cap(playback_channel->base.channel_client,
                                          SPICE_PLAYBACK_CAP_OPUS);
    int playback_compression =
        reds_config_get_playback_compression(red_channel_get_server(channel));
    int desired_mode = snd_desired_audio_mode(playback_compression, worker->frequency,
                                              client_can_celt, client_can_opus);
    playback_channel->mode = SPICE_AUDIO_DATA_MODE_RAW;
    if (desired_mode != SPICE_AUDIO_DATA_MODE_RAW) {
        if (snd_codec_create(&playback_channel->codec, desired_mode, worker->frequency,
                             SND_CODEC_ENCODE) == SND_CODEC_OK) {
            playback_channel->mode = desired_mode;
        } else {
            spice_printerr("create encoder failed");
        }
    }

    if (!red_client_during_migrate_at_target(client)) {
        on_new_playback_channel(worker, &playback_channel->base);
    }

    if (worker->active) {
        snd_playback_start(worker);
    }
    snd_playback_send(worker->connection);
}

static void snd_record_migrate_channel_client(RedChannelClient *rcc)
{
    SndWorker *worker;
    RedChannel *channel = red_channel_client_get_channel(rcc);

    spice_debug(NULL);
    spice_assert(channel);
    worker = (SndWorker *)g_object_get_data(G_OBJECT(channel), "sound-worker");
    spice_assert(worker);

    if (worker->connection) {
        spice_assert(worker->connection->channel_client == rcc);
        snd_set_command(worker->connection, SND_MIGRATE_MASK);
        snd_record_send(worker->connection);
    }
}

SPICE_GNUC_VISIBLE void spice_server_record_set_volume(SpiceRecordInstance *sin,
                                                uint8_t nchannels,
                                                uint16_t *volume)
{
    SpiceVolumeState *st = &sin->st->worker.volume;
    SndChannelClient *client = sin->st->worker.connection;
    RecordChannel *record_channel = SPICE_CONTAINEROF(client, RecordChannel, base);

    st->volume_nchannels = nchannels;
    free(st->volume);
    st->volume = spice_memdup(volume, sizeof(uint16_t) * nchannels);

    if (!client || nchannels == 0)
        return;

    snd_record_send_volume(record_channel);
}

SPICE_GNUC_VISIBLE void spice_server_record_set_mute(SpiceRecordInstance *sin, uint8_t mute)
{
    SpiceVolumeState *st = &sin->st->worker.volume;
    SndChannelClient *client = sin->st->worker.connection;
    RecordChannel *record_channel = SPICE_CONTAINEROF(client, RecordChannel, base);

    st->mute = mute;

    if (!client)
        return;

    snd_record_send_mute(record_channel);
}

static void snd_record_start(SndWorker *worker)
{
    SndChannelClient *client = worker->connection;
    RecordChannel *record_channel = SPICE_CONTAINEROF(client, RecordChannel, base);

    worker->active = 1;
    if (!client)
        return;
    spice_assert(!client->active);
    record_channel->read_pos = record_channel->write_pos = 0;   //todo: improve by
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
    snd_record_start(&sin->st->worker);
}

SPICE_GNUC_VISIBLE void spice_server_record_stop(SpiceRecordInstance *sin)
{
    SndChannelClient *client = sin->st->worker.connection;
    RecordChannel *record_channel = SPICE_CONTAINEROF(client, RecordChannel, base);

    sin->st->worker.active = 0;
    if (!client)
        return;
    spice_assert(record_channel->base.active);
    record_channel->base.active = FALSE;
    if (record_channel->base.client_active) {
        snd_set_command(&record_channel->base, SND_CTRL_MASK);
        snd_record_send(&record_channel->base);
    } else {
        record_channel->base.command &= ~SND_CTRL_MASK;
    }
}

SPICE_GNUC_VISIBLE uint32_t spice_server_record_get_samples(SpiceRecordInstance *sin,
                                                            uint32_t *samples, uint32_t bufsize)
{
    SndChannelClient *client = sin->st->worker.connection;
    RecordChannel *record_channel = SPICE_CONTAINEROF(client, RecordChannel, base);
    uint32_t read_pos;
    uint32_t now;
    uint32_t len;

    if (!client)
        return 0;
    spice_assert(record_channel->base.active);

    if (record_channel->write_pos < RECORD_SAMPLES_SIZE / 2) {
        return 0;
    }

    len = MIN(record_channel->write_pos - record_channel->read_pos, bufsize);

    if (len < bufsize) {
        SndWorker *worker = record_channel->base.worker;
        snd_receive(&record_channel->base);
        if (!worker->connection) {
            return 0;
        }
        len = MIN(record_channel->write_pos - record_channel->read_pos, bufsize);
    }

    read_pos = record_channel->read_pos % RECORD_SAMPLES_SIZE;
    record_channel->read_pos += len;
    now = MIN(len, RECORD_SAMPLES_SIZE - read_pos);
    memcpy(samples, &record_channel->samples[read_pos], now * 4);
    if (now < len) {
        memcpy(samples + now, record_channel->samples, (len - now) * 4);
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

static void snd_set_rate(SndWorker *worker, uint32_t frequency, uint32_t cap_opus)
{
    RedChannel *channel = worker->base_channel;
    worker->frequency = frequency;
    if (channel && snd_codec_is_capable(SPICE_AUDIO_DATA_MODE_OPUS, frequency)) {
        red_channel_set_cap(channel, cap_opus);
    }
}

SPICE_GNUC_VISIBLE uint32_t spice_server_get_best_playback_rate(SpicePlaybackInstance *sin)
{
    return snd_get_best_rate(sin ? sin->st->worker.connection : NULL, SPICE_PLAYBACK_CAP_OPUS);
}

SPICE_GNUC_VISIBLE void spice_server_set_playback_rate(SpicePlaybackInstance *sin, uint32_t frequency)
{
    snd_set_rate(&sin->st->worker, frequency, SPICE_PLAYBACK_CAP_OPUS);
}

SPICE_GNUC_VISIBLE uint32_t spice_server_get_best_record_rate(SpiceRecordInstance *sin)
{
    return snd_get_best_rate(sin ? sin->st->worker.connection : NULL, SPICE_RECORD_CAP_OPUS);
}

SPICE_GNUC_VISIBLE void spice_server_set_record_rate(SpiceRecordInstance *sin, uint32_t frequency)
{
    snd_set_rate(&sin->st->worker, frequency, SPICE_RECORD_CAP_OPUS);
}

static void on_new_record_channel(SndWorker *worker, SndChannelClient *snd_channel)
{
    spice_assert(snd_channel);

    worker->connection = snd_channel ;
    if (worker->volume.volume_nchannels) {
        snd_set_command(snd_channel, SND_VOLUME_MASK);
    }
    if (snd_channel->active) {
        snd_set_command(snd_channel, SND_CTRL_MASK);
    }
}

static void snd_record_cleanup(SndChannelClient *client)
{
    RecordChannel *record_channel = SPICE_CONTAINEROF(client, RecordChannel, base);
    snd_codec_destroy(&record_channel->codec);
}

static void snd_set_record_peer(RedChannel *channel, RedClient *client, RedsStream *stream,
                                int migration, int num_common_caps, uint32_t *common_caps,
                                int num_caps, uint32_t *caps)
{
    SndWorker *worker = g_object_get_data(G_OBJECT(channel), "sound-worker");
    RecordChannel *record_channel;

    snd_disconnect_channel(worker->connection);

    if (!(record_channel = (RecordChannel *)__new_channel(worker,
                                                          sizeof(*record_channel),
                                                          SPICE_CHANNEL_RECORD,
                                                          client,
                                                          stream,
                                                          migration,
                                                          snd_record_send,
                                                          snd_record_handle_message,
                                                          snd_record_on_message_done,
                                                          snd_record_cleanup,
                                                          common_caps, num_common_caps,
                                                          caps, num_caps))) {
        return;
    }

    record_channel->mode = SPICE_AUDIO_DATA_MODE_RAW;

    on_new_record_channel(worker, &record_channel->base);
    if (worker->active) {
        snd_record_start(worker);
    }
    snd_record_send(worker->connection);
}

static void snd_playback_migrate_channel_client(RedChannelClient *rcc)
{
    SndWorker *worker;
    RedChannel *channel = red_channel_client_get_channel(rcc);

    spice_assert(channel);
    worker = (SndWorker *)g_object_get_data(G_OBJECT(channel), "sound-worker");
    spice_assert(worker);
    spice_debug(NULL);

    if (worker->connection) {
        spice_assert(worker->connection->channel_client == rcc);
        snd_set_command(worker->connection, SND_MIGRATE_MASK);
        snd_playback_send(worker->connection);
    }
}

static void add_worker(SndWorker *worker)
{
    worker->next = workers;
    workers = worker;
}

static void remove_worker(SndWorker *worker)
{
    SndWorker **now = &workers;
    while (*now) {
        if (*now == worker) {
            *now = worker->next;
            return;
        }
        now = &(*now)->next;
    }
    spice_printerr("not found");
}

void snd_attach_playback(RedsState *reds, SpicePlaybackInstance *sin)
{
    SndWorker *playback_worker;
    RedChannel *channel;
    ClientCbs client_cbs = { NULL, };

    sin->st = spice_new0(SpicePlaybackState, 1);
    playback_worker = &sin->st->worker;
    playback_worker->frequency = SND_CODEC_CELT_PLAYBACK_FREQ; /* Default to the legacy rate */

    // TODO: Make RedChannel base of worker? instead of assigning it to channel->data
    channel = dummy_channel_new(reds, SPICE_CHANNEL_PLAYBACK, 0);

    g_object_set_data(G_OBJECT(channel), "sound-worker", playback_worker);
    client_cbs.connect = snd_set_playback_peer;
    client_cbs.disconnect = snd_disconnect_channel_client;
    client_cbs.migrate = snd_playback_migrate_channel_client;
    red_channel_register_client_cbs(channel, &client_cbs, playback_worker);

    if (snd_codec_is_capable(SPICE_AUDIO_DATA_MODE_CELT_0_5_1, SND_CODEC_ANY_FREQUENCY))
        red_channel_set_cap(channel, SPICE_PLAYBACK_CAP_CELT_0_5_1);

    red_channel_set_cap(channel, SPICE_PLAYBACK_CAP_VOLUME);

    playback_worker->base_channel = channel;
    add_worker(playback_worker);
    reds_register_channel(reds, channel);
}

void snd_attach_record(RedsState *reds, SpiceRecordInstance *sin)
{
    SndWorker *record_worker;
    RedChannel *channel;
    ClientCbs client_cbs = { NULL, };

    sin->st = spice_new0(SpiceRecordState, 1);
    record_worker = &sin->st->worker;
    record_worker->frequency = SND_CODEC_CELT_PLAYBACK_FREQ; /* Default to the legacy rate */

    // TODO: Make RedChannel base of worker? instead of assigning it to channel->data
    channel = dummy_channel_new(reds, SPICE_CHANNEL_RECORD, 0);

    g_object_set_data(G_OBJECT(channel), "sound-worker", record_worker);
    client_cbs.connect = snd_set_record_peer;
    client_cbs.disconnect = snd_disconnect_channel_client;
    client_cbs.migrate = snd_record_migrate_channel_client;
    red_channel_register_client_cbs(channel, &client_cbs, record_worker);
    if (snd_codec_is_capable(SPICE_AUDIO_DATA_MODE_CELT_0_5_1, SND_CODEC_ANY_FREQUENCY))
        red_channel_set_cap(channel, SPICE_RECORD_CAP_CELT_0_5_1);
    red_channel_set_cap(channel, SPICE_RECORD_CAP_VOLUME);

    record_worker->base_channel = channel;
    add_worker(record_worker);
    reds_register_channel(reds, channel);
}

static void snd_detach_common(SndWorker *worker)
{
    if (!worker) {
        return;
    }
    RedsState *reds = red_channel_get_server(worker->base_channel);

    remove_worker(worker);
    snd_disconnect_channel(worker->connection);
    reds_unregister_channel(reds, worker->base_channel);
    red_channel_destroy(worker->base_channel);
    free(worker->volume.volume);
    worker->volume.volume = NULL;
}

static void spice_playback_state_free(SpicePlaybackState *st)
{
    free(st);
}

void snd_detach_playback(SpicePlaybackInstance *sin)
{
    snd_detach_common(&sin->st->worker);
    spice_playback_state_free(sin->st);
}

static void spice_record_state_free(SpiceRecordState *st)
{
    free(st);
}

void snd_detach_record(SpiceRecordInstance *sin)
{
    snd_detach_common(&sin->st->worker);
    spice_record_state_free(sin->st);
}

void snd_set_playback_compression(int on)
{
    SndWorker *now = workers;

    for (; now; now = now->next) {
        uint32_t type;
        g_object_get(now->base_channel, "channel-type", &type, NULL);
        if (type == SPICE_CHANNEL_PLAYBACK && now->connection) {
            PlaybackChannel* playback = (PlaybackChannel*)now->connection;
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
