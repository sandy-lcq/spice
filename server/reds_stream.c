/* -*- Mode: C; c-basic-offset: 4; indent-tabs-mode: nil -*- */
/*
   Copyright (C) 2009, 2013 Red Hat, Inc.

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

#include "main_dispatcher.h"
#include "red_common.h"
#include "reds_stream.h"
#include "common/log.h"

#include <errno.h>
#include <netdb.h>
#include <unistd.h>
#include <sys/socket.h>

#include <openssl/err.h>

extern SpiceCoreInterface *core;

static ssize_t stream_write_cb(RedsStream *s, const void *buf, size_t size)
{
    return write(s->socket, buf, size);
}

static ssize_t stream_writev_cb(RedsStream *s, const struct iovec *iov, int iovcnt)
{
    ssize_t ret = 0;
    do {
        int tosend;
        ssize_t n, expected = 0;
        int i;
#ifdef IOV_MAX
        tosend = MIN(iovcnt, IOV_MAX);
#else
        tosend = iovcnt;
#endif
        for (i = 0; i < tosend; i++) {
            expected += iov[i].iov_len;
        }
        n = writev(s->socket, iov, tosend);
        if (n <= expected) {
            if (n > 0)
                ret += n;
            return ret == 0 ? n : ret;
        }
        ret += n;
        iov += tosend;
        iovcnt -= tosend;
    } while(iovcnt > 0);

    return ret;
}

static ssize_t stream_read_cb(RedsStream *s, void *buf, size_t size)
{
    return read(s->socket, buf, size);
}

static ssize_t stream_ssl_write_cb(RedsStream *s, const void *buf, size_t size)
{
    int return_code;
    SPICE_GNUC_UNUSED int ssl_error;

    return_code = SSL_write(s->ssl, buf, size);

    if (return_code < 0) {
        ssl_error = SSL_get_error(s->ssl, return_code);
    }

    return return_code;
}

static ssize_t stream_ssl_read_cb(RedsStream *s, void *buf, size_t size)
{
    int return_code;
    SPICE_GNUC_UNUSED int ssl_error;

    return_code = SSL_read(s->ssl, buf, size);

    if (return_code < 0) {
        ssl_error = SSL_get_error(s->ssl, return_code);
    }

    return return_code;
}

void reds_stream_remove_watch(RedsStream* s)
{
    if (s->watch) {
        core->watch_remove(s->watch);
        s->watch = NULL;
    }
}

static ssize_t reds_stream_sasl_read(RedsStream *s, uint8_t *buf, size_t nbyte);

ssize_t reds_stream_read(RedsStream *s, void *buf, size_t nbyte)
{
    ssize_t ret;

#if HAVE_SASL
    if (s->sasl.conn && s->sasl.runSSF) {
        ret = reds_stream_sasl_read(s, buf, nbyte);
    } else
#endif
        ret = s->read(s, buf, nbyte);

    return ret;
}

bool reds_stream_write_all(RedsStream *stream, const void *in_buf, size_t n)
{
    const uint8_t *buf = (uint8_t *)in_buf;

    while (n) {
        int now = reds_stream_write(stream, buf, n);
        if (now <= 0) {
            if (now == -1 && (errno == EINTR || errno == EAGAIN)) {
                continue;
            }
            return FALSE;
        }
        n -= now;
        buf += now;
    }
    return TRUE;
}

static ssize_t reds_stream_sasl_write(RedsStream *s, const void *buf, size_t nbyte);

ssize_t reds_stream_write(RedsStream *s, const void *buf, size_t nbyte)
{
    ssize_t ret;

#if HAVE_SASL
    if (s->sasl.conn && s->sasl.runSSF) {
        ret = reds_stream_sasl_write(s, buf, nbyte);
    } else
#endif
        ret = s->write(s, buf, nbyte);

    return ret;
}

ssize_t reds_stream_writev(RedsStream *s, const struct iovec *iov, int iovcnt)
{
    int i;
    int n;
    ssize_t ret = 0;

    if (s->writev != NULL) {
        return s->writev(s, iov, iovcnt);
    }

    for (i = 0; i < iovcnt; ++i) {
        n = reds_stream_write(s, iov[i].iov_base, iov[i].iov_len);
        if (n <= 0)
            return ret == 0 ? n : ret;
        ret += n;
    }

    return ret;
}

void reds_stream_free(RedsStream *s)
{
    if (!s) {
        return;
    }

    reds_stream_push_channel_event(s, SPICE_CHANNEL_EVENT_DISCONNECTED);

#if HAVE_SASL
    if (s->sasl.conn) {
        s->sasl.runSSF = s->sasl.wantSSF = 0;
        s->sasl.len = 0;
        s->sasl.encodedLength = s->sasl.encodedOffset = 0;
        s->sasl.encoded = NULL;
        free(s->sasl.mechlist);
        free(s->sasl.mechname);
        s->sasl.mechlist = NULL;
        sasl_dispose(&s->sasl.conn);
        s->sasl.conn = NULL;
    }
#endif

    if (s->ssl) {
        SSL_free(s->ssl);
    }

    reds_stream_remove_watch(s);
    spice_info("close socket fd %d", s->socket);
    close(s->socket);

    free(s);
}

void reds_stream_push_channel_event(RedsStream *s, int event)
{
    main_dispatcher_channel_event(event, s->info);
}

static void reds_stream_set_socket(RedsStream *stream, int socket)
{
    stream->socket = socket;
    /* deprecated fields. Filling them for backward compatibility */
    stream->info->llen = sizeof(stream->info->laddr);
    stream->info->plen = sizeof(stream->info->paddr);
    getsockname(stream->socket, (struct sockaddr*)(&stream->info->laddr), &stream->info->llen);
    getpeername(stream->socket, (struct sockaddr*)(&stream->info->paddr), &stream->info->plen);

    stream->info->flags |= SPICE_CHANNEL_EVENT_FLAG_ADDR_EXT;
    stream->info->llen_ext = sizeof(stream->info->laddr_ext);
    stream->info->plen_ext = sizeof(stream->info->paddr_ext);
    getsockname(stream->socket, (struct sockaddr*)(&stream->info->laddr_ext),
                &stream->info->llen_ext);
    getpeername(stream->socket, (struct sockaddr*)(&stream->info->paddr_ext),
                &stream->info->plen_ext);
}

RedsStream *reds_stream_new(int socket)
{
    RedsStream *stream;

    stream = spice_new0(RedsStream, 1);
    stream->info = spice_new0(SpiceChannelEventInfo, 1);
    reds_stream_set_socket(stream, socket);

    stream->read = stream_read_cb;
    stream->write = stream_write_cb;
    stream->writev = stream_writev_cb;

    stream->async_read.stream = stream;

    return stream;
}

RedsStreamSslStatus reds_stream_ssl_accept(RedsStream *stream)
{
    int ssl_error;
    int return_code;

    return_code = SSL_accept(stream->ssl);
    if (return_code == 1) {
        return REDS_STREAM_SSL_STATUS_OK;
    }

    ssl_error = SSL_get_error(stream->ssl, return_code);
    if (return_code == -1 && (ssl_error == SSL_ERROR_WANT_READ ||
                              ssl_error == SSL_ERROR_WANT_WRITE)) {
        if (ssl_error == SSL_ERROR_WANT_READ) {
            return REDS_STREAM_SSL_STATUS_WAIT_FOR_READ;
        } else {
            return REDS_STREAM_SSL_STATUS_WAIT_FOR_WRITE;
        }
    }

    ERR_print_errors_fp(stderr);
    spice_warning("SSL_accept failed, error=%d", ssl_error);
    SSL_free(stream->ssl);
    stream->ssl = NULL;

    return REDS_STREAM_SSL_STATUS_ERROR;
}

int reds_stream_enable_ssl(RedsStream *stream, SSL_CTX *ctx)
{
    BIO *sbio;

    // Handle SSL handshaking
    if (!(sbio = BIO_new_socket(stream->socket, BIO_NOCLOSE))) {
        spice_warning("could not allocate ssl bio socket");
        return REDS_STREAM_SSL_STATUS_ERROR;
    }

    stream->ssl = SSL_new(ctx);
    if (!stream->ssl) {
        spice_warning("could not allocate ssl context");
        BIO_free(sbio);
        return REDS_STREAM_SSL_STATUS_ERROR;
    }

    SSL_set_bio(stream->ssl, sbio, sbio);

    stream->write = stream_ssl_write_cb;
    stream->read = stream_ssl_read_cb;
    stream->writev = NULL;

    return reds_stream_ssl_accept(stream);
}

void async_read_set_error_handler(AsyncRead *async,
                                  AsyncReadError error_handler,
                                 void *opaque)
{
    async->error = error_handler;
}

static inline void async_read_clear_handlers(AsyncRead *obj)
{
    if (!obj->stream->watch) {
        return;
    }

    reds_stream_remove_watch(obj->stream);
}

void async_read_handler(int fd, int event, void *data)
{
    AsyncRead *obj = (AsyncRead *)data;

    for (;;) {
        int n = obj->end - obj->now;

        spice_assert(n > 0);
        n = reds_stream_read(obj->stream, obj->now, n);
        if (n <= 0) {
            if (n < 0) {
                switch (errno) {
                case EAGAIN:
                    if (!obj->stream->watch) {
                        obj->stream->watch = core->watch_add(obj->stream->socket,
                                                           SPICE_WATCH_EVENT_READ,
                                                           async_read_handler, obj);
                    }
                    return;
                case EINTR:
                    break;
                default:
                    async_read_clear_handlers(obj);
		    if (obj->error) {
                        obj->error(obj->opaque, errno);
		    }
                    return;
                }
            } else {
                async_read_clear_handlers(obj);
		if (obj->error) {
		    obj->error(obj->opaque, 0);
		}
                return;
            }
        } else {
            obj->now += n;
            if (obj->now == obj->end) {
                async_read_clear_handlers(obj);
                obj->done(obj->opaque);
                return;
            }
        }
    }
}

#if HAVE_SASL
bool reds_stream_write_u8(RedsStream *s, uint8_t n)
{
    return reds_stream_write_all(s, &n, sizeof(uint8_t));
}

bool reds_stream_write_u32(RedsStream *s, uint32_t n)
{
    return reds_stream_write_all(s, &n, sizeof(uint32_t));
}

static ssize_t reds_stream_sasl_write(RedsStream *s, const void *buf, size_t nbyte)
{
    ssize_t ret;

    if (!s->sasl.encoded) {
        int err;
        err = sasl_encode(s->sasl.conn, (char *)buf, nbyte,
                          (const char **)&s->sasl.encoded,
                          &s->sasl.encodedLength);
        if (err != SASL_OK) {
            spice_warning("sasl_encode error: %d", err);
            return -1;
        }

        if (s->sasl.encodedLength == 0) {
            return 0;
        }

        if (!s->sasl.encoded) {
            spice_warning("sasl_encode didn't return a buffer!");
            return 0;
        }

        s->sasl.encodedOffset = 0;
    }

    ret = s->write(s, s->sasl.encoded + s->sasl.encodedOffset,
                   s->sasl.encodedLength - s->sasl.encodedOffset);

    if (ret <= 0) {
        return ret;
    }

    s->sasl.encodedOffset += ret;
    if (s->sasl.encodedOffset == s->sasl.encodedLength) {
        s->sasl.encoded = NULL;
        s->sasl.encodedOffset = s->sasl.encodedLength = 0;
        return nbyte;
    }

    /* we didn't flush the encoded buffer */
    errno = EAGAIN;
    return -1;
}

static ssize_t reds_stream_sasl_read(RedsStream *s, uint8_t *buf, size_t nbyte)
{
    uint8_t encoded[4096];
    const char *decoded;
    unsigned int decodedlen;
    int err;
    int n;

    n = spice_buffer_copy(&s->sasl.inbuffer, buf, nbyte);
    if (n > 0) {
        spice_buffer_remove(&s->sasl.inbuffer, n);
        if (n == nbyte)
            return n;
        nbyte -= n;
        buf += n;
    }

    n = s->read(s, encoded, sizeof(encoded));
    if (n <= 0) {
        return n;
    }

    err = sasl_decode(s->sasl.conn,
                      (char *)encoded, n,
                      &decoded, &decodedlen);
    if (err != SASL_OK) {
        spice_warning("sasl_decode error: %d", err);
        return -1;
    }

    if (decodedlen == 0) {
        errno = EAGAIN;
        return -1;
    }

    n = MIN(nbyte, decodedlen);
    memcpy(buf, decoded, n);
    spice_buffer_append(&s->sasl.inbuffer, decoded + n, decodedlen - n);
    return n;
}
#endif
