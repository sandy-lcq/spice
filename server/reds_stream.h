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

#ifndef _H_REDS_STREAM
#define _H_REDS_STREAM

#include "spice.h"
#include "common/mem.h"

#include <stdbool.h>

#include <openssl/ssl.h>

#if HAVE_SASL
#include <sasl/sasl.h>

typedef void (*AsyncReadDone)(void *opaque);
typedef void (*AsyncReadError)(void *opaque, int err);

typedef struct RedsSASL {
    sasl_conn_t *conn;

    /* If we want to negotiate an SSF layer with client */
    int wantSSF :1;
    /* If we are now running the SSF layer */
    int runSSF :1;

    /*
     * Buffering encoded data to allow more clear data
     * to be stuffed onto the output buffer
     */
    const uint8_t *encoded;
    unsigned int encodedLength;
    unsigned int encodedOffset;

    SpiceBuffer inbuffer;

    char *username;
    char *mechlist;
    char *mechname;

    /* temporary data during authentication */
    unsigned int len;
    char *data;
} RedsSASL;
#endif

typedef struct RedsStream RedsStream;
typedef struct AsyncRead {
    RedsStream *stream;
    void *opaque;
    uint8_t *now;
    uint8_t *end;
    AsyncReadDone done;
    AsyncReadError error;
} AsyncRead;

void async_read_handler(int fd, int event, void *data);
void async_read_set_error_handler(AsyncRead *async,
                                  AsyncReadError error_handler,
                                  void *opaque);

struct RedsStream {
    int socket;
    SpiceWatch *watch;

    /* set it to TRUE if you shutdown the socket. shutdown read doesn't work as accepted -
       receive may return data afterward. check the flag before calling receive*/
    int shutdown;
    SSL *ssl;

    AsyncRead async_read;

#if HAVE_SASL
    RedsSASL sasl;
#endif

    /* life time of info:
     * allocated when creating RedsStream.
     * deallocated when main_dispatcher handles the SPICE_CHANNEL_EVENT_DISCONNECTED
     * event, either from same thread or by call back from main thread. */
    SpiceChannelEventInfo* info;

    /* private */
    ssize_t (*read)(RedsStream *s, void *buf, size_t nbyte);
    ssize_t (*write)(RedsStream *s, const void *buf, size_t nbyte);
    ssize_t (*writev)(RedsStream *s, const struct iovec *iov, int iovcnt);
};

typedef enum {
    REDS_STREAM_SSL_STATUS_OK,
    REDS_STREAM_SSL_STATUS_ERROR,
    REDS_STREAM_SSL_STATUS_WAIT_FOR_READ,
    REDS_STREAM_SSL_STATUS_WAIT_FOR_WRITE
} RedsStreamSslStatus;

/* any thread */
ssize_t reds_stream_read(RedsStream *s, void *buf, size_t nbyte);
ssize_t reds_stream_write(RedsStream *s, const void *buf, size_t nbyte);
ssize_t reds_stream_writev(RedsStream *s, const struct iovec *iov, int iovcnt);
bool reds_stream_write_all(RedsStream *stream, const void *in_buf, size_t n);
bool reds_stream_write_u8(RedsStream *s, uint8_t n);
bool reds_stream_write_u32(RedsStream *s, uint32_t n);
void reds_stream_free(RedsStream *s);

void reds_stream_push_channel_event(RedsStream *s, int event);
void reds_stream_remove_watch(RedsStream* s);
RedsStream *reds_stream_new(int socket);
RedsStreamSslStatus reds_stream_ssl_accept(RedsStream *stream);
int reds_stream_enable_ssl(RedsStream *stream, SSL_CTX *ctx);

#endif
