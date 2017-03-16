/* -*- Mode: C; c-basic-offset: 4; indent-tabs-mode: nil -*- */
/*
   Copyright (C) 2009, 2017 Red Hat, Inc.

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

#include <errno.h>
#include <fcntl.h>
#include <stdbool.h>
#include <string.h>
#include <netinet/ip.h>
#include <netinet/tcp.h>
#include <sys/socket.h>

#include <common/log.h>

#include "net-utils.h"

/**
 * red_socket_set_no_delay:
 * @fd: a socket file descriptor
 * @no_delay: whether to enable TCP_NODELAY on @fd
 *
 * Returns: #true if the operation succeeded, #false otherwise.
 */
bool red_socket_set_no_delay(int fd, bool no_delay)
{
    int optval = no_delay;

    if (setsockopt(fd, IPPROTO_TCP, TCP_NODELAY,
                   &optval, sizeof(optval)) != 0) {
        if (errno != ENOTSUP && errno != ENOPROTOOPT) {
            spice_warning("setsockopt failed, %s", strerror(errno));
            return false;
        }
    }

    return true;
}

/**
 * red_socket_set_non_blocking:
 * @fd: a socket file descriptor
 * @non_blocking: whether to enable O_NONBLOCK on @fd
 *
 * Returns: #true if the operation succeeded, #false otherwise.
 */
bool red_socket_set_non_blocking(int fd, bool non_blocking)
{
    int flags;

    if ((flags = fcntl(fd, F_GETFL)) == -1) {
        spice_warning("fnctl(F_GETFL) failed, %s", strerror(errno));
        return false;
    }

    if (non_blocking) {
        flags |= O_NONBLOCK;
    } else {
        flags &= ~O_NONBLOCK;
    }

    if (fcntl(fd, F_SETFL, flags) == -1) {
        spice_warning("fnctl(F_SETFL) failed, %s", strerror(errno));
        return false;
    }

    return true;
}
