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

#ifndef RED_WORKER_H_
#define RED_WORKER_H_

#include "red-common.h"
#include "red-qxl.h"
#include "red-parse-qxl.h"
#include "red-channel.h"

typedef struct RedWorker RedWorker;

RedWorker* red_worker_new(QXLInstance *qxl,
                          const ClientCbs *client_cursor_cbs,
                          const ClientCbs *client_display_cbs);
bool       red_worker_run(RedWorker *worker);
void red_worker_free(RedWorker *worker);

#endif /* RED_WORKER_H_ */
