/* -*- Mode: C; c-basic-offset: 4; indent-tabs-mode: nil -*- */
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
#ifndef REDS_PRIVATE_H
#define REDS_PRIVATE_H

#include <spice/protocol.h>

#define MIGRATE_TIMEOUT (MSEC_PER_SEC * 10)
#define MM_TIME_DELTA 400 /*ms*/

typedef struct TicketAuthentication {
    char password[SPICE_MAX_PASSWORD_LENGTH];
    time_t expiration_time;
} TicketAuthentication;

typedef struct TicketInfo {
    RSA *rsa;
    int rsa_size;
    BIGNUM *bn;
    SpiceLinkEncryptedTicket encrypted_ticket;
} TicketInfo;

typedef struct MonitorMode {
    uint32_t x_res;
    uint32_t y_res;
} MonitorMode;

typedef struct VDIPortState VDIPortState;

typedef struct VDIReadBuf {
    VDIPortState *state;
    RingItem link;
    uint32_t refs;

    int len;
    uint8_t data[SPICE_AGENT_MAX_DATA_SIZE];
} VDIReadBuf;

enum {
    VDI_PORT_READ_STATE_READ_HEADER,
    VDI_PORT_READ_STATE_GET_BUFF,
    VDI_PORT_READ_STATE_READ_DATA,
};

struct VDIPortState {
    SpiceCharDeviceState *base;
    uint32_t plug_generation;
    int client_agent_started;

    /* write to agent */
    SpiceCharDeviceWriteBuffer *recv_from_client_buf;
    int recv_from_client_buf_pushed;
    AgentMsgFilter write_filter;

    /* read from agent */
    Ring read_bufs;
    uint32_t read_state;
    uint32_t message_receive_len;
    uint8_t *receive_pos;
    uint32_t receive_len;
    VDIReadBuf *current_read_buf;
    AgentMsgFilter read_filter;

    VDIChunkHeader vdi_chunk_header;

    SpiceMigrateDataMain *mig_data; /* storing it when migration data arrives
                                       before agent is attached */
};

/* messages that are addressed to the agent and are created in the server */
typedef struct __attribute__ ((__packed__)) VDInternalBuf {
    VDIChunkHeader chunk_header;
    VDAgentMessage header;
    union {
        VDAgentMouseState mouse_state;
    }
    u;
} VDInternalBuf;

#ifdef RED_STATISTICS

#define REDS_MAX_STAT_NODES 100
#define REDS_STAT_SHM_SIZE (sizeof(SpiceStat) + REDS_MAX_STAT_NODES * sizeof(SpiceStatNode))

typedef struct RedsStatValue {
    uint32_t value;
    uint32_t min;
    uint32_t max;
    uint32_t average;
    uint32_t count;
} RedsStatValue;

#endif

typedef struct RedsMigPendingLink {
    RingItem ring_link; // list of links that belongs to the same client
    SpiceLinkMess *link_msg;
    RedsStream *stream;
} RedsMigPendingLink;

typedef struct RedsMigTargetClient {
    RedsState *reds;
    RingItem link;
    RedClient *client;
    Ring pending_links;
} RedsMigTargetClient;

typedef struct RedsMigWaitDisconnectClient {
    RingItem link;
    RedClient *client;
} RedsMigWaitDisconnectClient;

typedef struct SpiceCharDeviceStateItem {
    RingItem link;
    SpiceCharDeviceState *st;
} SpiceCharDeviceStateItem;

/* Intermediate state for on going monitors config message from a single
 * client, being passed to the guest */
typedef struct RedsClientMonitorsConfig {
    MainChannelClient *mcc;
    uint8_t *buffer;
    int buffer_size;
    int buffer_pos;
} RedsClientMonitorsConfig;

typedef struct ChannelSecurityOptions ChannelSecurityOptions;

typedef struct RedSSLParameters {
    char keyfile_password[256];
    char certs_file[256];
    char private_key_file[256];
    char ca_certificate_file[256];
    char dh_key_file[256];
    char ciphersuite[256];
} RedSSLParameters;

struct RedsState {
    int listen_socket;
    int secure_listen_socket;
    SpiceWatch *listen_watch;
    SpiceWatch *secure_listen_watch;
    VDIPortState agent_state;
    int pending_mouse_event;
    Ring clients;
    int num_clients;
    MainChannel *main_channel;
    InputsChannel *inputs_channel;

    int mig_wait_connect; /* src waits for clients to establish connection to dest
                             (before migration starts) */
    int mig_wait_disconnect; /* src waits for clients to disconnect (after migration completes) */
    Ring mig_wait_disconnect_clients; /* List of RedsMigWaitDisconnectClient. Holds the clients
                                         which the src waits for their disconnection */

    int mig_inprogress;
    int expect_migrate;
    int src_do_seamless_migrate; /* per migration. Updated after the migration handshake
                                    between the 2 servers */
    int dst_do_seamless_migrate; /* per migration. Updated after the migration handshake
                                    between the 2 servers */
    Ring mig_target_clients;
    int num_mig_target_clients;
    RedsMigSpice *mig_spice;

    int num_of_channels;
    Ring channels;
    int mouse_mode;
    int is_client_mouse_allowed;
    int dispatcher_allows_client_mouse;
    MonitorMode monitor_mode;
    SpiceTimer *mig_timer;

    int vm_running;
    Ring char_devs_states; /* list of SpiceCharDeviceStateItem */
    int seamless_migration_enabled; /* command line arg */
    int keepalive_timeout;

    SSL_CTX *ctx;

#ifdef RED_STATISTICS
    char *stat_shm_name;
    SpiceStat *stat;
    pthread_mutex_t stat_lock;
    RedsStatValue roundtrip_stat;
#endif
    int peer_minor_version;
    int allow_multiple_clients;

    RedsClientMonitorsConfig client_monitors_config;
    int mm_time_enabled;
    uint32_t mm_time_latency;

    int default_channel_security;
    ChannelSecurityOptions *channels_security;
    GArray *renderers;

    int spice_port;
    int spice_secure_port;
    SpiceCharDeviceInstance *vdagent;
    SpiceMigrateInstance *migration_interface;
    int spice_listen_socket_fd;
    char spice_addr[256];
    int spice_family;
    TicketAuthentication taTicket;

    int sasl_enabled;
#if HAVE_SASL
    char *sasl_appname;
#endif
    char *spice_name;

    bool spice_uuid_is_set;
    uint8_t spice_uuid[16];

    gboolean ticketing_enabled;
    uint32_t streaming_video;
    SpiceImageCompression image_compression;
    spice_wan_compression_t jpeg_state;
    spice_wan_compression_t zlib_glz_state;

    gboolean agent_mouse;
    gboolean agent_copypaste;
    gboolean agent_file_xfer;
    gboolean exit_on_disconnect;

    RedSSLParameters ssl_parameters;
    SpiceCoreInterfaceInternal *core;
    GList *qxl_states;
    MainDispatcher *main_dispatcher;
};

#endif
