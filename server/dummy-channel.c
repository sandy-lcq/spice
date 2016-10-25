/* dummy-channel.c */
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "dummy-channel.h"

G_DEFINE_TYPE(DummyChannel, dummy_channel, RED_TYPE_CHANNEL)

static int dummy_config_socket(RedChannelClient *self)
{
    return 1;
}

static void dummy_on_disconnect(RedChannelClient *self)
{
}

static uint8_t* dummy_alloc_recv_buf(RedChannelClient *self,
                                     uint16_t type,
                                     uint32_t size)
{
    return NULL;
}

static void dummy_release_recv_buf(RedChannelClient *self,
                                   uint16_t type,
                                   uint32_t size,
                                   uint8_t *msg)
{
}

static void
dummy_channel_class_init(DummyChannelClass *klass)
{
    RedChannelClass *channel_class = RED_CHANNEL_CLASS(klass);
    channel_class->config_socket = dummy_config_socket;
    channel_class->on_disconnect = dummy_on_disconnect;
    channel_class->alloc_recv_buf = dummy_alloc_recv_buf;
    channel_class->release_recv_buf = dummy_release_recv_buf;
}

static void
dummy_channel_init(DummyChannel *self)
{
}

// TODO: red_worker can use this one
static void dummy_watch_update_mask(SpiceWatch *watch, int event_mask)
{
}

static SpiceWatch *dummy_watch_add(int fd, int event_mask, SpiceWatchFunc func, void *opaque)
{
    return NULL; // apparently allowed?
}

static void dummy_watch_remove(SpiceWatch *watch)
{
}

// TODO: actually, since I also use channel_client_dummym, no need for core. Can be NULL
static const SpiceCoreInterface dummy_core = {
    .watch_update_mask = dummy_watch_update_mask,
    .watch_add = dummy_watch_add,
    .watch_remove = dummy_watch_remove,
};

RedChannel *dummy_channel_new(RedsState *reds, uint32_t type, uint32_t id)
{
    return g_object_new(TYPE_DUMMY_CHANNEL,
                        "spice-server", reds,
                        "core-interface", &dummy_core,
                        "channel-type", type,
                        "id", id,
                        NULL);
}
