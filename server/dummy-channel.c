/* dummy-channel.c */
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "dummy-channel.h"

G_DEFINE_TYPE(DummyChannel, dummy_channel, RED_TYPE_CHANNEL)

static void
dummy_channel_class_init(DummyChannelClass *klass)
{
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
