/* Do some tests on memory parsing
 */

#undef NDEBUG
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <strings.h>
#include <assert.h>

#include <spice/macros.h>
#include "memslot.h"
#include "red-parse-qxl.h"

static int exit_code = EXIT_SUCCESS;
static const char *test_name = NULL;

static void
failure(void)
{
    assert(test_name);
    printf("Test %s FAILED!\n", test_name);
    exit_code = EXIT_FAILURE;
}

static void
test(const char *desc)
{
    test_name = desc;
    printf("Starting test %s\n", desc);
}

static inline QXLPHYSICAL
to_physical(const void *ptr)
{
    return (uintptr_t) ptr;
}

int main(int argc, char **argv)
{
    RedMemSlotInfo mem_info;
    memslot_info_init(&mem_info, 1 /* groups */, 1 /* slots */, 1, 1, 0);
    memslot_info_add_slot(&mem_info, 0, 0, 0 /* delta */, 0 /* start */, ~0ul /* end */, 0 /* generation */);

    RedSurfaceCmd cmd;
    QXLSurfaceCmd qxl;

    memset(&qxl, 0, sizeof(qxl));

    qxl.surface_id = 123;

    /* try to create a surface with no issues, should succeed */
    test("no issues");
    qxl.u.surface_create.format = SPICE_SURFACE_FMT_32_xRGB;
    qxl.u.surface_create.width = 128;
    qxl.u.surface_create.stride = 512;
    qxl.u.surface_create.height = 128;
    qxl.u.surface_create.data = to_physical(malloc(0x10000));
    if (red_get_surface_cmd(&mem_info, 0, &cmd, to_physical(&qxl)))
        failure();

    /* try to create a surface with a stride too small to fit
     * the entire width.
     * This can be used to cause buffer overflows so refuse it.
     */
    test("stride too small");
    qxl.u.surface_create.stride = 256;
    if (!red_get_surface_cmd(&mem_info, 0, &cmd, to_physical(&qxl)))
        failure();

    /* try to create a surface quite large.
     * The sizes (width and height) were chosen so the multiplication
     * using 32 bit values gives a very small value.
     * These kind of values should be refused as they will cause
     * overflows. Also the total memory for the card is not enough to
     * hold the surface so surely can't be accepted.
     */
    test("too big image");
    qxl.u.surface_create.stride = 0x08000004 * 4;
    qxl.u.surface_create.width = 0x08000004;
    qxl.u.surface_create.height = 0x40000020;
    if (!red_get_surface_cmd(&mem_info, 0, &cmd, to_physical(&qxl)))
        failure();

    return exit_code;
}
