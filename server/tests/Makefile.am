NULL =

@VALGRIND_CHECK_RULES@
VALGRIND_SUPPRESSIONS_FILES = $(top_srcdir)/server/tests/valgrind/glib.supp
EXTRA_DIST =				\
	$(VALGRIND_SUPPRESSIONS_FILES)	\
	pki/ca-cert.pem			\
	pki/server-cert.pem		\
	pki/server-key.pem		\
	$(NULL)

AM_CPPFLAGS =					\
	-DSPICE_TOP_SRCDIR=\"$(abs_top_srcdir)\"\
	-I$(top_srcdir)				\
	-I$(top_srcdir)/server			\
	-I$(top_builddir)/server		\
	-I$(top_srcdir)/server/tests		\
	$(COMMON_CFLAGS)			\
	$(GLIB2_CFLAGS)				\
	$(GOBJECT2_CFLAGS)				\
	$(SMARTCARD_CFLAGS)			\
	$(SPICE_NONPKGCONFIG_CFLAGS)		\
	$(SPICE_PROTOCOL_CFLAGS)		\
	$(WARN_CFLAGS)				\
	$(NULL)

if HAVE_AUTOMATED_TESTS
AM_CPPFLAGS += -DAUTOMATED_TESTS
endif

noinst_LIBRARIES = libtest.a

libtest_a_SOURCES =				\
	basic-event-loop.c			\
	basic-event-loop.h			\
	test-display-base.c			\
	test-display-base.h			\
	test-glib-compat.c			\
	test-glib-compat.h			\
	$(NULL)

LDADD =								\
	libtest.a						\
	$(top_builddir)/spice-common/common/libspice-common.la	\
	$(top_builddir)/server/libserver.la			\
	$(GLIB2_LIBS)						\
	$(GOBJECT2_LIBS)					\
	$(SPICE_NONPKGCONFIG_LIBS)		                \
	$(NULL)

check_PROGRAMS =				\
	test-codecs-parsing			\
	test-options				\
	test-stat				\
	test-stream				\
	test-agent-msg-filter			\
	test-loop				\
	test-qxl-parsing			\
	test-stat-file				\
	test-leaks				\
	test-vdagent				\
	$(NULL)

noinst_PROGRAMS =				\
	test-display-no-ssl			\
	test-display-streaming			\
	test-empty-success			\
	test-fail-on-null-core-interface	\
	test-just-sockets-no-ssl		\
	test-playback				\
	test-display-resolution-changes		\
	test-two-servers			\
	test-display-width-stride		\
	spice-server-replay			\
	$(check_PROGRAMS)			\
	$(NULL)

if HAVE_GSTREAMER
noinst_PROGRAMS += test-gst
endif

TESTS = $(check_PROGRAMS)			\
	$(NULL)

if ENABLE_EXTRA_CHECKS
if HAVE_GSTREAMER_1_0
TESTS += video-encoders
endif
endif
EXTRA_DIST += video-encoders

noinst_LIBRARIES += \
	libtest-stat1.a \
	libtest-stat2.a \
	libtest-stat3.a \
	libtest-stat4.a \
	$(NULL)

spice_server_replay_SOURCES = replay.c		\
	../event-loop.c				\
	basic-event-loop.c			\
	basic-event-loop.h

spice_server_replay_LDADD =					\
	$(top_builddir)/spice-common/common/libspice-common.la	\
	$(top_builddir)/server/libspice-server.la		\
	$(GLIB2_LIBS)						\
	$(GOBJECT2_LIBS)					\
	$(SPICE_NONPKGCONFIG_LIBS)		                \
	$(NULL)

test_stat_SOURCES = stat-main.c
test_stat_LDADD = \
	libtest-stat1.a \
	libtest-stat2.a \
	libtest-stat3.a \
	libtest-stat4.a \
	$(LDADD) \
	$(NULL)

libtest_stat1_a_SOURCES = stat-test.c
libtest_stat1_a_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_COMPRESS_STAT=0 -DTEST_RED_WORKER_STAT=0 -DTEST_NAME=stat_test1

libtest_stat2_a_SOURCES = stat-test.c
libtest_stat2_a_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_COMPRESS_STAT=0 -DTEST_RED_WORKER_STAT=1 -DTEST_NAME=stat_test2

libtest_stat3_a_SOURCES = stat-test.c
libtest_stat3_a_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_COMPRESS_STAT=1 -DTEST_RED_WORKER_STAT=0 -DTEST_NAME=stat_test3

libtest_stat4_a_SOURCES = stat-test.c
libtest_stat4_a_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_COMPRESS_STAT=1 -DTEST_RED_WORKER_STAT=1 -DTEST_NAME=stat_test4

test_qxl_parsing_LDADD = ../libserver.la $(LDADD)

# Fallback implementations are provided for older glibs for the recent glib
# methods this test is using, so no need to warn about them
test_vdagent_CPPFLAGS =			\
	$(AM_CPPFLAGS)			\
	-UGLIB_VERSION_MIN_REQUIRED	\
	-UGLIB_VERSION_MAX_ALLOWED	\
	$(NULL)
test_codecs_parsing_CPPFLAGS = $(test_vdagent_CPPFLAGS)
test_qxl_parsing_CPPFLAGS = $(test_vdagent_CPPFLAGS)

if HAVE_GSTREAMER
test_gst_SOURCES = test-gst.c \
	$(NULL)
test_gst_CPPFLAGS = \
	$(AM_CPPFLAGS) \
	$(GSTREAMER_0_10_CFLAGS)		\
	$(GSTREAMER_1_0_CFLAGS)			\
	$(NULL)
endif
