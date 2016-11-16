NULL =

AM_CPPFLAGS =					\
	-I$(top_srcdir)				\
	-I$(top_srcdir)/common			\
	-I$(top_srcdir)/server			\
	-I$(top_builddir)/server		\
	-I$(top_srcdir)/server/tests		\
	$(COMMON_CFLAGS)			\
	$(GLIB2_CFLAGS)				\
	$(GOBJECT2_CFLAGS)				\
	$(SMARTCARD_CFLAGS)			\
	$(SPICE_NONPKGCONFIG_CFLAGS)		\
	$(SPICE_PROTOCOL_CFLAGS)		\
	$(NULL)

if HAVE_AUTOMATED_TESTS
AM_CPPFLAGS += -DAUTOMATED_TESTS
endif

noinst_LIBRARIES = libtest.a

libtest_a_SOURCES =				\
	basic_event_loop.c			\
	basic_event_loop.h			\
	test_display_base.c			\
	test_display_base.h			\
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
	spice-codecs-parsing-test		\
	spice-options-test			\
	stat_test				\
	stream-test				\
	test-agent-msg-filter			\
	test-loop				\
	test-qxl-parsing			\
	stat-file-test				\
	$(NULL)

noinst_PROGRAMS =				\
	test_display_no_ssl			\
	test_display_streaming			\
	test_empty_success			\
	test_fail_on_null_core_interface	\
	test_just_sockets_no_ssl		\
	test_playback				\
	test_display_resolution_changes		\
	test_two_servers			\
	test_vdagent				\
	test_display_width_stride		\
	spice-server-replay			\
	$(check_PROGRAMS)			\
	$(NULL)

TESTS = $(check_PROGRAMS)			\
	$(NULL)

noinst_LIBRARIES += \
	libstat_test1.a \
	libstat_test2.a \
	libstat_test3.a \
	libstat_test4.a \
	$(NULL)

spice_server_replay_SOURCES = replay.c		\
	../event-loop.c				\
	basic_event_loop.c			\
	basic_event_loop.h

spice_server_replay_LDADD =					\
	$(top_builddir)/spice-common/common/libspice-common.la	\
	$(top_builddir)/server/libspice-server.la		\
	$(GLIB2_LIBS)						\
	$(GOBJECT2_LIBS)					\
	$(SPICE_NONPKGCONFIG_LIBS)		                \
	$(NULL)

stat_test_SOURCES = stat-main.c
stat_test_LDADD = \
	libstat_test1.a \
	libstat_test2.a \
	libstat_test3.a \
	libstat_test4.a \
	$(LDADD) \
	$(NULL)

libstat_test1_a_SOURCES = stat-test.c
libstat_test1_a_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_COMPRESS_STAT=0 -DTEST_RED_WORKER_STAT=0 -DTEST_NAME=stat_test1

libstat_test2_a_SOURCES = stat-test.c
libstat_test2_a_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_COMPRESS_STAT=0 -DTEST_RED_WORKER_STAT=1 -DTEST_NAME=stat_test2

libstat_test3_a_SOURCES = stat-test.c
libstat_test3_a_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_COMPRESS_STAT=1 -DTEST_RED_WORKER_STAT=0 -DTEST_NAME=stat_test3

libstat_test4_a_SOURCES = stat-test.c
libstat_test4_a_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_COMPRESS_STAT=1 -DTEST_RED_WORKER_STAT=1 -DTEST_NAME=stat_test4

test_qxl_parsing_LDADD = ../libserver.la $(LDADD)
