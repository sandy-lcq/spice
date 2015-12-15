NULL =

AM_CPPFLAGS =					\
	$(GLIB2_CFLAGS)				\
	-I$(top_srcdir)				\
	-I$(top_srcdir)/common			\
	-I$(top_srcdir)/server			\
	-I$(top_builddir)/server		\
	-I$(top_srcdir)/server/tests		\
	$(COMMON_CFLAGS)			\
	$(GLIB2_CFLAGS)				\
	$(SMARTCARD_CFLAGS)			\
	$(SPICE_NONPKGCONFIG_CFLAGS)		\
	$(SPICE_PROTOCOL_CFLAGS)		\
	$(NULL)

if HAVE_AUTOMATED_TESTS
AM_CPPFLAGS += -DAUTOMATED_TESTS
endif

LDADD =								\
	$(top_builddir)/spice-common/common/libspice-common.la	\
	$(top_builddir)/server/libspice-server.la		\
	$(GLIB2_LIBS)						\
	$(SPICE_NONPKGCONFIG_LIBS)		                \
	$(NULL)

COMMON_BASE =					\
	basic_event_loop.c			\
	basic_event_loop.h			\
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
	stream-test				\
	stat_test \
	$(NULL)

TESTS =						\
	stat_test				\
	test-qxl-parsing			\
	stream-test				\
	$(NULL)

check_PROGRAMS = $(TESTS)

noinst_LIBRARIES = \
	libstat_test1.a \
	libstat_test2.a \
	libstat_test3.a \
	libstat_test4.a \
	$(NULL)

test_vdagent_SOURCES =		\
	$(COMMON_BASE)				\
	test_display_base.c			\
	test_display_base.h			\
	test_vdagent.c		\
	$(NULL)

test_display_streaming_SOURCES =		\
	$(COMMON_BASE)				\
	test_display_base.c			\
	test_display_base.h			\
	test_display_streaming.c		\
	$(NULL)

test_display_no_ssl_SOURCES =			\
	$(COMMON_BASE)				\
	test_display_base.c			\
	test_display_base.h			\
	test_display_no_ssl.c 			\
	$(NULL)

test_display_resolution_changes_SOURCES = \
	$(COMMON_BASE)				\
	test_display_base.c			\
	test_display_base.h			\
	test_display_resolution_changes.c	\
	$(NULL)

test_just_sockets_no_ssl_SOURCES =		\
	$(COMMON_BASE)				\
	test_just_sockets_no_ssl.c		\
	$(NULL)

test_playback_SOURCES =				\
	$(COMMON_BASE)				\
	test_playback.c				\
	$(NULL)

test_empty_success_SOURCES =			\
	test_empty_success.c			\
	$(NULL)

test_fail_on_null_core_interface_SOURCES =	\
	test_fail_on_null_core_interface.c	\
	$(NULL)

test_two_servers_SOURCES =			\
	$(COMMON_BASE)				\
	test_display_base.c			\
	test_display_base.h			\
	test_two_servers.c 			\
	$(NULL)

test_display_width_stride_SOURCES =		\
	$(COMMON_BASE)				\
	test_display_base.c			\
	test_display_base.h			\
	test_display_width_stride.c 		\
	$(NULL)

spice_server_replay_SOURCES = 			\
	$(COMMON_BASE)				\
	replay.c				\
	test_display_base.h			\
	$(NULL)

stream_test_SOURCES =				\
	$(COMMON_BASE)				\
	stream-test.c				\
	$(NULL)
stream_test_LDADD = ../libserver.la

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

test_qxl_parsing_SOURCES =           \
	test-qxl-parsing.c      \
	../red-parse-qxl.c      \
	../memslot.c            \
	$(NULL)
