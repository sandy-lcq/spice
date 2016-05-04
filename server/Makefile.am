NULL =
SUBDIRS = . tests

AM_CPPFLAGS =					\
	-DSPICE_SERVER_INTERNAL			\
	-DRED_STATISTICS			\
	$(COMMON_CFLAGS)			\
	$(GLIB2_CFLAGS)				\
	$(GOBJECT2_CFLAGS)			\
	$(LZ4_CFLAGS)				\
	$(PIXMAN_CFLAGS)			\
	$(SASL_CFLAGS)				\
	$(SLIRP_CFLAGS)				\
	$(SMARTCARD_CFLAGS)			\
	$(SPICE_PROTOCOL_CFLAGS)		\
	$(SSL_CFLAGS)				\
	$(VISIBILITY_HIDDEN_CFLAGS)		\
	$(WARN_CFLAGS)				\
	$(NULL)

noinst_LTLIBRARIES = libserver.la
lib_LTLIBRARIES = libspice-server.la

libspice_server_la_LDFLAGS =			\
	-version-info $(SPICE_LT_VERSION)	\
	-no-undefined				\
	$(RELRO_LDFLAGS)			\
	$(NO_INDIRECT_LDFLAGS)			\
	$(NULL)

if HAVE_LD_VERSION_SCRIPT
libspice_server_la_LDFLAGS += \
	-Wl,--version-script=$(top_srcdir)/server/spice-server.syms
endif

libserver_la_LIBADD =							\
	$(top_builddir)/spice-common/common/libspice-common.la		\
	$(top_builddir)/spice-common/common/libspice-common-server.la	\
	$(GL_LIBS)							\
	$(GLIB2_LIBS)							\
	$(GOBJECT2_LIBS)						\
	$(JPEG_LIBS)							\
	$(LZ4_LIBS)							\
	$(LIBRT)							\
	$(PIXMAN_LIBS)							\
	$(SASL_LIBS)							\
	$(SLIRP_LIBS)							\
	$(SSL_LIBS)							\
	$(Z_LIBS)							\
	$(SPICE_NONPKGCONFIG_LIBS)					\
	$(NULL)

libspice_serverincludedir = $(includedir)/spice-server
libspice_serverinclude_HEADERS =		\
	spice-audio.h				\
	spice-char.h				\
	spice-core.h				\
	spice-experimental.h			\
	spice-input.h				\
	spice-migration.h			\
	spice-qxl.h				\
	spice-server.h				\
	spice-version.h				\
	spice.h					\
	$(NULL)

libserver_la_SOURCES =				\
	$(libspice_serverinclude_HEADERS)	\
	agent-msg-filter.c			\
	agent-msg-filter.h			\
	cache-item.h				\
	char-device.c				\
	char-device.h				\
	demarshallers.h				\
	event-loop.c				\
	glz-encoder.c				\
	glz-encoder.h				\
	glz-encoder-dict.c		\
	glz-encoder-dict.h		\
	glz-encoder-priv.h	\
	inputs-channel.c			\
	inputs-channel.h			\
	jpeg-encoder.c				\
	jpeg-encoder.h				\
	lz4-encoder.c				\
	lz4-encoder.h				\
	main-channel.c				\
	main-channel.h				\
	mjpeg-encoder.c				\
	red-channel.c				\
	red-channel.h				\
	red-common.h				\
	dispatcher.c				\
	dispatcher.h				\
	red-qxl.c				\
	red-qxl.h				\
	main-dispatcher.c			\
	main-dispatcher.h			\
	migration-protocol.h		\
	memslot.c				\
	memslot.h				\
	red-parse-qxl.c				\
	red-record-qxl.c			\
	red-record-qxl.h			\
	red-replay-qxl.c			\
	red-replay-qxl.h			\
	red-parse-qxl.h				\
	red-worker.c				\
	red-worker.h				\
	display-channel.c			\
	display-channel.h			\
	cursor-channel.c			\
	cursor-channel.h			\
	red-pipe-item.c				\
	red-pipe-item.h				\
	reds.c					\
	reds.h					\
	reds-private.h				\
	reds-stream.c				\
	reds-stream.h				\
	sw-canvas.c			\
	sw-canvas.h			\
	sound.c				\
	sound.h				\
	stat.h					\
	spicevmc.c				\
	video-encoder.h				\
	zlib-encoder.c				\
	zlib-encoder.h				\
	image-cache.h			\
	image-cache.c			\
	pixmap-cache.h				\
	pixmap-cache.c				\
	tree.h				\
	tree.c				\
	spice-bitmap-utils.h			\
	spice-bitmap-utils.c			\
	utils.c					\
	utils.h					\
	stream.c					\
	stream.h					\
	dcc.c					\
	dcc-send.c					\
	dcc.h					\
	display-limits.h			\
	dcc-encoders.c					\
	dcc-encoders.h					\
	$(NULL)

if HAVE_SMARTCARD
libserver_la_SOURCES +=	\
	smartcard.c		\
	smartcard.h		\
	$(NULL)
endif

libspice_server_la_LIBADD = libserver.la
libspice_server_la_SOURCES =

EXTRA_DIST =					\
	spice-bitmap-utils.tmpl.c			\
	cache-item.tmpl.c			\
	glz-encode-match.tmpl.c			\
	glz-encode.tmpl.c			\
	spice-server.syms			\
	$(NULL)

BUILT_SOURCES = $(spice_built_sources)
