NULL =
ASCIIDOC_FLAGS = -a icons -a toc

EXTRA_DIST =					\
	spice_style.html			\
	spice_style.txt				\
	$(NULL)

if BUILD_MANUAL
SUBDIRS = manual

all-local: spice_style.html

spice_style.html: spice_style.txt
	$(AM_V_GEN) $(ASCIIDOC) -n $(ASCIIDOC_FLAGS) -o $@ $<
endif

clean-local:
	rm -f spice_style.html
