NULL =
SUFFIXES = .html
ASCIIDOC_FLAGS = -a icons -a toc

EXTRA_DIST =					\
	docbook-xsl.css				\
	images/icons/*.png			\
	images/spicec01.png			\
	manual.html				\
	manual.chunked				\
	manual.txt				\
	$(NULL)

.txt.html:
	$(AM_V_GEN) $(ASCIIDOC) $(ASCIIDOC_FLAGS) $<

manual.chunked: manual.txt
	$(AM_V_GEN) $(A2X) -f chunked -D $(builddir) $(ASCIIDOC_FLAGS) $<

docfiles =
if BUILD_HTML_MANUAL
docfiles += manual.html
endif
if BUILD_CHUNKED_MANUAL
docfiles += manual.chunked
endif

all-local: $(docfiles)

clean-local:
	rm -rf manual.chunked
