NULL =
SUFFIXES = .html
ASCIIDOC_FLAGS = -a icons -a toc

EXTRA_DIST =					\
	docbook-xsl.css				\
	images/icons/*.png			\
	images/spicec01.png			\
	manual.html				\
	manual.txt				\
	$(NULL)

.txt.html:
	$(AM_V_GEN) $(ASCIIDOC) $(ASCIIDOC_FLAGS) $<

all-local: manual.html

clean-local:
	rm manual.html
