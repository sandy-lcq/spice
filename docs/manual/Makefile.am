SUFFIXES = .xml .html .txt .1

# apparently, xmlto does not support validation of docbook5 docs
# that's why it's disabled with --skip-validation
.xml.html:
	$(AM_V_GEN)$(XMLTO) --skip-validation -o html xhtml $<

.xml.1:
	$(AM_V_GEN)$(XMLTO) --skip-validation -o man man $<

.xml.txt:
	$(AM_V_GEN)$(XMLTO) --skip-validation -o txt txt $<

all: allhtml manpages

XMLMAN =
XMLDOC = \
	SpiceUserManual-Basics.xml \
	SpiceUserManual-Guest.xml \
	SpiceUserManual-Installation.xml \
	SpiceUserManual-Introduction.xml \
	SpiceUserManual-References.xml \
	SpiceUserManual.xml
XMLALL = $(XMLMAN) $(XMLDOC)
SOURCES = $(XMLALL) $(TXTDOC)

allhtml: $(XMLALL:.xml=.html)

manpages: $(XMLMAN:.xml=.1)

# Control what goes in the distribution tarball.
# We include all of the XML, and also generated HTML pages
# so people working from the distribution tarball won't need xmlto.
EXTRA_DIST = $(SOURCES) html

clean-local:
	rm -fr html $(XMLMAN:.xml=.1)

