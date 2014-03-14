all: html
.PHONY: html

# apparently, xmlto does not support validation of docbook5 docs
# that's why it's disabled with --skip-validation
XMLTO_FLAGS = --skip-validation

XMLDOC =					\
	SpiceUserManual-Basics.xml		\
	SpiceUserManual-Guest.xml		\
	SpiceUserManual-Installation.xml	\
	SpiceUserManual-Introduction.xml	\
	SpiceUserManual-References.xml		\
	SpiceUserManual.xml			\
	$(NULL)

html-stamp: $(XMLDOC)
	$(AM_V_GEN)$(XMLTO) $(XMLTO_FLAGS) -o html xhtml $(srcdir)/SpiceUserManual.xml
	touch $@

html: html-stamp

# Control what goes in the distribution tarball.
# We include all of the XML, and also generated HTML pages
# so people working from the distribution tarball won't need xmlto.
EXTRA_DIST = $(XMLDOC) html html-stamp

clean-local:
	rm -fr html
	rm -f *-stamp
