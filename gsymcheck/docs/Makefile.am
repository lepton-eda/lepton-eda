
EXTRA_DIST = gsymcheck.1 gsymcheck.html logo.png

BUILT_SOURCES = gsymcheck.html

man_MANS = gsymcheck.1

docsmandir = $(docdir)/man
docsman_DATA = gsymcheck.html

.1.html:
if ENABLE_GROFF
	LC_NUMERIC=C $(GROFF) -man -T html $< > $@.tmp \
	  && mv -f $@.tmp $@
else
	echo "Cannot build $@ from $<: groff is missing."
endif

MOSTLYCLEANFILES = *.log *~
CLEANFILES = *.log *~ gsymcheck.html
DISTCLEANFILES = *.log core FILE *~
MAINTAINERCLEANFILES = *.log *~ Makefile.in gsymcheck.html

