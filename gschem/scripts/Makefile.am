## $Id$
##

scmdatadir = @GEDADATADIR@/scheme
scmdata_DATA = image.scm print.scm

bin_SCRIPTS = gschemdoc
EXTRA_DIST = config.sh.in ChangeLog ChangeLog-1.0 makeallimages \
             gschemdoc.sh makeimages print.scm image.scm

BUILT_SOURCES = config.sh

gschemdoc: config.sh $(srcdir)/gschemdoc.sh
	cat config.sh $(srcdir)/gschemdoc.sh > tmp$@
	chmod a+x tmp$@
	mv -f tmp$@ $@

MOSTLYCLEANFILES =	*.log *~
CLEANFILES = 		*.log *~
DISTCLEANFILES = 	*.log core FILE *~ prototype.bak gschemdoc
MAINTAINERCLEANFILES = 	*.log *~ Makefile.in configure gschemdoc
