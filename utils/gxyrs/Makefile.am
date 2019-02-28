## $Id$
##
## Process this file with automake to produce Makefile.in

bin_SCRIPTS = lepton-xyrs

rcdatadir = @GEDADATADIR@/perl/lib
rcdata_DATA = xyrs.pm

EXTRA_DIST = \
	lepton-xyrs.pl \
	xyrs.pm

all: lepton-xyrs

distclean-local:
	-rm -rf autom4te.cache

maintainer-clean-local:
	-rm -rf autom4te.cache

##  This hardcodes the path to the
##  system libraries into geda_filter prior to installation.
lepton-xyrs: $(srcdir)/lepton-xyrs.pl
	sed -e "s#GEDADATADIR#$(GEDADATADIR)#" $(srcdir)/lepton-xyrs.pl | sed -e "s#GEDARCDIR#$(GEDARCDIR)#" > tmp$@
	chmod a+x tmp$@
	mv -f tmp$@ $@

MOSTLYCLEANFILES = *.log *.ps core FILE tmp *~
CLEANFILES = lepton-xyrs
DISTCLEANFILES = *.log core FILE tmp *~
MAINTAINERCLEANFILES = *.log *.ps core FILE tmp *~ Makefile.in
