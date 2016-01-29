## $Id$
##
## Process this file with automake to produce Makefile.in

bin_SCRIPTS = refdes_renum \
	      tragesym pcb_backannotate garchive gsymfix \
	      schdiff

EXTRA_DIST = refdes_renum gsymfix \
	     tragesym pcb_backannotate garchive.py \
	     schdiff

##  SDB's stuff to enable garchive.  This hardcodes the path to the
##  system libraries into garchive prior to installation.
garchive: $(srcdir)/garchive.py
	sed -e "s#GEDADATADIR#$(GEDADATADIR)#" $(srcdir)/garchive.py > tmp$@
	chmod a+x tmp$@
	mv -f tmp$@ $@

MOSTLYCLEANFILES = *.log *.ps core FILE tmp *~
CLEANFILES = *.log *.ps core FILE tmp  *~
DISTCLEANFILES = *.log core FILE tmp *~ garchive sw2asc
MAINTAINERCLEANFILES = *.log *.ps core FILE tmp *~ Makefile.in garchive
