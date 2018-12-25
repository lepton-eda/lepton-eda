## $Id$
##
## Process this file with automake to produce Makefile.in

bin_SCRIPTS = lepton-refdes_renum \
	      lepton-tragesym lepton-pcb_backannotate lepton-archive lepton-symfix \
	      lepton-schdiff

noinst_SCRIPTS = gschupdate gsymupdate

EXTRA_DIST = gschupdate gsymupdate lepton-refdes_renum lepton-symfix \
	     lepton-tragesym lepton-pcb_backannotate lepton-archive.py \
	     lepton-schdiff

##  SDB's stuff to enable lepton-archive.  This hardcodes the path to the
##  system libraries into lepton-archive prior to installation.
lepton-archive: $(srcdir)/lepton-archive.py
	sed -e "s#GEDADATADIR#$(GEDADATADIR)#" $(srcdir)/lepton-archive.py > tmp$@
	chmod a+x tmp$@
	mv -f tmp$@ $@

MOSTLYCLEANFILES = *.log *.ps core FILE tmp *~
CLEANFILES = *.log *.ps core FILE tmp  *~
DISTCLEANFILES = *.log core FILE tmp *~ lepton-archive
MAINTAINERCLEANFILES = *.log *.ps core FILE tmp *~ Makefile.in lepton-archive
