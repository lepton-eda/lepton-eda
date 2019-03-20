## $Id$
##
## Process this file with automake to produce Makefile.in

bin_SCRIPTS = lepton-refdes_renum \
	      lepton-tragesym lepton-pcb_backannotate lepton-archive lepton-symfix \
	      lepton-schdiff

noinst_SCRIPTS = gschupdate gsymupdate

EXTRA_DIST = gschupdate gsymupdate lepton-refdes_renum lepton-symfix \
	     lepton-tragesym lepton-pcb_backannotate lepton-archive.in \
	     lepton-schdiff

do_subst = sed -e 's,[@]libdir[@],$(libdir),g' \
	-e 's,[@]GUILE[@],$(GUILE),g' \
	-e 's,[@]ccachedir[@],@LEPTON_SCM_PRECOMPILE_DIR@,g'

lepton-archive: lepton-archive.in
	$(do_subst) < $(srcdir)/$@.in > $@
	chmod +x $@

MOSTLYCLEANFILES = *.log *.ps core FILE tmp *~
CLEANFILES = lepton-archive
DISTCLEANFILES = *.log core FILE tmp *~
MAINTAINERCLEANFILES = *.log *.ps core FILE tmp *~ Makefile.in
