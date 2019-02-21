## $Id$
##
bin_SCRIPTS = lepton-upcfg

do_subst = sed -e 's,[@]libdir[@],$(libdir),g' \
	-e 's,[@]GUILE[@],$(GUILE),g' \
	-e 's,[@]ccachedir[@],@LEPTON_SCM_PRECOMPILE_DIR@,g'

lepton-upcfg: lepton-upcfg.in Makefile
	$(do_subst) < $(srcdir)/$@.in > $@
	chmod +x $@

EXTRA_DIST = lepton-upcfg.in

CLEANFILES = $(bin_SCRIPTS)
MAINTAINERCLEANFILES = *~ Makefile.in
