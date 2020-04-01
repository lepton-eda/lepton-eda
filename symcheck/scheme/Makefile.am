## -*- Makefile -*-

bin_SCRIPTS = lepton-symcheck
EXTRA_DIST = lepton-symcheck.in

do_subst = sed -e 's,[@]libdir[@],$(libdir),g' \
	-e 's,[@]GUILE[@],$(GUILE),g' \
	-e 's,[@]localedir[@],$(localedir),g'
	-e 's,[@]ccachedir[@],@LEPTON_SCM_PRECOMPILE_DIR@,g'

lepton-symcheck: lepton-symcheck.in Makefile
	$(do_subst) < $(srcdir)/$@.in > $@
	chmod +x $@

CLEANFILES = $(bin_SCRIPTS)
