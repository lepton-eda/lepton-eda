SUBDIRS = \
	tests \
	docs

bin_SCRIPTS = \
	lepton-symcheck

EXTRA_DIST = \
	lepton-symcheck.scm

do_subst = sed -e 's,[@]libdir[@],$(libdir),g' \
	-e 's,[@]GUILE[@],$(GUILE),g' \
	-e 's,[@]LEPTON_SCHEME_DIR[@],$(LEPTON_SCHEME_DIR),g' \
	-e 's,[@]localedir[@],$(localedir),g' \
	-e 's,[@]ccachedir[@],@LEPTON_SCM_PRECOMPILE_DIR@,g'

lepton-symcheck: lepton-symcheck.scm
	$(do_subst) < $(srcdir)/$@.scm > $@
	chmod +x $@

CLEANFILES = \
	lepton-symcheck
