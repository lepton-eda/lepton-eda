bin_SCRIPTS = \
	lepton-tragesym

EXTRA_DIST = \
	lepton-tragesym.in

do_subst = sed -e 's,[@]libdir[@],$(libdir),g' \
	-e 's,[@]GUILE[@],$(GUILE),g' \
	-e 's,[@]localedir[@],$(localedir),g' \
	-e 's,[@]ccachedir[@],@LEPTON_SCM_PRECOMPILE_DIR@,g'

lepton-tragesym: lepton-tragesym.in
	$(do_subst) < $(srcdir)/$@.in > $@
	chmod +x $@

CLEANFILES = \
	lepton-tragesym
