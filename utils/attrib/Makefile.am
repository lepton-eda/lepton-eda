bin_SCRIPTS = lepton-attrib

lepton-attrib: lepton-attrib.scm Makefile
	$(do_subst) < $(srcdir)/$@.scm > $@
	chmod +x $@

do_subst = sed \
	-e 's,[@]libdir[@],$(libdir),g' \
	-e 's,[@]GUILE[@],$(GUILE),g' \
	-e 's,[@]LEPTON_SCHEME_DIR[@],$(LEPTON_SCHEME_DIR),g' \
	-e 's,[@]localedir[@],$(localedir),g' \
	-e 's,[@]ccachedir[@],@LEPTON_SCM_PRECOMPILE_DIR@,g'

EXTRA_DIST = \
	lepton-attrib.scm

CLEANFILES = $(bin_SCRIPTS)
