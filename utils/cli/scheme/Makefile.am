bin_SCRIPTS = \
	lepton-cli

EXTRA_DIST = \
	lepton-cli.scm

do_subst = sed -e 's,[@]libdir[@],$(libdir),g' \
	-e 's,[@]GUILE[@],$(GUILE),g' \
	-e 's,[@]localedir[@],$(localedir),g' \
	-e 's,[@]bindir[@],$(bindir),g' \
	-e 's,[@]LEPTON_SCHEME_MODULE_DIRECTORY[@],$(LEPTON_SCHEME_MODULE_DIRECTORY),g' \
	-e 's,[@]ccachedir[@],@LEPTON_SCM_PRECOMPILE_DIR@,g'

lepton-cli: lepton-cli.scm
	$(do_subst) < $(srcdir)/$@.scm > $@
	chmod +x $@

CLEANFILES = \
	lepton-cli
