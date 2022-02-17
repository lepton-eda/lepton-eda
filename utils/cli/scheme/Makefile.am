bin_SCRIPTS = \
	lepton-cli \
	lepton-config \
	lepton-export \
	lepton-shell

EXTRA_DIST = \
	lepton-cli.scm \
	lepton-config.scm \
	lepton-export.scm \
	lepton-shell.scm

do_subst = sed -e 's,[@]libdir[@],$(libdir),g' \
	-e 's,[@]GUILE[@],$(GUILE),g' \
	-e 's,[@]localedir[@],$(localedir),g' \
	-e 's,[@]bindir[@],$(bindir),g' \
	-e 's,[@]LEPTON_SCHEME_DIR[@],$(LEPTON_SCHEME_DIR),g' \
	-e 's,[@]ccachedir[@],@LEPTON_CCACHE_DIR@,g'

lepton-cli: lepton-cli.scm
	$(do_subst) < $(srcdir)/$@.scm > $@
	chmod +x $@

lepton-config: lepton-config.scm
	$(do_subst) < $(srcdir)/$@.scm > $@
	chmod +x $@

lepton-export: lepton-export.scm
	$(do_subst) < $(srcdir)/$@.scm > $@
	chmod +x $@

lepton-shell: lepton-shell.scm
	$(do_subst) < $(srcdir)/$@.scm > $@
	chmod +x $@

CLEANFILES = $(bin_SCRIPTS)
