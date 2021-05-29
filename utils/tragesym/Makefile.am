SUBDIRS = examples docs

bin_SCRIPTS = \
	lepton-tragesym

EXTRA_DIST = \
	lepton-tragesym.scm

do_subst = sed -e 's,[@]libdir[@],$(libdir),g' \
	-e 's,[@]GUILE[@],$(GUILE),g' \
	-e 's,[@]LEPTON_SCHEME_MODULE_DIRECTORY[@],$(LEPTON_SCHEME_MODULE_DIRECTORY),g' \
	-e 's,[@]localedir[@],$(localedir),g' \
	-e 's,[@]ccachedir[@],@LEPTON_SCM_PRECOMPILE_DIR@,g'

lepton-tragesym: lepton-tragesym.scm
	$(do_subst) < $(srcdir)/$@.scm > $@
	chmod +x $@

CLEANFILES = \
	lepton-tragesym
