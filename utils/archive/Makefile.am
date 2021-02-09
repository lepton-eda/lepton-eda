SUBDIRS = \
	docs

bin_SCRIPTS = \
	lepton-archive

EXTRA_DIST = \
	lepton-archive.in

do_subst = sed -e 's,[@]libdir[@],$(libdir),g' \
	-e 's,[@]GUILE[@],$(GUILE),g' \
	-e 's,[@]localedir[@],$(localedir),g' \
	-e 's,[@]ccachedir[@],@LEPTON_SCM_PRECOMPILE_DIR@,g'

lepton-archive: lepton-archive.in
	$(do_subst) < $(srcdir)/$@.in > $@
	chmod +x $@

CLEANFILES = \
	lepton-archive
