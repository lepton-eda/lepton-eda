bin_SCRIPTS = lepton-attrib

do_subst = sed -e 's,[@]libdir[@],$(libdir),g' \
	-e 's,[@]GUILE[@],$(GUILE),g' \
	-e 's,[@]localedir[@],$(localedir),g'
	-e 's,[@]ccachedir[@],@LEPTON_SCM_PRECOMPILE_DIR@,g'

lepton-attrib: lepton-attrib.in Makefile
	$(do_subst) < $(srcdir)/$@.in > $@
	chmod +x $@

SUBDIRS = docs

EXTRA_DIST = lepton-attrib.in NOTES README ChangeLog ToDos

if HAVE_GIT_REPO
ChangeLog: $(top_builddir)/stamp-git
	( \
	  cd $(srcdir) && \
	  $(GIT) log --pretty=medium $(CHANGELOG_BASE).. -- . || \
	  echo "WARNING: ChangeLog information not available from git" >&2 ; \
	) > $@
endif HAVE_GIT_REPO

CLEANFILES = $(bin_SCRIPTS)
