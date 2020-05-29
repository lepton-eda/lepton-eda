bin_SCRIPTS = lepton-attrib

SUBDIRS = po src include design data docs

pkgconfigdir            = $(libdir)/pkgconfig
pkgconfig_DATA          = libleptonattrib.pc

libleptonattrib-pc-install: libleptonattrib.pc
	$(mkinstalldirs) $(DESTDIR)$(pkgconfigdir)
	$(INSTALL_DATA) libleptonattrib.pc $(DESTDIR)$(pkgconfigdir)

lepton-attrib: lepton-attrib.in Makefile
	$(do_subst) < $(srcdir)/$@.in > $@
	chmod +x $@

if HAVE_GIT_REPO
ChangeLog: $(top_builddir)/stamp-git
	( \
	  cd $(srcdir) && \
	  $(GIT) log --pretty=medium $(CHANGELOG_BASE).. -- . || \
	  echo "WARNING: ChangeLog information not available from git" >&2 ; \
	) > $@
endif HAVE_GIT_REPO

do_subst = sed -e 's,[@]libdir[@],$(libdir),g' \
	-e 's,[@]GUILE[@],$(GUILE),g' \
	-e 's,[@]localedir[@],$(localedir),g' \
	-e 's,[@]ccachedir[@],@LEPTON_SCM_PRECOMPILE_DIR@,g'

EXTRA_DIST = lepton-attrib.in NOTES README ChangeLog ToDos

CLEANFILES = $(bin_SCRIPTS)

