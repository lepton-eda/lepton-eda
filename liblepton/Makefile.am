SUBDIRS = po data docs include lib src scheme tests

EXTRA_DIST = HACKING ChangeLog

pkgconfigdir            = $(libdir)/pkgconfig
pkgconfig_DATA          = liblepton.pc

liblepton-pc-install: liblepton.pc
	$(mkinstalldirs) $(DESTDIR)$(pkgconfigdir)
	$(INSTALL_DATA) liblepton.pc $(DESTDIR)$(pkgconfigdir)

if HAVE_GIT_REPO
ChangeLog: $(top_builddir)/stamp-git
	( \
	  cd $(srcdir) && \
	  $(GIT) log --pretty=medium $(CHANGELOG_BASE).. -- . || \
	  echo "WARNING: ChangeLog information not available from git" >&2 ; \
	) > $@
endif HAVE_GIT_REPO
