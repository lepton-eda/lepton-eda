SUBDIRS = \
	po bitmap data docs examples include lib scheme \
	src tests

pkgconfigdir   = $(libdir)/pkgconfig
pkgconfig_DATA = libleptongui.pc

libleptongui-pc-install: libleptongui.pc
	$(mkinstalldirs) $(DESTDIR)$(pkgconfigdir)
	$(INSTALL_DATA) libleptongui.pc $(DESTDIR)$(pkgconfigdir)


EXTRA_DIST = ChangeLog

if HAVE_GIT_REPO
ChangeLog: $(top_builddir)/stamp-git
	( \
	  cd $(srcdir) && \
	  $(GIT) log --pretty=medium $(CHANGELOG_BASE).. -- . || \
	  echo "WARNING: ChangeLog information not available from git" >&2 ; \
	) > $@
endif HAVE_GIT_REPO

maintainer-clean-local:
	-rm -rf po/*.sed po/*.header po/*.sin po/*.template po/Rules-quot
