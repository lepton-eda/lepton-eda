SUBDIRS = \
	po bitmap data docs examples include lib scheme \
	src tests

EXTRA_DIST = ChangeLog ChangeLog-1.0

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

MAINTAINERCLEANFILES = Makefile.in
