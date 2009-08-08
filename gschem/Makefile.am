SUBDIRS = \
	po bitmap data docs examples include lib scheme scripts \
	src tests

EXTRA_DIST = BUGS ChangeLog ChangeLog-1.0 icon-theme-installer

if HAVE_GIT_REPO
ChangeLog: $(top_builddir)/stamp-git
	(cd $(srcdir) && $(GIT) log --pretty=medium $(CHANGELOG_BASE).. -- .) > $@
endif HAVE_GIT_REPO

maintainer-clean-local:
	-rm -rf po/*.sed po/*.header po/*.sin po/*.template po/Rules-quot

MOSTLYCLEANFILES = *.log core FILE *~
CLEANFILES = *.log core FILE *~
DISTCLEANFILES = *.log core FILE *~
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in
