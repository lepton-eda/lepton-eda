SUBDIRS = po src include lib design docs data

EXTRA_DIST = BUGS NOTES README ChangeLog ChangeLog-1.0 \
	INSTALL ToDos

if HAVE_GIT_REPO
ChangeLog: $(top_builddir)/stamp-git
	(cd $(srcdir) && $(GIT) log --pretty=medium $(CHANGELOG_BASE).. -- .) > $@
endif HAVE_GIT_REPO

MOSTLYCLEANFILES = *.log core FILE *~ #*#
CLEANFILES = *.log core FILE *~ #*#
DISTCLEANFILES = *.log core FILE *~
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in ChangeLog #*#
