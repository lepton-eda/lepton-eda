
SUBDIRS = \
	docs \
	examples \
	scripts \
	src \
	tests

EXTRA_DIST = \
	AUTHORS \
	ChangeLog \
	ChangeLog-1.0 \
	ChangeLog.gsch2pcb-1.0 \
	ChangeLog.tragesym-1.0

if HAVE_GIT_REPO
ChangeLog: $(top_builddir)/stamp-git
	( \
	  cd $(srcdir) && \
	  $(GIT) log --pretty=medium $(CHANGELOG_BASE).. -- . || \
	  echo "WARNING: ChangeLog information not available from git" >&2 ; \
	) > $@
endif HAVE_GIT_REPO
