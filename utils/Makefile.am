SUBDIRS = \
	docs \
	examples \
	scripts \
	src \
	tests \
	sch2pcb

EXTRA_DIST = \
	AUTHORS \
	ChangeLog

if HAVE_GIT_REPO
ChangeLog: $(top_builddir)/stamp-git
	( \
	  cd $(srcdir) && \
	  $(GIT) log --pretty=medium $(CHANGELOG_BASE).. -- . || \
	  echo "WARNING: ChangeLog information not available from git" >&2 ; \
	) > $@
endif HAVE_GIT_REPO
