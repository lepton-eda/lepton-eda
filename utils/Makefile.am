
SUBDIRS = src schlas scripts include examples docs tests upcfg

EXTRA_DIST = ChangeLog ChangeLog.tragesym-1.0 ChangeLog.gsch2pcb-1.0 \
	ChangeLog-1.0 AUTHORS

if HAVE_GIT_REPO
ChangeLog: $(top_builddir)/stamp-git
	( \
	  cd $(srcdir) && \
	  $(GIT) log --pretty=medium $(CHANGELOG_BASE).. -- . || \
	  echo "WARNING: ChangeLog information not available from git" >&2 ; \
	) > $@
endif HAVE_GIT_REPO
