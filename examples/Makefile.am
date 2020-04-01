SUBDIRS = gTAG lightning_detector RF_Amp TwoStageAmp

exampledir = $(docdir)/examples/

example_DATA = README

EXTRA_DIST = ChangeLog $(example_DATA)

if HAVE_GIT_REPO
ChangeLog: $(top_builddir)/stamp-git
	( \
	  cd $(srcdir) && \
	  $(GIT) log --pretty=medium $(CHANGELOG_BASE).. -- . || \
	  echo "WARNING: ChangeLog information not available from git" >&2 ; \
	) > $@
endif HAVE_GIT_REPO
