
SUBDIRS = gTAG lightning_detector RF_Amp TwoStageAmp

exampledir = $(docdir)/examples/

example_DATA = README 

EXTRA_DIST = ChangeLog ChangeLog-1.0 $(example_DATA)

if HAVE_GIT_REPO
ChangeLog: $(top_builddir)/stamp-git
	(cd $(srcdir) && $(GIT) log --pretty=medium $(CHANGELOG_BASE).. -- .) > $@
endif HAVE_GIT_REPO

MOSTLYCLEANFILES = *.log core FILE *~ 
CLEANFILES = *.log core FILE *~ 
DISTCLEANFILES = *.log core FILE *~
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in ChangeLog
