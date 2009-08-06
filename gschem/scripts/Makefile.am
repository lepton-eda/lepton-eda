scmdatadir = $(GEDADATADIR)/scheme
scmdata_DATA = image.scm print.scm

bin_SCRIPTS = gschemdoc
EXTRA_DIST = ChangeLog ChangeLog-1.0 makeallimages \
             gschemdoc.sh makeimages print.scm image.scm

if HAVE_GIT_REPO
ChangeLog: $(top_builddir)/stamp-git
	(cd $(srcdir) && $(GIT) log --pretty=medium $(CHANGELOG_BASE).. -- .) > $@
endif HAVE_GIT_REPO

gschemdoc: gschemdoc.sh
	sed -e 's:[@]DOCDIR@:$(docdir):' < $(srcdir)/gschemdoc.sh > tmp_gschemdoc
	chmod a+x tmp_gschemdoc
	mv -f tmp_gschemdoc gschemdoc

MOSTLYCLEANFILES =	*.log *~
CLEANFILES = 		*.log *~
DISTCLEANFILES = 	*.log core FILE *~ gschemdoc
MAINTAINERCLEANFILES = 	*.log *~ Makefile.in gschemdoc
