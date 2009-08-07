exampledir = $(docdir)/examples/gTAG/

example_DATA = 7414-1.sym attribs gafrc copyleft.sym crdist.sh \
	     cy7c64603-52nc.sym gTAG-consio.ps gTAG-consio.sch \
             gTAG-consio.sym gTAG-jtagio.ps gTAG-jtagio.sch \
             gTAG-jtagio.sym gTAG-pcb.net gTAG-psu.ps gTAG-psu.sch \
	     gTAG-psu.sym gTAG-ucont.ps gTAG-ucont.sch gTAG-ucont.sym \
             gTAG.bom gTAG.ps gTAG.sch gnetlistrc max882.sym sn75240pw.sym \
	     README

EXTRA_DIST = ChangeLog ChangeLog-1.0 $(example_DATA)

if HAVE_GIT_REPO
ChangeLog: $(top_builddir)/stamp-git
	(cd $(srcdir) && $(GIT) log --pretty=medium $(CHANGELOG_BASE).. -- .) > $@
endif HAVE_GIT_REPO

MOSTLYCLEANFILES = gschem.log *~
CLEANFILES = gschem.log *~
DISTCLEANFILES = *.log core FILE *~
MAINTAINERCLEANFILES = gschem.log *~ Makefile.in
