## Process this file with automake to produce Makefile.in

EXTRA_DIST = gnetlistrc.hierarchy2 hierarchy2.geda top.sch under.sch under.sym

# Temporarily disabled make check, since this is interfering with 
# make distcheck
check_SCRIPTS = tests

BUILDDIR=$(top_builddir)/gnetlist/tests/hierarchy2
SRCDIR=$(srcdir)
GNETLIST=$(top_builddir)/gnetlist/src/gnetlist

tests:
	rm -f $(BUILDDIR)/new_*
	cp -f $(SRCDIR)/gnetlistrc.hierarchy2 $(BUILDDIR)/gnetlistrc
	( TESTDIR=$(SRCDIR) \
	  GEDADATARC=$(top_builddir)/gnetlist/lib \
	  SCMDIR=${top_builddir}/gnetlist/scheme \
	  SYMDIR=${top_srcdir}/symbols \
	  $(GNETLIST) \
	     -L $(top_srcdir)/libgeda/scheme \
	     -L $(top_builddir)/libgeda/scheme \
	     -o $(BUILDDIR)/new_hierarchy2.geda -g geda \
	     $(SRCDIR)/top.sch )
	diff $(SRCDIR)/hierarchy2.geda $(BUILDDIR)/new_hierarchy2.geda;
	rm -f $(BUILDDIR)/gnetlistrc

MOSTLYCLEANFILES = new_* core *.log FILE *.ps *~ gnetlistrc
CLEANFILES = new_* core *.log FILE *.ps *~ gnetlistrc
DISTCLEANFILES = *.log core FILE *~ gnetlistrc
MAINTAINERCLEANFILES = new_* core *.log FILE *.ps *~ Makefile.in gnetlistrc
