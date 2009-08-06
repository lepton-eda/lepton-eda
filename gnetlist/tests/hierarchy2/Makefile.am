## Process this file with automake to produce Makefile.in

EXTRA_DIST = gafrc.hierarchy2 hierarchy2.geda top.sch under.sch under.sym

# Temporarily disabled make check, since this is interfering with 
# make distcheck
check_SCRIPTS = tests

BUILDDIR=$(top_builddir)/tests/hierarchy2
SRCDIR=$(srcdir)
GNETLIST=$(top_builddir)/src/gnetlist

tests:
	rm -f $(BUILDDIR)/new_*
	cp -f $(SRCDIR)/gafrc.hierarchy2 $(BUILDDIR)/gafrc
	( TESTDIR=$(SRCDIR); export TESTDIR ; SCMDIR=${top_builddir}/scheme ; export SCMDIR ; \
	  echo $$TESTDIR; \
	  $(GNETLIST) \
	     -o $(BUILDDIR)/new_hierarchy2.geda -g geda \
	     $(SRCDIR)/top.sch )
	diff $(SRCDIR)/hierarchy2.geda $(BUILDDIR)/new_hierarchy2.geda;

MOSTLYCLEANFILES = new_* core *.log FILE *.ps *~
CLEANFILES = new_* core *.log FILE *.ps *~
DISTCLEANFILES = *.log core FILE *~ prototype.bak gafrc
MAINTAINERCLEANFILES = new_* core *.log FILE *.ps *~ Makefile.in configure



