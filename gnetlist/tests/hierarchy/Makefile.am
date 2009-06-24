## Process this file with automake to produce Makefile.in

EXTRA_DIST = 1217.geda bottom.sch bottom.sym gnetlistrc.hierarchy gschemrc \
	     gschlasrc hierarchy.geda middle.sch middle.sym rock.sch rock.sym \
	     top.sch


# Temporarily disabled make check, since this is interfering with 
# make distcheck
check_SCRIPTS = tests

BUILDDIR=$(top_builddir)/tests/hierarchy
SRCDIR=$(srcdir)
GNETLIST=$(top_builddir)/src/gnetlist

tests: tests-job tests-clean

tests-job:
	rm -f $(BUILDDIR)/new_*
	cp -f $(SRCDIR)/gnetlistrc.hierarchy $(BUILDDIR)/gnetlistrc
	( TESTDIR=$(SRCDIR); export TESTDIR ; SCMDIR=${top_builddir}/scheme ; export SCMDIR ; \
	  echo $$TESTDIR; \
	  $(GNETLIST) \
	     -o $(BUILDDIR)/new_hierarchy.geda -g geda \
	     $(SRCDIR)/top.sch )
	diff $(SRCDIR)/hierarchy.geda $(BUILDDIR)/new_hierarchy.geda;

tests-clean:
	rm -f $(BUILDDIR)/gnetlistrc

MOSTLYCLEANFILES = new_* core *.log FILE *.ps *~
CLEANFILES = new_* core *.log FILE *.ps *~
DISTCLEANFILES = *.log core FILE *~ prototype.bak
MAINTAINERCLEANFILES = new_* core *.log FILE *.ps *~ Makefile.in configure
