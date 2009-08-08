## Process this file with automake to produce Makefile.in

EXTRA_DIST = duplicated-refdes.sch duplicated-refdes.drc2 \
	     duplicated_slot.sch duplicated_slot.drc2 \
	     gnd-with-nc-directive.sch gnd-with-nc-directive.drc2 \
	     gnd-without-nc-directive.sch gnd-without-nc-directive.drc2 \
	     gnetlistrc.drc2 \
	     net-not-driven.sch net-not-driven.drc2 \
	     net-not-driven-with-dont-check-if-driven-directive.sch \
	     net-not-driven-with-dont-check-if-driven-directive.drc2 \
	     net-not-driven-with-nc-directive.sch \
	     net-not-driven-with-nc-directive.drc2 \
	     net-with-only-one-connection.sch \
	     net-with-only-one-connection.drc2 \
	     numslots-is-not-an-integer.sch \
	     numslots-is-not-an-integer.drc2 \
	     numslots-not-defined.sch \
	     numslots-not-defined.drc2 \
	     output-connected-to-output.sch \
	     output-connected-to-output.drc2 \
	     pin-without-pintype-attrib.sch \
	     pin-without-pintype-attrib.drc2 \
	     refdes-not-numbered.sch \
	     refdes-not-numbered.drc2 \
	     slot-is-not-an-integer.sch \
	     slot-is-not-an-integer.drc2 \
	     slot-not-defined.sch \
	     slot-not-defined.drc2 \
	     slot_out_of_range.sch \
	     slot_out_of_range.drc2 \
	     unconnected-pin.sch \
	     unconnected-pin.drc2 \
	     unconnected-pin-with-drc-directive.sch \
	     unconnected-pin-with-drc-directive.drc2 \
	     unused-slot.sch \
	     unused-slot.drc2


# Temporarily disabled make check, since this is interfering with 
# make distcheck
check_SCRIPTS = tests

BUILDDIR=$(builddir)
SRCDIR=$(srcdir)
GNETLIST=$(builddir)/../../src/gnetlist

gnet_srcdir=$(top_srcdir)/gnetlist
gnet_builddir=$(top_builddir)/gnetlist

.PHONY : tests
tests:
# cleanup
	rm -f $(BUILDDIR)/new_*
	rm -f $(BUILDDIR)/failed.log

# make the tests
	cp $(srcdir)/gnetlistrc.drc2 $(builddir)/gnetlistrc
	failed=0; \
	for file in $(SRCDIR)/*.sch; do \
	  file_basename=`basename $$file .sch`; \
	  echo Checking test in $(SRCDIR)/$$file_basename.sch; \
	  ( TESTDIR=$(SRCDIR) \
	    GEDADATARC=$(top_builddir)/gnetlist/lib \
	    SCMDIR=${top_builddir}/gnetlist/scheme \
	    SYMDIR=${top_srcdir}/symbols \
	    $(GNETLIST) -g drc2 -o $(BUILDDIR)/new_$$file_basename.drc2 \
            $$file ); \
          diff $(SRCDIR)/$$file_basename.drc2 \
                 $(BUILDDIR)/new_$$file_basename.drc2; \
          if [ $$? -ne 0 ]; then \
	     echo "# Test in $(SRCDIR)/$$file_basename.sch FAILED!!" | tee -a $(BUILDDIR)/failed.log ; \
	     echo "diff $(SRCDIR)/$$file_basename.drc2 $(BUILDDIR)/new_$$file_basename.drc2" >> $(BUILDDIR)/failed.log ; \
	     failed=`expr $$failed + 1`; \
	  else \
	     echo "Test in $(SRCDIR)/$$file_basename.sch PASSED."; \
	  fi; \
	done ; \
	if test $$failed -ne 0 ; then \
		echo "FAILED $$failed test(s)" ; \
		cat $(BUILDDIR)/failed.log ; \
		exit 1 ; \
	fi
	rm -f $(BUILDDIR)/gnetlistrc

MOSTLYCLEANFILES = new_* core *.log FILE *.ps *~ gnetlistrc
CLEANFILES = new_* core *.log FILE *.ps *~ gnetlistrc
DISTCLEANFILES = *.log core FILE *~ gnetlistrc
MAINTAINERCLEANFILES = new_* core *.log FILE *.ps *~ Makefile.in gnetlistrc

