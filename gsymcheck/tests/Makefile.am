
EXTRA_DIST = buses.sym connections1.sym connections2.sym connections3.sym \
	     correct.sym duplicate_pinnumber.sym duplicate_pinseq.sym \
	     graphical_correct.sym graphical_incorrect.sym \
	     invalid_slotdef1.sym invalid_slotdef2.sym invalid_slotdef3.sym \
	     invalid_slotdef4.sym labelinside.sym lots_of_errors.sym \
	     many_slotdefs.sym missing_device.sym missing_footprint.sym \
             missing_numslots.sym missing_pinlabel.sym missing_pinnumber.sym \
             missing_pinseq.sym missing_slotdef.sym multiple_footprint.sym \
             multiple_pinlabel.sym multiple_pinnumber.sym \
             multiple_pinseq.sym multiple_pintype.sym multiple_refdes.sym \
             nameinside.sym nets.sym noslots.sym old_symbol.sym oldpins.sym \
             oldslot.sym typeinside.sym urefinside.sym zero_pinnumber.sym \
             zero_pinseq.sym zero_slotnum.sym zero_slots.sym \
	     duplicate_net.sym pin_offgrid.sym pintypes.sym\
	     runtest.sh \
	     buses.output connections1.output connections2.output \
	     connections3.output correct.output duplicate_net.output \
	     duplicate_pinnumber.output duplicate_pinseq.output \
	     graphical_correct.output graphical_incorrect.output \
	     invalid_slotdef1.output \
	     invalid_slotdef2.output invalid_slotdef3.output \
	     invalid_slotdef4.output labelinside.output lots_of_errors.output \
	     many_slotdefs.output missing_device.output \
	     missing_footprint.output missing_numslots.output \
	     missing_pinlabel.output missing_pinnumber.output \
	     missing_pinseq.output missing_slotdef.output \
	     multiple_footprint.output multiple_pinlabel.output \
	     multiple_pinnumber.output multiple_pinseq.output \
	     multiple_pintype.output multiple_refdes.output \
	     nameinside.output nets.output noslots.output oldpins.output \
	     oldslot.output old_symbol.output typeinside.output \
	     urefinside.output zero_pinnumber.output zero_pinseq.output \
	     zero_slotnum.output zero_slots.output pin_offgrid.output \
	     pintypes.output

check_SCRIPTS = tests

BUILDDIR=$(top_builddir)/tests
SRCDIR=$(srcdir)

tests:

# Cleanup
	rm -f $(BUILDDIR)/new_*

	for file in $(SRCDIR)/*.sym; do \
	  $(SRCDIR)/runtest.sh $$file $(BUILDDIR) $(SRCDIR); \
          if [ $$? -ne 0 ]; then \
	     echo "$$file FAILED!"; \
             exit 1; \
	   else \
	     echo "$$file passed."; \
	   fi; \
	done

# Cleanup
#	rm -f $(BUILDDIR)/new_*



MOSTLYCLEANFILES = *.log *~ new_*
CLEANFILES = *.log *~ new_*
DISTCLEANFILES = *.log core FILE *~ prototype.bak new_*
MAINTAINERCLEANFILES = system-gsymcheckrc *.log *~ Makefile.in configure
