## Process this file with automake to produce Makefile.in

EXTRA_DIST = duplicated-refdes.sch duplicated-refdes.drc2 \
	     duplicated_slot.sch duplicated_slot.drc2 \
	     gnd-with-nc-directive.sch gnd-with-nc-directive.drc2 \
	     gnd-without-nc-directive.sch gnd-without-nc-directive.drc2 \
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

tests:
# cleanup
	rm -f new_*

# make the tests
	for file in *.sch; do \
	  file_basename=`basename $$file .sch`; \
	  echo Checking test in $$file_basename.sch; \
	  gnetlist -g drc2 -o new_$$file_basename.drc2 $$file || true; \
	  test -f $$file_basename.drc2 && \
            diff $$file_basename.drc2 new_$$file_basename.drc2; \
          if [ $$? -ne 0 ]; then \
	     echo "Test in $$file_basename.sch FAILED!!"; \
	     break; \
	  else \
	     echo "Test in $$file_basename.sch PASSED."; \
	  fi; \
	done

MOSTLYCLEANFILES = new_* core *.log FILE *.ps *~
CLEANFILES = new_* core *.log FILE *.ps *~
DISTCLEANFILES = *.log core FILE *~ prototype.bak
MAINTAINERCLEANFILES = new_* core *.log FILE *.ps *~ Makefile.in configure



