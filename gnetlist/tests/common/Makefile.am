input_files = \
	inputs/gafrc \
	inputs/LVDfoo.sch \
	inputs/Simulation.cmd \
	inputs/TwoStageAmp.sch \
	inputs/SlottedOpamps.sch \
	inputs/cascade.sch \
	inputs/multiequal.sch \
	inputs/netattrib.sch \
	inputs/powersupply.sch \
	inputs/singlenet.sch \
	inputs/attribs \
	\
	inputs/models/2N3904.mod \
	inputs/models/openIP_5.cir \
	\
	inputs/sym/LVD.sym \
	inputs/sym/transistor.sym \
	inputs/sym/LM324_slotted-1.sym

TESTS_ENVIRONMENT= PERL=${PERL}

RUN_TESTS= run_tests.sh

check_SCRIPTS= ${RUN_TESTS}

TESTS= ${RUN_TESTS}

EXTRA_DIST= run_backend_tests.sh ${RUN_TESTS} tests.list backends.list \
	always-copy.list $(input_files)
CLEANFILES= *.log

dist-hook: tests.list backends.list
	tests=`cat tests.list | sed -e "s:#.*$$::" | sed -e "s:|.*$$::"`; \
	backends=`cat backends.list`; \
	$(MKDIR_P) $(distdir)/outputs || exit $$?; \
	for backend in $$backends; do \
	  dir=outputs/$$backend; \
	  $(MKDIR_P) $(distdir)/$$dir; \
	  for test in $$tests; do \
	    cp -p $(srcdir)/$$dir/$$test.retcode \
	          $(srcdir)/$$dir/$$test-output.net \
	          $(distdir)/$$dir || exit $$?; \
	  done; \
	done;
