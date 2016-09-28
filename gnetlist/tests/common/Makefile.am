input_files = \
	inputs/JD.sch \
	inputs/TwoStageAmp.sch \
	inputs/SlottedOpamps.sch \
	inputs/cascade.sch \
	inputs/multiequal.sch \
	inputs/netattrib.sch \
	inputs/powersupply.sch \
	inputs/singlenet.sch \
	inputs/stack-torture.sch \
	\
	inputs/JD-extras/openIP_5.cir \
	inputs/TwoStageAmp-extras/2N3904.mod \
	inputs/TwoStageAmp-extras/Simulation.cmd \
	\
	inputs/JD-symbols/LVD.sym \
	inputs/SlottedOpamps-symbols/LM324_slotted-1.sym \
	inputs/TwoStageAmp-symbols/transistor.sym

AM_TESTS_ENVIRONMENT= PERL=${PERL}

check-local:
	$(AM_TESTS_ENVIRONMENT) srcdir=$(srcdir) $(SHELL) $(srcdir)/run_tests.sh

EXTRA_DIST= run_backend_tests.sh run_tests.sh run-test \
	tests.list backends.list $(input_files)

dist-hook: tests.list backends.list
	@tests=`cat $(srcdir)/tests.list | sed -e "s:#.*$$::" | sed -e "s:|.*$$::"`; \
	backends=`cat $(srcdir)/backends.list`; \
	$(MKDIR_P) $(distdir)/outputs || exit $$?; \
	for backend in $$backends; do \
	  dir=outputs/$$backend; \
	  $(MKDIR_P) $(distdir)/$$dir; \
	  for test in $$tests; do \
	    if test -f $(srcdir)/$$dir/$$test.retcode ; then \
	      cp -p $(srcdir)/$$dir/$$test.retcode $(distdir)/$$dir \
	        || exit $$?; \
	    fi; \
	    if test -f $(srcdir)/$$dir/$$test-output.net ; then \
	      cp -p $(srcdir)/$$dir/$$test-output.net $(distdir)/$$dir \
	        || exit $$?; \
	    fi; \
	  done; \
	done;

MOSTLYCLEANFILES = *.log
