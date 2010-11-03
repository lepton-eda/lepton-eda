
scmdatadir = $(GEDADATADIR)/scheme
nobase_dist_scmdata_DATA = geda.scm color-map.scm \
	geda/object.scm

# Unit test support.  The unit tests are run using the geda-batch
# program, with config loading disabled (-q) so that user config
# already on the system can't interfere with the test process.
TESTS_ENVIRONMENT = $(builddir)/../shell/geda-shell -q -L $(srcdir) \
	-c '(use-modules (unit-test)) (load (list-ref (command-line) 1)) (exit (if (tests-passed?) 0 1))'

TESTS = unit-tests/t0001-geda-conf-lib.scm \
	unit-tests/t0010-object-line.scm \
	unit-tests/t0011-object-box.scm \
	unit-tests/t0012-object-circle.scm \
	unit-tests/t0013-object-arc.scm\
	unit-tests/t0014-object-text.scm

dist_noinst_DATA = unit-test.scm $(TESTS)

MOSTLYCLEANFILES = *.log *~
CLEANFILES = *.log *~
DISTCLEANFILES = *.log core FILE *~
MAINTAINERCLEANFILES = *.log *~ Makefile.in
