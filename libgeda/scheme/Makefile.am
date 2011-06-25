
scmdatadir = $(GEDADATADIR)/scheme
nobase_dist_scmdata_DATA = geda.scm color-map.scm \
	geda/object.scm geda/page.scm geda/attrib.scm geda/deprecated.scm

# Unit test support.  The unit tests are run using the geda-batch
# program, with config loading disabled (-q) so that user config
# already on the system can't interfere with the test process.
TESTS_ENVIRONMENT = $(builddir)/../shell/geda-shell -q -L $(srcdir) \
	-c '(use-modules (unit-test)) (load (list-ref (command-line) 1)) (exit (if (tests-passed?) 0 1))'

TESTS = unit-tests/t0001-geda-conf-lib.scm \
	unit-tests/t0100-object-line.scm \
	unit-tests/t0101-object-box.scm \
	unit-tests/t0102-object-circle.scm \
	unit-tests/t0103-object-arc.scm\
	unit-tests/t0104-object-text.scm \
	unit-tests/t0105-object-complex.scm \
	unit-tests/t0106-object-bounds.scm \
	unit-tests/t0107-object-stroke-fill.scm \
	unit-tests/t0108-object-connections.scm \
	unit-tests/t0109-object-copy.scm \
	unit-tests/t0110-object-transform.scm \
	unit-tests/t0200-page.scm \
	unit-tests/t0201-page-dirty.scm \
	unit-tests/t0300-attribute.scm \
	unit-tests/t0301-promotable-attributes.scm \
	unit-tests/t1000-deprecated.scm

XFAIL_TESTS = unit-tests/t0301-promotable-attributes.scm

dist_noinst_DATA = unit-test.scm $(TESTS)

MOSTLYCLEANFILES = *.log *~
CLEANFILES = *.log *~
DISTCLEANFILES = *.log core FILE *~
MAINTAINERCLEANFILES = *.log *~ Makefile.in
