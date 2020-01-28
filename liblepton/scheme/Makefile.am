
scmdatadir = $(GEDADATADIR)/scheme
nobase_dist_scmdata_DATA = \
	geda.scm \
	geda-deprecated-config.scm \
	color-map.scm \
	geda/object.scm \
	geda/page.scm \
	geda/attrib.scm \
	geda/deprecated.scm \
	geda/os.scm \
	geda/config.scm \
	geda/log.scm \
	geda/library.scm \
	lepton/attrib.scm \
	lepton/config.scm \
	lepton/file-system.scm \
	lepton/library.scm \
	lepton/library/component.scm \
	lepton/log.scm \
	lepton/log-rotate.scm \
	lepton/object.scm \
	lepton/os.scm \
	lepton/page.scm \
	lepton/rc.scm \
	lepton/repl.scm \
	lepton/version.scm \
	lepton/legacy-config/keylist.scm \
	lepton/legacy-config.scm
nobase_scmdata_DATA = lepton/core/gettext.scm

check-am: update-cli-tool

# Unit test support.  The unit tests are run using the geda-batch
# program, with config loading disabled (-q) so that user config
# already on the system can't interfere with the test process.
CLI_SHELL = $(top_builddir)/cli/lepton-cli --no-rcfiles shell
TEST_EXTENSIONS = .scm
SCM_LOG_COMPILER = $(CLI_SHELL)
AM_SCM_LOG_FLAGS = -L $(srcdir) -L $(builddir) \
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
	unit-tests/t0111-object-path.scm \
	unit-tests/t0112-object-picture.scm \
	unit-tests/t0113-object-selectable.scm \
	unit-tests/t0114-object-embedded.scm \
	unit-tests/t0200-page.scm \
	unit-tests/t0201-page-dirty.scm \
	unit-tests/t0202-page-string.scm \
	unit-tests/t0203-page-string-syntax.scm \
	unit-tests/t0204-page-parse-ordering.scm \
	unit-tests/t0205-page-parse-unterminated-attribute-list.scm \
	unit-tests/t0206-page-parse-garbage-attribute.scm \
	unit-tests/t0207-page-parse-line-endings.scm \
	unit-tests/t0208-page-parse-embed-no-complex.scm \
	unit-tests/t0300-attribute.scm \
	unit-tests/t0301-promotable-attributes.scm \
	unit-tests/t0400-os.scm \
	unit-tests/t0401-os-expand-env-variables.scm \
	unit-tests/t0402-config.scm \
	unit-tests/t0500-source-library.scm \
	unit-tests/t1000-deprecated.scm

XFAIL_TESTS = \
	unit-tests/t0301-promotable-attributes.scm

dist_noinst_DATA = lepton/core/gettext.scm.in unit-test.scm $(TESTS)

lepton/core/gettext.scm: $(srcdir)/lepton/core/gettext.scm.in Makefile
	@domain=$(LIBLEPTON_GETTEXT_DOMAIN); \
	$(MKDIR_P) lepton/core; \
	sed -e"s:[@]LIBLEPTON_GETTEXT_DOMAIN@:$$domain:" < $(srcdir)/$@.in > $@.new; \
	if cmp $@ $@.new > /dev/null 2>&1; then \
	  rm $@.new; echo "$@ is unchanged"; \
	else \
	  echo "Recreating $@"; mv $@.new $@; \
	fi

update-cli-tool:
	for d in liblepton/src libleptonrenderer cli; do \
	  (cd $(top_builddir)/$$d && $(MAKE) $(AM_MAKEFLAGS) all-am); \
	done

.PHONY: update-cli-tool

CLEANFILES = lepton/core/gettext.scm unit-tests/dummy.sym unit-tests/dummy.xpm
MAINTAINERCLEANFILES = Makefile.in
