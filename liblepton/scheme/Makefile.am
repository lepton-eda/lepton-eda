scmdatadir = $(LEPTONDATADIR)/scheme
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
	lepton/legacy-config.scm \
	netlist.scm \
	netlist/attrib/compare.scm \
	netlist/attrib/refdes.scm \
	netlist/backend-getopt.scm \
	netlist/config.scm \
	netlist/deprecated.scm \
	netlist/duplicate.scm \
	netlist/error.scm \
	netlist/hierarchy.scm \
	netlist/mode.scm \
	netlist/net.scm \
	netlist/option.scm \
	netlist/package-pin.scm \
	netlist/package.scm \
	netlist/partlist.scm \
	netlist/partlist/common.scm \
	netlist/port.scm \
	netlist/schematic-component.scm \
	netlist/schematic-connection.scm \
	netlist/schematic-port.scm \
	netlist/schematic.scm \
	netlist/schematic/toplevel.scm \
	netlist/subschematic.scm \
	netlist/subschematic-connection.scm \
	netlist/verbose.scm \
	symbol/blame.scm \
	symbol/check.scm \
	symbol/check/alignment.scm \
	symbol/check/arc.scm \
	symbol/check/attrib.scm \
	symbol/check/box.scm \
	symbol/check/bus.scm \
	symbol/check/circle.scm \
	symbol/check/component.scm \
	symbol/check/connection.scm \
	symbol/check/duplicate.scm \
	symbol/check/entity-pin.scm \
	symbol/check/forbidden.scm \
	symbol/check/line.scm \
	symbol/check/log.scm \
	symbol/check/net.scm \
	symbol/check/net-attrib.scm \
	symbol/check/obsolete.scm \
	symbol/check/path.scm \
	symbol/check/picture.scm \
	symbol/check/pin.scm \
	symbol/check/pin-attrib.scm \
	symbol/check/primitive.scm \
	symbol/check/slot.scm \
	symbol/check/text.scm \
	symcheck/check.scm \
	symcheck/option.scm \
	symcheck/report.scm

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

TESTS = \
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
