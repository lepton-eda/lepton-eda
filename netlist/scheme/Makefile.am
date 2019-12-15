## $Id$
##
bin_SCRIPTS = lepton-netlist

do_subst = sed -e 's,[@]libdir[@],$(libdir),g' \
	-e 's,[@]GUILE[@],$(GUILE),g' \
	-e 's,[@]ccachedir[@],@LEPTON_SCM_PRECOMPILE_DIR@,g'

lepton-netlist: lepton-netlist.in Makefile
	$(do_subst) < $(srcdir)/$@.in > $@
	chmod +x $@

if INSTALL_COMPAT_SYMLINKS
NAME = lepton-netlist$(EXEEXT)
COMPAT_NAME = gnetlist$(EXEEXT)
install-exec-hook:
	cd $(DESTDIR)$(bindir) && \
	if [ -h $(COMPAT_NAME) ]; then rm -f $(COMPAT_NAME); fi && \
	ln -s $(NAME) $(COMPAT_NAME)
uninstall-hook:
	rm -f $(DESTDIR)$(bindir)/$(COMPAT_NAME)
endif INSTALL_COMPAT_SYMLINKS

M4=		@M4@
PCBM4DIR=	@PCBM4DIR@
PCBCONFDIR=	@PCBCONFDIR@

scmdatadir = @GEDADATADIR@/scheme
nobase_scmdata_DATA = $(DIST_SCM) $(DIST_SCM_BACKENDS) $(DIST_SCM_BACKENDS_COMMON) $(BUILT_SCM)


config-netlist.scm: config-netlist.scm.in
	sed -e 's,[@]backenddir[@],$(scmdatadir)/backend,g' < $(srcdir)/$@.in > $@


DIST_SCM_BACKENDS = \
	backend/gnet-allegro.scm \
	backend/gnet-bae.scm \
	backend/gnet-bom.scm \
	backend/gnet-bom2.scm \
	backend/gnet-calay.scm \
	backend/gnet-cascade.scm \
	backend/gnet-drc.scm \
	backend/gnet-drc2.scm \
	backend/gnet-eagle.scm \
	backend/gnet-ewnet.scm \
	backend/gnet-futurenet2.scm \
	backend/gnet-geda.scm \
	backend/gnet-gossip.scm \
	backend/gnet-liquidpcb.scm \
	backend/gnet-makedepend.scm \
	backend/gnet-mathematica.scm \
	backend/gnet-maxascii.scm \
	backend/gnet-osmond.scm \
	backend/gnet-pads.scm \
	backend/gnet-partslist1.scm \
	backend/gnet-partslist2.scm \
	backend/gnet-partslist3.scm \
	backend/gnet-PCB.scm \
	backend/gnet-pcbpins.scm \
	backend/gnet-protelII.scm \
	backend/gnet-redac.scm \
	backend/gnet-spice-sdb.scm \
	backend/gnet-spice.scm \
	backend/gnet-switcap.scm \
	backend/gnet-systemc.scm \
	backend/gnet-tango.scm \
	backend/gnet-tEDAx.scm \
	backend/gnet-vams.scm \
	backend/gnet-verilog.scm \
	backend/gnet-vhdl.scm \
	backend/gnet-vipec.scm

DIST_SCM_BACKENDS_COMMON = \
	backend/spice/common.scm

DIST_SCM = \
	netlist.scm \
	netlist/attrib/compare.scm \
	netlist/backend-getopt.scm \
	netlist/config.scm \
	netlist/core/gettext.scm \
	netlist/deprecated.scm \
	netlist/error.scm \
	netlist/hierarchy.scm \
	netlist/mode.scm \
	netlist/net.scm \
	netlist/option.scm \
	netlist/package-pin.scm \
	netlist/package.scm \
	netlist/partlist.scm \
	netlist/partlist/common.scm \
	netlist/pin-net.scm \
	netlist/port.scm \
	netlist/rename.scm \
	netlist/repl.scm \
	netlist/schematic-component.scm \
	netlist/schematic-connection.scm \
	netlist/schematic-port.scm \
	netlist/schematic.scm \
	netlist/schematic/toplevel.scm \
	netlist/sort.scm \
	netlist/subschematic.scm \
	netlist/subschematic-connection.scm \
	netlist/traverse.scm \
	netlist/verbose.scm

TESTS = unit-tests/test-netlist-load-path.scm \
	unit-tests/test-component-library-search.scm \
	unit-tests/test-netlist-attrib.scm \
	unit-tests/test-netlist-partlist.scm

TEST_EXTENSIONS = .scm
# $(srcdir) and $(builddir) are added here and not in
# AM_SCM_LOG_FLAGS below because guile must know where to find
# netlist modules before it runs tests
SCM_LOG_DRIVER = $(GUILE) \
	-L $(abs_top_srcdir)/liblepton/scheme \
	-L $(abs_top_builddir)/liblepton/scheme \
	-L $(abs_top_srcdir)/netlist/scheme \
	-L $(abs_top_builddir)/netlist/scheme \
	-L $(abs_top_srcdir)/symcheck/scheme \
	-L $(abs_top_builddir)/symcheck/scheme \
	--no-auto-compile -e main/with-toplevel -s unit-test.scm

AM_SCM_LOG_FLAGS = --
AM_TESTS_ENVIRONMENT = GUILE_AUTO_COMPILE=0

dist_noinst_DATA = unit-test.scm $(TESTS)

EXTRA_DIST = $(DIST_SCM) $(DIST_SCM_BACKENDS) $(DIST_SCM_BACKENDS_COMMON) $(SCM_SRCS) lepton-netlist.in config-netlist.scm.in

SCM_SRCS=	backend/gnet-gsch2pcb.scm.in

BUILT_SCM=	backend/gnet-gsch2pcb.scm

SETVBUF_MODE = `$(GUILE) -c '(display (if (string= (minor-version) "0") "_IONBF" (quote (quote none))))'`

backend/gnet-gsch2pcb.scm: $(srcdir)/backend/gnet-gsch2pcb.scm.in
	if test "$(srcdir)" != "@builddir@" ; then \
		echo "creating directories" ; \
		$(MKDIR_P) @builddir@/backend ; \
	else \
		echo "source directory = build directory" ; \
	fi
	sed \
		-e 's;@m4@;${M4};g' \
		-e 's;@pcbm4dir@;${PCBM4DIR};g' \
		-e "s;@mode@;${SETVBUF_MODE};g" \
		$(srcdir)/backend/gnet-gsch2pcb.scm.in > $@

all-local:
	if test "$(srcdir)" != "@builddir@" ; then \
		echo "copying scheme files" ; \
		cp -f $(srcdir)/*.scm @builddir@/ ; \
	else \
		echo "source directory = build directory" ; \
	fi

clean-local:
	if test "$(srcdir)" != "@builddir@" ; then \
		echo "cleaning scheme files" ; \
		rm -f @builddir@/*.scm ; \
	else \
		echo "source directory = build directory" ; \
	fi

MOSTLYCLEANFILES = *.log *~
CLEANFILES = *.log *~ backend/gnet-gsch2pcb.scm $(bin_SCRIPTS) config-netlist.scm
DISTCLEANFILES = *.log core FILE *~ backend/gnet-gsch2pcb.scm config-netlist.scm
MAINTAINERCLEANFILES = *.log *~ Makefile.in backend/gnet-gsch2pcb.scm config-netlist.scm
