## $Id$
##
bin_SCRIPTS = lepton-netlist

do_subst = sed -e 's,[@]libdir[@],$(libdir),g'

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
nobase_scmdata_DATA = $(DIST_SCM) $(BUILT_SCM)

DIST_SCM = gnet-PCB.scm gnet-allegro.scm gnet-bom.scm gnet-ewnet.scm \
	   gnet-geda.scm spice-common.scm \
	   gnet-spice.scm gnet-tango.scm gnet-verilog.scm \
	   gnet-vhdl.scm gnet-vipec.scm \
           gnet-pads.scm gnetlist.scm \
           gnet-bae.scm gnet-protelII.scm \
           gnet-bom2.scm gnet-gossip.scm gnet-drc.scm gnet-vams.scm \
	   gnet-partslist1.scm \
	   gnet-partslist2.scm gnet-partslist3.scm gnet-maxascii.scm \
	   gnet-switcap.scm gnet-spice-sdb.scm gnet-drc2.scm \
	   gnet-futurenet2.scm gnet-cascade.scm \
	   gnet-redac.scm gnet-systemc.scm gnet-eagle.scm \
	   gnet-pcbpins.scm gnet-calay.scm gnet-osmond.scm \
	   gnet-mathematica.scm gnet-liquidpcb.scm \
	   gnet-makedepend.scm \
	   gnetlist/backend-getopt.scm gnetlist/partlist.scm \
	   gnetlist/partlist/common.scm gnetlist/attrib/compare.scm \
	   gnetlist/option.scm gnetlist/sort.scm gnetlist/verbose.scm \
	   gnetlist/package.scm gnetlist/package-pin.scm gnetlist/pin-net.scm \
	   gnetlist/traverse.scm gnetlist/schematic.scm gnetlist/config.scm \
	   gnetlist/core/gettext.scm gnetlist/rename.scm gnetlist/port.scm \
	   gnetlist/hierarchy.scm \
	   gnetlist/net.scm \
	   gnetlist/repl.scm

TESTS = unit-tests/test-gnetlist-partlist.scm \
	unit-tests/test-gnetlist-attrib.scm
TEST_EXTENSIONS = .scm
# $(srcdir) and $(builddir) are added here and not in
# AM_SCM_LOG_FLAGS below because guile must know where to find
# gnetlist modules before it runs tests
SCM_LOG_DRIVER = $(GUILE) -L $(srcdir) -L $(builddir) --no-auto-compile -e main -s unit-test.scm
AM_SCM_LOG_FLAGS = --
AM_TESTS_ENVIRONMENT = GUILE_AUTO_COMPILE=0

dist_noinst_DATA = unit-test.scm $(TESTS)

EXTRA_DIST = $(DIST_SCM) $(SCM_SRCS) lepton-netlist.in

SCM_SRCS=	gnet-gsch2pcb.scm.in

BUILT_SCM=	gnet-gsch2pcb.scm

gnet-gsch2pcb.scm: $(srcdir)/gnet-gsch2pcb.scm.in
	sed \
		-e 's;@m4@;${M4};g' \
		-e 's;@pcbm4dir@;${PCBM4DIR};g' \
		$(srcdir)/gnet-gsch2pcb.scm.in > $@

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
CLEANFILES = *.log *~ gnet-gsch2pcb.scm $(bin_SCRIPTS)
DISTCLEANFILES = *.log core FILE *~ gnet-gsch2pcb.scm
MAINTAINERCLEANFILES = *.log *~ Makefile.in gnet-gsch2pcb.scm
