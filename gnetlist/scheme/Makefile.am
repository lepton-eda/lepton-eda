## $Id$
##

M4=		@M4@
PCBM4DIR=	@PCBM4DIR@
PCBCONFDIR=	@PCBCONFDIR@

scmdatadir = @GEDADATADIR@/scheme
scmdata_DATA = $(DIST_SCM) $(BUILT_SCM)

DIST_SCM = gnet-PCB.scm gnet-allegro.scm gnet-bom.scm gnet-geda.scm \
	   gnet-spice.scm gnet-tango.scm gnet-verilog.scm \
	   gnet-vhdl.scm gnet-vipec.scm \
           gnet-pads.scm gnetlist.scm gnetlist-post.scm \
           gnet-bae.scm gnet-protelII.scm \
           gnet-bom2.scm gnet-gossip.scm gnet-drc.scm gnet-vams.scm \
	   partslist-common.scm gnet-partslist1.scm \
	   gnet-partslist2.scm gnet-partslist3.scm gnet-maxascii.scm \
	   gnet-switcap.scm gnet-spice-sdb.scm gnet-drc2.scm \
	   gnet-futurenet2.scm gnet-cascade.scm \
	   gnet-redac.scm gnet-systemc.scm gnet-eagle.scm \
	   gnet-pcbpins.scm gnet-calay.scm gnet-osmond.scm \
	   gnet-mathematica.scm gnet-liquidpcb.scm


EXTRA_DIST = $(DIST_SCM) $(SCM_SRCS)

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
CLEANFILES = *.log *~ gnet-gsch2pcb.scm
DISTCLEANFILES = *.log core FILE *~ gnet-gsch2pcb.scm
MAINTAINERCLEANFILES = *.log *~ Makefile.in gnet-gsch2pcb.scm
