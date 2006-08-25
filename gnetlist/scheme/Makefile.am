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
           gnet-pads.scm gnetlist.scm gnet-bae.scm gnet-protelII.scm \
           gnet-bom2.scm gnet-gossip.scm gnet-drc.scm gnet-vams.scm \
	   gnet-partslist-common.scm gnet-partslist1.scm \
	   gnet-partslist2.scm gnet-partslist3.scm gnet-maxascii.scm \
	   gnet-switcap.scm gnet-spice-sdb.scm gnet-drc2.scm \
	   gnet-futurenet2.scm gnet-cascade.scm \
	   gnet-redac.scm gnet-systemc.scm gnet-eagle.scm \
	   gnet-pcbpins.scm


EXTRA_DIST = $(DIST_SCM) $(SCM_SRCS)

SCM_SRCS=	gnet-PCBboard.scm.in \
		gnet-gsch2pcb.scm.in

BUILT_SCM=	gnet-PCBboard.scm gnet-gsch2pcb.scm

gnet-PCBboard.scm: $(srcdir)/gnet-PCBboard.scm.in
	sed \
		-e 's;@m4@;${M4};g' \
		-e 's;@pcbm4dir@;${PCBM4DIR};g' \
		-e 's;@pcbconfdir@;${PCBCONFDIR};g' \
		$(srcdir)/gnet-PCBboard.scm.in > $@

gnet-gsch2pcb.scm: $(srcdir)/gnet-gsch2pcb.scm.in
	sed \
		-e 's;@m4@;${M4};g' \
		-e 's;@pcbm4dir@;${PCBM4DIR};g' \
		-e 's;@pcbconfdir@;${PCBCONFDIR};g' \
		$(srcdir)/gnet-gsch2pcb.scm.in > $@

MOSTLYCLEANFILES = *.log *~
CLEANFILES = *.log *~
DISTCLEANFILES = *.log core FILE *~ prototype.bak \
	         gnet-PCBboard.scm gnet-gsch2pcb.scm
MAINTAINERCLEANFILES = *.log *~ Makefile.in configure \
	         gnet-PCBboard.scm gnet-gsch2pcb.scm
