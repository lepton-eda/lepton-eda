## Process this file with automake to produce Makefile.in

SUBDIRS = hierarchy hierarchy2 drc2 common

EXTRA_DIST = runtest.sh \
	     README cascade.sch cascade.cascade \
	     netattrib.geda \
	     netattrib.sch powersupply.PCB powersupply.allegro \
             powersupply.bae powersupply.geda powersupply.maxascii \
             powersupply.pads powersupply.protelII powersupply.sch \
             powersupply.tango singlenet.PCB singlenet.geda singlenet.pads \
	     powersupply.liquidpcb \
             singlenet.pcbpins \
             singlenet.protelII singlenet.sch stack_1.geda \
	     singlenet.liquidpcb \
	     multiequal.sch \
	     multiequal.spice-sdb gnetlistrc.orig

check_SCRIPTS = tests

BUILDDIR=$(builddir)
SRCDIR=$(srcdir)
GNETLIST=$(builddir)/../src/gnetlist

tests:

# cleanup
	rm -f $(BUILDDIR)/new_*
	rm -rf $(BUILDDIR)/devfiles
	mkdir $(BUILDDIR)/devfiles
	cp -f $(srcdir)/gnetlistrc.orig $(builddir)/gnetlistrc

# netattrib gEDA
	$(SRCDIR)/runtest.sh $(SRCDIR)/netattrib.sch geda \
		$(BUILDDIR) $(SRCDIR)

# stack gEDA
	$(SRCDIR)/runtest.sh $(SRCDIR)/../examples/stack_1.sch geda \
		$(BUILDDIR) $(SRCDIR)

# singlenet gEDA
	$(SRCDIR)/runtest.sh $(SRCDIR)/singlenet.sch geda \
		$(BUILDDIR) $(SRCDIR)

# singlenet PCB
	$(SRCDIR)/runtest.sh $(SRCDIR)/singlenet.sch PCB \
		$(BUILDDIR) $(SRCDIR)

# singlenet pcbpins
	$(SRCDIR)/runtest.sh $(SRCDIR)/singlenet.sch pcbpins \
		$(BUILDDIR) $(SRCDIR)

# singlenet PADS
	$(SRCDIR)/runtest.sh $(SRCDIR)/singlenet.sch pads \
		$(BUILDDIR) $(SRCDIR)

# singlenet protelII
	$(SRCDIR)/runtest.sh $(SRCDIR)/singlenet.sch protelII \
		$(BUILDDIR) $(SRCDIR)

# singlenet liquidpcb
	$(SRCDIR)/runtest.sh $(SRCDIR)/singlenet.sch liquidpcb \
		$(BUILDDIR) $(SRCDIR)

# powersupply PCB
	$(SRCDIR)/runtest.sh $(SRCDIR)/powersupply.sch PCB \
		$(BUILDDIR) $(SRCDIR)

# powersupply Allegro
	$(SRCDIR)/runtest.sh $(SRCDIR)/powersupply.sch allegro \
		$(BUILDDIR) $(SRCDIR)

# powersupply bae
	$(SRCDIR)/runtest.sh $(SRCDIR)/powersupply.sch bae \
		$(BUILDDIR) $(SRCDIR)

# powersupply gEDA
	$(SRCDIR)/runtest.sh $(SRCDIR)/powersupply.sch geda \
		$(BUILDDIR) $(SRCDIR)

# powersupply maxascii
	$(SRCDIR)/runtest.sh $(SRCDIR)/powersupply.sch maxascii \
		$(BUILDDIR) $(SRCDIR)

# powersupply pads
	$(SRCDIR)/runtest.sh $(SRCDIR)/powersupply.sch pads \
		$(BUILDDIR) $(SRCDIR)

# powersupply protelII
	$(SRCDIR)/runtest.sh $(SRCDIR)/powersupply.sch protelII \
		$(BUILDDIR) $(SRCDIR)

# powersupply tango
	$(SRCDIR)/runtest.sh $(SRCDIR)/powersupply.sch tango \
		$(BUILDDIR) $(SRCDIR)

# powersupply liquidpcb
	$(SRCDIR)/runtest.sh $(SRCDIR)/powersupply.sch liquidpcb \
		$(BUILDDIR) $(SRCDIR)

# multiple equals in attributes SPICE-SDB
	$(SRCDIR)/runtest.sh $(SRCDIR)/multiequal.sch spice-sdb \
		$(BUILDDIR) $(SRCDIR)

# cascade backend
	$(SRCDIR)/runtest.sh $(SRCDIR)/cascade.sch cascade \
		$(BUILDDIR) $(SRCDIR)


MOSTLYCLEANFILES = new_* gnetlistrc

mostlyclean-local:
	rm -rf devfiles
