## Process this file with automake to produce Makefile.in

SUBDIRS = hierarchy hierarchy2 drc2 common

EXTRA_DIST = runtest.sh \
	     7447.vhdl README amp.spice cascade.sch cascade.cascade \
	     darlington.spice netattrib.geda \
	     netattrib.sch powersupply.PCB powersupply.allegro \
             powersupply.bae powersupply.geda powersupply.maxascii \
             powersupply.pads powersupply.protelII powersupply.sch \
             powersupply.tango singlenet.PCB singlenet.geda singlenet.pads \
	     powersupply.liquidpcb \
             singlenet.pcbpins \
             singlenet.protelII singlenet.sch stack_1.geda amp.spice-sdb \
	     singlenet.liquidpcb \
	     darlington.spice-sdb skt.switcap test.ana multiequal.sch \
	     multiequal.spice-sdb gnetlistrc.vhdl gnetlistrc

check_SCRIPTS = tests

BUILDDIR=$(builddir)
SRCDIR=$(srcdir)
GNETLIST=$(builddir)/../src/gnetlist

tests:

# cleanup
	rm -f $(BUILDDIR)/new_*
	rm -rf $(BUILDDIR)/devfiles
	mkdir $(BUILDDIR)/devfiles
	cp $(srcdir)/gnetlistrc $(builddir)

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

# Cleanup
	rm -f $(BUILDDIR)/new_*
	rm -rf $(BUILDDIR)/devfiles
	rm -f $(builddir)/gnetlistrc


# These tests are internal to Ales and will not work without the schematics
# which I am unable to distribute (due to copyright laws).
tests_ales:

# cleanup
	rm -f $(BUILDDIR)/new_*
	rm -rf $(BUILDDIR)/devfiles
	mkdir $(BUILDDIR)/devfiles

# amp SPICE 
	$(SRCDIR)/runtest.sh $(HOME)/gEDA/alesch/amp.sch spice \
		$(BUILDDIR) $(SRCDIR)

# darlington SPICE 
	$(SRCDIR)/runtest.sh $(HOME)/gEDA/alesch/darlington.sch spice \
		$(BUILDDIR) $(SRCDIR)

# amp SPICE SDB
	$(SRCDIR)/runtest.sh $(HOME)/gEDA/alesch/amp.sch spice-sdb \
		$(BUILDDIR) $(SRCDIR) 

# darlington SPICE SDB
	$(SRCDIR)/runtest.sh $(HOME)/gEDA/alesch/darlington.sch spice-sdb \
		$(BUILDDIR) $(SRCDIR)

# switcap (does not work with make distcheck due to local test.ana file)
	$(GNETLIST) -o $(BUILDDIR)/new_skt.switcap -g switcap \
		$(SRCDIR)/../examples/switcap/ckt.sch \
		$(SRCDIR)/../examples/switcap/clocks.sch \
		$(SRCDIR)/../examples/switcap/analysis.sch
	diff $(SRCDIR)/skt.switcap $(BUILDDIR)/new_skt.switcap
	diff $(SRCDIR)/../examples/switcap/example.scn $(SRCDIR)/new_skt.switcap

# 7447 VHDL
	cp -f $(SRCDIR)/gnetlistrc.vhdl $(BUILDDIR)/gnetlistrc
	$(SRCDIR)/runtest.sh $(SRCDIR)/../examples/7447.sch vhdl \
		$(BUILDDIR) $(SRCDIR)
	rm -f $(BUILDDIR)/gnetlistrc

# cleanup
	rm -f $(BUILDDIR)/new_*
	rm -rf $(BUILDDIR)/devfiles

MOSTLYCLEANFILES = new_* core *.log FILE *.ps *~
CLEANFILES = new_* core *.log FILE *.ps *~
DISTCLEANFILES = *.log core FILE *~ prototype.bak gnetlistrc
MAINTAINERCLEANFILES = new_* core *.log FILE *.ps *~ Makefile.in configure



