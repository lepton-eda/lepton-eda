## Process this file with automake to produce Makefile.in

SUBDIRS = hierarchy drc2

EXTRA_DIST = 7447.vhdl README amp.spice darlington.spice netattrib.geda \
	     netattrib.sch powersupply.PCB powersupply.allegro \
             powersupply.bae powersupply.geda powersupply.maxascii \
             powersupply.pads powersupply.protelII powersupply.sch \
             powersupply.tango singlenet.PCB singlenet.geda singlenet.pads \
             singlenet.protel singlenet.sch stack.geda amp.spice-sdb \
	     darlington.spice-sdb skt.switcap test.ana multiequal.sch \
	     multiequal.spice-sdb gnetlistrc.vhdl

tests:
# cleanup
	rm -f new_*
	rm -rf devfiles
	mkdir devfiles
# netattrib gEDA 
	gnetlist -o new_netattrib.geda -g geda ./netattrib.sch
	diff netattrib.geda new_netattrib.geda
# stack gEDA 
	gnetlist -o new_stack.geda -g geda ../../gschem/examples/stack_1.sch
	diff stack.geda new_stack.geda
# 7447 VHDL
	cp -f ./gnetlistrc.vhdl ./gnetlistrc
	gnetlist -o new_7447.vhdl -g vhdl ../examples/7447.sch
	diff 7447.vhdl new_7447.vhdl
	rm -f ./gnetlistrc
# singlenet gEDA 
	gnetlist -o new_singlenet.geda -g geda ./singlenet.sch
	diff singlenet.geda new_singlenet.geda
# singlenet PCB 
	gnetlist -o new_singlenet.PCB -g PCB ./singlenet.sch
	diff singlenet.PCB new_singlenet.PCB
# singlenet PADS
	gnetlist -o new_singlenet.pads -g pads ./singlenet.sch
	diff singlenet.pads new_singlenet.pads
# singlenet protelII
	gnetlist -o new_singlenet.protel -g protelII ./singlenet.sch
	diff singlenet.protel new_singlenet.protel
# powersupply PCB
	gnetlist -o new_powersupply.PCB -g PCB ./powersupply.sch
	diff powersupply.PCB new_powersupply.PCB
# powersupply Allegro
	gnetlist -o new_powersupply.allegro -g allegro ./powersupply.sch
	diff powersupply.allegro new_powersupply.allegro
# powersupply bae
	gnetlist -o new_powersupply.bae -g bae ./powersupply.sch
	diff powersupply.bae new_powersupply.bae
# powersupply gEDA
	gnetlist -o new_powersupply.geda -g geda ./powersupply.sch
	diff powersupply.geda new_powersupply.geda
# powersupply maxascii
	gnetlist -o new_powersupply.maxascii -g maxascii ./powersupply.sch
	diff powersupply.maxascii new_powersupply.maxascii
# powersupply pads
	gnetlist -o new_powersupply.pads -g pads ./powersupply.sch
	diff powersupply.pads new_powersupply.pads
# powersupply protelII
	gnetlist -o new_powersupply.protelII -g protelII ./powersupply.sch
	diff powersupply.protelII new_powersupply.protelII
# powersupply tango
	gnetlist -o new_powersupply.tango -g tango ./powersupply.sch
	diff powersupply.tango new_powersupply.tango
# switcap 
	gnetlist -o new_skt.switcap -g switcap ../examples/switcap/ckt.sch \
		../examples/switcap/clocks.sch ../examples/switcap/analysis.sch
	diff skt.switcap new_skt.switcap
	diff new_skt.switcap ../examples/switcap/example.scn
# multiple equals in attributes SPICE-SDB 
	gnetlist -o new_multiequal.spice-sdb -g spice-sdb \
		./multiequal.sch
	diff multiequal.spice-sdb new_multiequal.spice-sdb


# These tests are internal to Ales and will not work without the schematics
# which I am unable to distribute (due to copyright laws).
tests_ales:
# cleanup
	rm -f new_*
	rm -rf devfiles
	mkdir devfiles
# amp SPICE 
	gnetlist -o new_amp.spice -g spice \
		/home/ahvezda/gEDA/alesch/amp.sch
	diff amp.spice new_amp.spice
# darlington SPICE 
	gnetlist -o new_darlington.spice -g spice \
		/home/ahvezda/gEDA/alesch/darlington.sch
	diff darlington.spice new_darlington.spice
# amp SPICE SDB
	gnetlist -o new_amp.spice-sdb -g spice-sdb \
		/home/ahvezda/gEDA/alesch/amp.sch
	diff amp.spice-sdb new_amp.spice-sdb
# darlington SPICE SDB
	gnetlist -o new_darlington.spice-sdb -g spice-sdb \
		/home/ahvezda/gEDA/alesch/darlington.sch
	diff darlington.spice-sdb new_darlington.spice-sdb

MOSTLYCLEANFILES = new_* core *.log FILE *.ps *~
CLEANFILES = new_* core *.log FILE *.ps *~
DISTCLEANFILES = *.log core FILE *~ prototype.bak
MAINTAINERCLEANFILES = new_* core *.log FILE *.ps *~ Makefile.in configure



