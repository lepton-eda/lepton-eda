## Process this file with automake to produce Makefile.in

EXTRA_DIST = README singlenet_1.PCB singlenet_1.geda \
	     stack_1.geda darlington_1.spice \
	     amp_1.spice 7447.vhdl singlenet_1.pads singlenet_1.protel
          
tests:
	rm -f new_*
# gEDA 
	gnetlist -o new_singlenet_1.geda -g geda ../examples/singlenet_1.sch
	diff singlenet_1.geda new_singlenet_1.geda
# gEDA 
	gnetlist -o new_stack_1.geda -g geda \
		../../gschem/examples/stack_1.sch
	diff stack_1.geda new_stack_1.geda
# PCB 
	gnetlist -o new_singlenet_1.PCB -g PCB ../examples/singlenet_1.sch
	diff singlenet_1.PCB new_singlenet_1.PCB
# VHDL
	gnetlist -o new_7447.vhdl -g vhdl \
		../examples/7447.sch
	diff 7447.vhdl new_7447.vhdl
# PADS
	gnetlist -o new_singlenet_1.pads -g pads ../examples/singlenet_1.sch
	diff singlenet_1.pads new_singlenet_1.pads
# protelII
	gnetlist -o new_singlenet_1.protel -g protelII ../examples/singlenet_1.sch
	diff singlenet_1.protel new_singlenet_1.protel

# These tests are internal to Ales and will not work without the schematics
# which I am unable to distribute (due to copyright laws).
tests_ales:
	rm -f new_*
# SPICE 
	gnetlist -o new_amp_1.spice -g spice \
		/home/ahvezda/gEDA/contrib/alesch/amp_1.sch
	diff amp_1.spice new_amp_1.spice
# SPICE 
	gnetlist -o new_darlington_1.spice -g spice \
		/home/ahvezda/gEDA/contrib/alesch/darlington_1.sch
	diff darlington_1.spice new_darlington_1.spice

MOSTLYCLEANFILES = new_* core *.log FILE *.ps *~
CLEANFILES = new_* core *.log FILE *.ps *~
DISTCLEANFILES = new_* core *.log FILE *.ps *~
MAINTAINERCLEANFILES = new_* core *.log FILE *.ps *~



