## Process this file with automake to produce Makefile.in

EXTRA_DIST = 7447.verilog README singlenet_1.PCB singlenet_1.geda \
	     test_verilog.verilog stack_1.geda darlington_1.spice \
	     amp_1.spice 7447.vhdl
          
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
# Verilog
	gnetlist -o new_test_verilog.verilog -g verilog \
		../examples/test_verilog.sch
	diff test_verilog.verilog new_test_verilog.verilog
# Verilog
	gnetlist -o new_7447.verilog -g verilog \
		../examples/7447.sch
	diff 7447.verilog new_7447.verilog
# VHDL
	gnetlist -o new_7447.vhdl -g vhdl \
		../examples/7447.sch
	diff 7447.vhdl new_7447.vhdl

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



