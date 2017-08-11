## Process this file with automake to produce Makefile.in

sch_files = \
	sch/verilog_rip_test.sch

sym_files = \
	sym/D_FF.sym 

SUBDIRS = vhdl

EXTRA_DIST = README gafrc dummy_test.v  \
	$(sch_files) $(sym_files)

MOSTLYCLEANFILES =  core *.log FILE *.ps *~
CLEANFILES = core *.log FILE *.ps *~
DISTCLEANFILES = *.log core FILE *~
MAINTAINERCLEANFILES = core *.log FILE *.ps *~ Makefile.in

