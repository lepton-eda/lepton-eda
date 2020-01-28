## Process this file with automake to produce Makefile.in

sch_files = \
	sch/frg_band_pass.sch

sym_files = 

ref_files = \
	frg_7700_ref/yaesu-frg-7700-schematic.pdf\
	frg_7700_ref/frg7700.pdf

model_files =
	models/D1N4148.sp

SUBDIRS = 

EXTRA_DIST = README gafrc ngspice.cmd wiki\
	$(sch_files) $(sym_files) $(ref_files) $(model_files)

MOSTLYCLEANFILES =  core *.log FILE *.ps *~
CLEANFILES = core *.log FILE *.ps *~
DISTCLEANFILES = *.log core FILE *~
MAINTAINERCLEANFILES = core *.log FILE *.ps *~ Makefile.in

