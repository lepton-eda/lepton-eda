
## Process this file with automake to produce Makefile.in

sch_files = \
	sch/frg-vco.sch

sym_files = \
	sym/varactor-1.sym

model_files =
	models/D1N4148.sp\
	models/MV104.sp\
	bc337_25.sp

SUBDIRS =

EXTRA_DIST = README gafrc wiki ngspice.cmd\
	$(sch_files) $(sym_files) $(ref_files) $(model_files)

MOSTLYCLEANFILES =  core *.log FILE *.ps *~
CLEANFILES = core *.log FILE *.ps *~
DISTCLEANFILES = *.log core FILE *~
MAINTAINERCLEANFILES = core *.log FILE *.ps *~ Makefile.in

