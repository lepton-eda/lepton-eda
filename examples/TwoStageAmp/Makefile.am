exampledir = $(docdir)/examples/TwoStageAmp/

nobase_example_DATA = \
	Simulation.cmd TwoStageAmp.sch spice.netlist gafrc
	models/2N3904.mod \
	sym/transistor.sym

EXTRA_DIST = $(nobase_example_DATA)

MOSTLYCLEANFILES = gschem.log *~
CLEANFILES = gschem.log *~
DISTCLEANFILES = *.log core FILE *~ prototype.bak
MAINTAINERCLEANFILES = gschem.log *~ Makefile.in configure

