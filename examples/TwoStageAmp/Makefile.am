exampledir = $(docdir)/examples/TwoStageAmp/

nobase_example_DATA = \
	Simulation.cmd \
	Simulation.batch.cmd \
	TwoStageAmp.sch \
	spice.netlist \
	gafrc \
	models/2N3904.mod \
	sym/transistor.sym \
	sym/capacitor-1.sym \
	sym/gnd-1.sym \
	sym/resistor-1.sym \
	sym/spice-directive-1.sym \
	sym/spice-include-1.sym \
	sym/spice-model-1.sym \
	sym/title-B.sym \
	sym/vcc-1.sym \
	sym/vdc-1.sym \
	sym/vsin-1.sym

EXTRA_DIST = $(nobase_example_DATA)

MOSTLYCLEANFILES = gschem.log *~
CLEANFILES = gschem.log *~
DISTCLEANFILES = *.log core FILE *~
MAINTAINERCLEANFILES = gschem.log *~ Makefile.in

