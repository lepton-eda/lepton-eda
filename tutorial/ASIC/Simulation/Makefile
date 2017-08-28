GNET=gnetlist -L ../../.. -g spice-noqsi
#
# Alternate for using the spice-sdb back end instead
# GNET=gnetlist -g spice-sdb
SPICE=ngspice

IMPORTS=DISCRI.sch Comparator.sch
CIRCUITS=DISCRI.cir Comparator.cir DISCRItest.cir
MODELS=../Models/submicron.inc ../Models/openIP.inc

%.cir : %.sch
	$(GNET) $< -o $@

.PHONY : simulation

simulation : $(CIRCUITS) control.cir
	$(SPICE) $(CIRCUITS) control.cir $(MODELS) 

DISCRI.sch : ../Schematic/DISCRI.sch
	cp $< $@

Comparator.sch : ../Schematic/Comparator.sch
	cp $< $@

clean : 
	rm -f $(IMPORTS) $(CIRCUITS) \#* *~
