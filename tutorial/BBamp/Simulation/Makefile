GNET=gnetlist -L ../../.. -g spice-noqsi
SPICE=ngspice

%.cir : %.sch
	$(GNET) $< -o $@

.PHONY : simulation

simulation : Test.cir BBamp.cir
	$(SPICE) Test.cir transistors.lib

BBamp.sch : ../Schematic/BBamp.sch
	cp $< $@

clean : 
	rm -f Test.cir BBamp.cir BBamp.sch \#* *~
