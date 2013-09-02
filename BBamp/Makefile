GNET=gnetlist -g spice-noqsi
SPICE=ngspice

%.cir : %.sch
	$(GNET) $< -o $@

.PHONY : simulation

simulation : AC.cir BBamp.cir
	$(SPICE) AC.cir transistors.lib

clean : 
	rm -f AC.cir BBamp.cir \#* *~
