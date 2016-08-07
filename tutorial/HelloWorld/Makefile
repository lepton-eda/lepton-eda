HelloWorld : HelloWorld.cir
	ngspice HelloWorld.cir

HelloWorld.cir : HelloWorld.sch
	gnetlist -g spice-noqsi HelloWorld.sch -o HelloWorld.cir

clean : 
	rm -f HelloWorld.cir *~ \#*
