all : Board.osmond Board.bom.tsv

Board.osmond : Board.sch BBamp.sch
	lepton-netlist -g osmond Board.sch -o Board.osmond

Board.bom.tsv : Board.sch BBamp.sch
	lepton-netlist -g bom Board.sch -o Board.bom.tsv

clean : 
	rm -f Board.osmond Board.bom.tsv \#* *~
