#!/bin/sh

DATETAG=`date '+%Y%m%d'`
DISTFILE=gTAG-sch-${DATETAG}

rm -f gTAG-sch-*.tar.gz gTAG.bom gTAG.pcb
lepton-netlist -g bom -o gTAG.bom gTAG.sch
lepton-netlist -g PCB -o gTAG-pcb.net gTAG.sch
cd ..
tar cf gTAG/${DISTFILE}.tar gTAG/*.sym gTAG/*.sch gTAG/*rc gTAG/*.ps gTAG/gTAG.bom gTAG/gTAG-pcb.net  gTAG/README gTAG/ChangeLog-1.0 gTAG/crdist.sh
cd gTAG
gzip ${DISTFILE}.tar
