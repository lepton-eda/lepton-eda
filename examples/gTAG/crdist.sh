#!/bin/sh

DATETAG=`date '+%Y%m%d'`
DISTFILE=gTAG-sch-${DATETAG}

rm -f gTAG-sch-*.tar.gz gTAG.bom gTAG.pcb
gnetlist -g bom -o gTAG.bom gTAG.sch
gnetlist -g PCB -o gTAG-pcb.net gTAG.sch
cd ..
tar cf gTAG/${DISTFILE}.tar gTAG/*.sym gTAG/*.sch gTAG/*rc gTAG/*.ps gTAG/gTAG.bom gTAG/gTAG-pcb.net  gTAG/README gTAG/COPYING gTAG/ChangeLog gTAG/crdist.sh
cd gTAG
gzip ${DISTFILE}.tar
