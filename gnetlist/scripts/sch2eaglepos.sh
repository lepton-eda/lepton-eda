#!/bin/bash
# By Braddock Gaskill (braddock@braddock.com), August 2004.  This
# software is hereby declared to be in the public domain by Braddock
# Gaskill, the author.
FNAME="$1"
if [ -z "$FNAME" ]; then
    echo "$0 <inputfile.sch>"
    echo "This script will read a gschem schematic and attempt to
    extract the relative positions of the components in the schematic,
    and generate corresponding MOVE instructions for Eagle.  You will
    likely have to adjust XOFFSET, YOFFSET, XSCAL, and YSCALE at the
    top of the script to obtain usable positions."
    echo "By Braddock Gaskill (braddock@braddock.com), August 2004"
    exit -1;
fi
XOFFSET=40000
YOFFSET=33000
#XSCALE=10000
#YSCALE=10000
XSCALE=9000
YSCALE=9000

TMP=/tmp/$$
grep -B1 refdes= "$FNAME" |sed 's/=/ /' | cut -d" " -f2,3 |grep -v '^--' >/tmp/$$

3<$TMP
while read -u 3; do
    # the directory on the client to backup
    X=`echo $REPLY | cut -d' ' -f1`
    Y=`echo $REPLY | cut -d' ' -f2`
    read -u 3;
    PART="$REPLY"
    X=`echo "scale=5; ($X - $XOFFSET) / $XSCALE" |bc`
    Y=`echo "scale=5; ($Y - $YOFFSET) / $YSCALE" |bc`
    echo "MOVE '$PART' ($X $Y);"
done
rm "$TMP"
