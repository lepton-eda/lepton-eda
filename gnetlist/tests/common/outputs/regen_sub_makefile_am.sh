#!/bin/sh

OUTPUTDIRS=`find . -mindepth 1 -type d`

for DIR in $OUTPUTDIRS; do
  EXTRADIST=`ls $DIR/*.net $DIR/*.retcode | sort`
  echo -n "EXTRA_DIST=" > $DIR/Makefile.am
  for FILE in $EXTRADIST; do
    BASEFILE=`basename $FILE`
    echo -n " \\\\\\n\\t$BASEFILE" >> $DIR/Makefile.am
  done
  echo >> $DIR/Makefile.am
done
