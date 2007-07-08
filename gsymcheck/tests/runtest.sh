#!/bin/sh

INPUT=$1
BACKEND=$2
BUILDDIR=$3
SRCDIR=$4
EXTRADIFF=$5
here=`pwd`
rundir=${here}/run

# create temporary run directory and required subdirs
if [ ! -d $rundir ]
then
   mkdir -p $rundir
fi

TESTDIR=${BUILDDIR}
export TESTDIR

symbasename=`basename $INPUT .sym`

tmpfile=$rundir/tmp$$
../src/gsymcheck -vv $INPUT 1> $tmpfile 2> $rundir/allerrors.output 

cat $tmpfile | \
	grep -v "gEDA/gsymcheck version" | \
	grep -v "ABSOLUTELY NO WARRANTY" | \
	grep -v "This is free software" | \
	grep -v "the COPYING file" | \
	grep -v "Checking: " | \
	grep -v '^$' > $rundir/${BUILDDIR}/new_${symbasename}.output 
rm -f $tmpfile

diff $EXTRADIFF ${BUILDDIR}/${symbasename}.output \
	 $rundir/${BUILDDIR}/new_${symbasename}.output
status=$?

rm -rf $rundir

if [ "$status" != 0 ]
then
	exit 2
fi

exit 0
