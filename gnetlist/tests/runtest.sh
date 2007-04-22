#!/bin/sh

INPUT=$1
BACKEND=$2
BUILDDIR=$3
SRCDIR=$4
EXTRADIFF=$5

TESTDIR=${BUILDDIR}
export TESTDIR

schbasename=`basename $INPUT .sch`

../src/gnetlist -o ${BUILDDIR}/new_${schbasename}.$BACKEND -g $BACKEND $INPUT 
status=$?

if [ "$status" != 0 ] 
then
	echo FAILED: gnetlist returned non-zero exit status
	exit 1
fi

diff $EXTRADIFF ${SRCDIR}/${schbasename}.$BACKEND ${BUILDDIR}/new_${schbasename}.$BACKEND
status=$?

if [ "$status" != 0 ]
then
	exit 2
fi


