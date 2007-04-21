#!/bin/sh

INPUT=$1
BACKEND=$2
BUILDDIR=$3
SRCDIR=$4
EXTRADIFF=$5

TESTDIR=${BUILDDIR}
export TESTDIR

schbasename=`basename $INPUT .sch`

gnetlist -o ${BUILDDIR}/new_${schbasename}.$BACKEND -g $BACKEND $INPUT 

diff $EXTRADIFF ${SRCDIR}/${schbasename}.$BACKEND ${BUILDDIR}/new_${schbasename}.$BACKEND

