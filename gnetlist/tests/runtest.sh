#!/bin/sh

INPUT=$1
BACKEND=$2
BUILDDIR=$3
SRCDIR=$4
EXTRADIFF=$5

TESTDIR=${BUILDDIR}
export TESTDIR

schbasename=`basename $INPUT .sch`

SCMDIR=$SRCDIR/../scheme \
SYMDIR=$SRCDIR/../../symbols \
GEDADATARC=$BUILDDIR/../lib \
../src/gnetlist -o ${BUILDDIR}/new_${schbasename}.$BACKEND -g $BACKEND $INPUT 
status=$?

if [ "$status" != 0 ] 
then
	echo FAILED: gnetlist returned non-zero exit status
	exit 1
fi

sed '/gnetlist.*-g/d' ${SRCDIR}/${schbasename}.$BACKEND > \
	${BUILDDIR}/${schbasename}.${BACKEND}.filtered
sed '/gnetlist.*-g/d' ${BUILDDIR}/new_${schbasename}.$BACKEND > \
	${BUILDDIR}/new_${schbasename}.${BACKEND}.filtered
diff $EXTRADIFF ${BUILDDIR}/${schbasename}.${BACKEND}.filtered \
	 ${BUILDDIR}/new_${schbasename}.${BACKEND}.filtered
status=$?

rm ${BUILDDIR}/${schbasename}.${BACKEND}.filtered ${BUILDDIR}/new_${schbasename}.${BACKEND}.filtered
if [ "$status" != 0 ]
then
	exit 2
fi


