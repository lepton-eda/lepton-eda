#!/bin/sh

INPUT=$1
BUILDDIR=$2
SRCDIR=$3
rundir=${BUILDDIR}/run

# create temporary run directory and required subdirs
mkdir -m 0700 -p ${rundir}
rc=$?
if test $rc -ne 0 ; then
	echo "Failed to create directory ${rundir} with appropriate permissions"
	echo "mkdir returned $rc"
	exit 1
fi

TESTDIR=${BUILDDIR}
export TESTDIR

symbasename=`basename $INPUT .sym`

in="${INPUT}"
ref="${SRCDIR}/${symbasename}.output"
new="${rundir}/new_${symbasename}.output"
tmpfile=${rundir}/tmp$$

${BUILDDIR}/../src/gsymcheck -vv ${in} 1> ${tmpfile} 2> ${rundir}/allerrors.output

cat ${tmpfile} | \
	grep -v "gEDA/gsymcheck version" | \
	grep -v "ABSOLUTELY NO WARRANTY" | \
	grep -v "This is free software" | \
	grep -v "the COPYING file" | \
	grep -v "Checking: " | \
	grep -v '^$' > ${new}
rm -f ${tmpfile}

diff "${ref}" "${new}"
status=$?

rm -rf $rundir

if test $status -ne 0; then
	exit 2
fi

exit 0
