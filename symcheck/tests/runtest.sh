#!/bin/sh

INPUT=$1
rundir=${abs_builddir}/run
SYMCHECK=${abs_top_builddir}/symcheck/scheme/lepton-symcheck
# Hack to enable `make distcheck'

# create temporary run directory and required subdirs
mkdir -m 0700 -p ${rundir}
rc=$?
if test $rc -ne 0 ; then
	echo "Failed to create directory ${rundir} with appropriate permissions"
	echo "mkdir returned $rc"
	exit 1
fi

symbasename=`basename $INPUT .sym`

in="${INPUT}"
ref="${abs_srcdir}/${symbasename}.output"
new="${rundir}/new_${symbasename}.output"
tmpfile=${rundir}/tmp$$

# ERRORLOG is a variable exported in Makefile
cd ${rundir} &&
    GUILE_LOAD_PATH="${abs_top_srcdir}/symcheck/scheme:${abs_top_srcdir}/liblepton/scheme:${abs_top_builddir}/liblepton/scheme" \
                   ${SYMCHECK} -vv ${in} 1> ${tmpfile} 2>> ${ERRORLOG}

cat ${tmpfile} | \
	grep -v "Checking: " | \
	grep -v '^$' > ${new}

diff "${ref}" "${new}"
status=$?

# Don't remove temporaries if something went wrong.
# Just exit with non-zero code.
if test $status -ne 0; then
	exit 2
fi

rm -f ${tmpfile}
rm -rf $rundir

exit 0
