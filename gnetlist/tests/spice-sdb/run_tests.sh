#!/bin/sh
#
# This script runs the tests called out for in test.list
#

regen=no

usage() {
cat << EOF

$0 -- Testsuite program for spice-sdb

Usage

  $0 [-h | --help]
  $0 [-r | --regen] [test1 [test2 [....]]]

Options

  -h | --help     Prints this help message and exits.

  -r | --regen    Regenerates the reference files.  If you use
                  this option, YOU MUST HAND VERIFY THE RESULTS
                  BEFORE COMMITTING to the repository.

Description

$0 reads a file, tests.list,  describing tests to run on spice-sdb.
If no specific test is specified on the $0 command line, then all 
tests are run.

Examples

$0
$0 --regen new_test 

EOF
}
while test -n "$1"
do
    case "$1"
    in

    -h|--help)
	usage
	exit 0
	;;

    -r|--regen)
	# regenerate the 'golden' output files.  Use this with caution.
	# In particular, all differences should be noted and understood.
	regen=yes
	shift
	;;

    -*)
	echo "unknown option: $1"
	usage
	exit 1
	;;

    *)
	break
	;;

    esac
done


# make sure we have the right paths when running this from inside the
# source tree and also from outside the source tree.
here=`pwd`
srcdir=${srcdir:-$here}
srcdir=`cd $srcdir && pwd`

GNETLIST=../../../src/gnetlist

rundir=${here}/run

GOLDEN_DIR=${srcdir}/outputs
INPUT_DIR=${srcdir}/inputs

TESTLIST=${srcdir}/tests.list

if test ! -f $TESTLIST ; then
    echo "ERROR: ($0)  Test list $TESTLIST does not exist"
    exit 1
fi

# fail/pass/total counts
fail=0
pass=0
skip=0
tot=0

# here's where we look at the test.list file and extract the names of all
# the tests we want to run.
if test -z "$1" ; then
    all_tests=`awk 'BEGIN{FS="|"} /^#/{next} /^[ \t]*$/{next} {print $1}' $TESTLIST | sed 's; ;;g'`
else
    all_tests=$*
fi
echo All tests = $all_tests

cat << EOF

Starting tests in $here
srcdir:     $srcdir
INPUT_DIR:  ${INPUT_DIR}
GOLDEN_DIR: ${GOLDEN_DIR}
GNETLIST:   ${GNETLIST}
all_tests:

${all_tests}

EOF

# Now run through all tests in test.list, prepare the $rundir,
# then run the tests. 
for t in $all_tests ; do

    # strip any leading garbage 
    t=`echo $t | sed 's;^\*;;g'`

    # figure out what files we need to copy for this test and what
    # arguments to feed gnetlist 
    schematics=`grep "^[ \t]*${t}[ \t]*|" $TESTLIST | awk 'BEGIN{FS="|"} {print $2}'`
    auxfiles=`grep "^[ \t]*${t}[ \t]*|" $TESTLIST | awk 'BEGIN{FS="|"} {print $3}'`
    args=`grep "^[ \t]*${t}[ \t]*|" $TESTLIST | awk 'BEGIN{FS="|"} {print $4}'`
    code=`grep "^[ \t]*${t}[ \t]*|" $TESTLIST | awk 'BEGIN{FS="|"} {print $5}' | sed 's; ;;g'`
    if test "X$code" = "X" ; then
	code=0
    fi
    echo "Schematics to copy   = $schematics"
    echo "Args to copy         = $args"
    echo "Expected return code = \"$code\""


    tot=`expr $tot + 1`

    # create temporary run directory and required subdirs
    if test ! -d $rundir ; then
	mkdir -p $rundir
	mkdir -p $rundir/sym
	mkdir -p $rundir/models
    fi

    # Create the files needed
    # Note that we need to include not only the .sch files,
    # but also the contents of the sym and model directories.
    if test ! -z "$schematics" ; then
	echo "Copying over schematics to run dir"
	for f in $schematics ; do
	    echo "cp ${INPUT_DIR}/${f} ${rundir}/${f}"
	    cp ${INPUT_DIR}/${f} ${rundir}/${f}
	    chmod 644 ${rundir}/${f}
	done
    fi
    if test ! -z "$auxfiles" ; then
	echo "Copying over aux files to run dir"
	for f in $auxfiles ; do
	    echo "cp ${INPUT_DIR}/${f} ${rundir}/${f}"
	    cp ${INPUT_DIR}/${f} ${rundir}/${f}
	    chmod 644 ${rundir}/${f}
	done
    fi

    # run gnetlist -g spice-sdb
    echo "${GNETLIST} -g spice-sdb $args $schematics"
    cd ${rundir} && ${GNETLIST} -g spice-sdb $args $schematics 
    rc=$?
    if test $rc -ne $code ; then
	echo "FAILED:  gnetlist -g spice-sdb returned $rc which did not match the expected $code"
	fail=`expr $fail + 1`
	continue
    fi

    # OK, now check results of run.
    good=1
    bad=0
    soso=0

    ref=${GOLDEN_DIR}/${t}-output.net
    out=${rundir}/output.net
    
    if test "X$regen" = "Xyes" ; then
	cp ${out} ${ref}
	echo "Regenerated ${ref}"
    elif test -f ${ref} ; then

	sed '/gnetlist -g/d' ${ref} > ${out}.tmp1
	sed '/gnetlist -g/d' ${out} > ${out}.tmp2

	if diff -w ${out}.tmp1 ${out}.tmp2 >/dev/null ; then
	    echo "PASS"
	else
	    echo "FAILED:  See diff -w ${ref} ${out}"
	    fail=`expr $fail + 1`
	    good=0
	    bad=1
	fi
    else
	echo "No reference file.  Skipping"
	good=0
	soso=1
    fi

    if test $soso -ne 0 ; then
	good=0
	bad=0
    fi
    pass=`expr $pass + $good`
    fail=`expr $fail + $bad`
    skip=`expr $skip + $soso`

    cd $here
    
    # Delete the run directory in prep for the next test
    rm -fr ${rundir}

done

echo "Passed $pass, failed $fail, skipped $skip out of $tot tests."

rc=0
if test $pass -ne $tot ; then
    rc=`expr $tot - $pass`

fi

exit $rc

