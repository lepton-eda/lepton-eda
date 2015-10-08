#!/bin/sh
#
# This script runs the tests called out for in test.list
#

regen=no

usage() {
cat << EOF

$0 -- Testsuite program for various backends

Usage

  $0 [-h | --help]
  $0 [-d | --debug] [-r | --regen] <backend> [test1 [test2 [....]]]

Options

  -h | --help     Prints this help message and exits.

  -r | --regen    Regenerates the reference files.  If you use
                  this option, YOU MUST HAND VERIFY THE RESULTS
                  BEFORE COMMITTING to the repository.

Description

$0 reads a file, tests.list,  describing tests to run on <backend>.
If no specific test is specified on the $0 command line, then all
tests are run.

Examples

$0 spice-sdb
$0 --regen new_test spice-sdb

EOF
}
debug=no
while test -n "$1"
do
    case "$1"
    in

    -d|--debug)
        debug=yes
        shift
        ;;

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

if [ -f $1 ]; then
  usage
  exit 1
fi

backend=$1
shift

echo "********USING BACKEND ${backend}*********"

# make sure we have the right paths when running this from inside the
# source tree and also from outside the source tree.
here=`pwd`
srcdir=${srcdir:-$here}
srcdir=`cd $srcdir && pwd`
builddir="${here}"

GNETLIST="${builddir}/../../src/gnetlist"
GEDADATA="${srcdir}/../.." # HACKHACKHACK
GEDADATARC="${builddir}/../../lib"
SCMDIR="${builddir}/../../scheme"
GEDASCMDIR="${srcdir}/../../../libgeda/scheme"
GEDABUILTSCMDIR="${builddir}/../../../libgeda/scheme"
SYMDIR="${srcdir}/../../../symbols"
export GEDADATA
export GEDADATARC
export SCMDIR
export GEDASCMDIR
export GEDABUILTSCMDIR
export SYMDIR

rundir=${here}/run

GOLDEN_DIR=${srcdir}/outputs/${backend}
INPUT_DIR=${srcdir}/inputs

TESTLIST=${srcdir}/tests.list
ALWAYSCOPYLIST=${srcdir}/always-copy.list

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

# here's where we look at always-copy.list file and extract the names of all
# the extra files we want to copy for every test
always_copy=`cat $ALWAYSCOPYLIST | sed '/^#/d'`

cat << EOF

Starting tests in $here
srcdir:     $srcdir
INPUT_DIR:  ${INPUT_DIR}
GOLDEN_DIR: ${GOLDEN_DIR}
GNETLIST:   ${GNETLIST}
GEDADATA:   ${GEDATADA}
GEDADATARC: ${GEDATADARC}
SCMDIR:     ${SCMDIR}
GEDASCMDIR: ${GEDASCMDIR}
GEDABUILTSCMDIR: ${GEDABUILTSCMDIR}
SYMDIR:     ${SYMDIR}
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
    condition=`grep "^[ \t]*${t}[ \t]*|" $TESTLIST | awk 'BEGIN{FS="|"} {print $5}' | sed 's; ;;g'`

    refcode=${GOLDEN_DIR}/${t}.retcode
    if test -f $refcode; then
        code=`grep -v "^#" $refcode | sed 's; ;;g;'`
    else
        code=0
    fi
    if test "X$code" = "X" ; then
        code=0
    fi

    echo "Schematics to copy   = $schematics"
    echo "Args to copy         = $args"
    echo "Always copying       = $always_copy"
    echo "Expected return code = \"$code\""
    if test "X$condition" != "X" ; then
        eval "ctest=\`echo \$$condition\`"
        if test X$ctest = "Xyes" ; then
            echo "Running test because $condition = yes"
        else
            echo "Skipping test because $condition = $ctest"
            continue
        fi
    fi

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
    if test ! -z "$always_copy" ; then
        echo "Copying over always copied files to run dir"
        for f in $always_copy ; do
            echo "cp ${INPUT_DIR}/${f} ${rundir}/${f}"
            cp ${INPUT_DIR}/${f} ${rundir}/${f}
            chmod 644 ${rundir}/${f}
        done
    fi

    # run gnetlist -g $backend
    echo "${GNETLIST} -g $backend $args $schematics"
    cd ${rundir} && ${GNETLIST} -g $backend $args $schematics
    rc1=$?
    echo "${GNETLIST} -g $backend -o - $args $schematics > stdout.net"
    ${GNETLIST} -g $backend -o - $args $schematics > stdout.net
    rc2=$?
    echo "${GNETLIST} -g $backend -v -o verbose.net $args $schematics"
    ${GNETLIST} -g $backend -v -o verbose.net $args $schematics
    rc3=$?


    # OK, now check results of run.
    good=0
    bad=0
    soso=0

    ref=${GOLDEN_DIR}/${t}-output.net
    out=${rundir}/output.net
    std=${rundir}/stdout.net
    vrb=${rundir}/verbose.net

    # Hack to help with vams backend
    if [ -f ${rundir}/default_entity_arc.net ]; then
      mv ${rundir}/default_entity_arc.net $out
      # vams intentionally outputs data into several files, so checking it with
      # the option '-o verbose.net' is nonsense
      cp $out $vrb
    fi

    if test "X$regen" = "Xyes" ; then

        # Copy output on top of golden output, accounting for the case
        # that no output file was generated.
        if test -f ${out} ; then
            cp ${out} ${ref}
        else
            rm ${ref}
        fi

        echo "$rc1" > $refcode
        echo "Regenerated ${ref}"
        good=1
    elif test $rc1 -ne $code ; then
        echo "FAILED:  gnetlist -g $backend returned $rc1 which did not match the expected $code"
        bad=1
    elif test $rc2 -ne $code ; then
        echo "FAILED:  gnetlist -g $backend -o - returned $rc2 which did not match the expected $code"
        bad=1
    elif test $rc3 -ne $code ; then
        echo "FAILED:  gnetlist -g $backend -v returned $rc3 which did not match the expected $code"
        bad=1
    elif test -f ${ref} ; then

        sed '/gnetlist -g/d' ${ref} > ${out}.tmp1
        sed '/gnetlist -g/d' ${out} > ${out}.tmp2
        sed '/gnetlist -g/d' ${std} > ${out}.tmp3
        sed '/gnetlist -g/d' ${vrb} > ${out}.tmp4

        # Hack to help with allegro backend
        # Device files are ignored as yet
        if test "X$backend" = "Xallegro" ; then
            sed '/gnetlist -g/d' ${std} | sed '/^\$END$/ q' > ${out}.tmp3
        fi

        if ! diff -w ${out}.tmp1 ${out}.tmp2 >/dev/null; then
            echo "FAILED: Wrong plain output. See diff -w ${ref} ${out}"
            bad=1
        elif ! diff -w ${out}.tmp1 ${out}.tmp3 >/dev/null; then
            echo "FAILED: Wrong stdout output. See diff -w ${ref} ${std}"
            bad=1
        elif ! diff -w ${out}.tmp1 ${out}.tmp4 >/dev/null; then
            echo "FAILED: Wrong verbose output. See diff -w ${ref} ${vrb}"
            bad=1
        else
            echo "PASS"
            good=1
        fi
    elif test ! -f $out ; then
        # No output file, but this is expected since there was no reference file
        echo "PASS"
        good=1
    else
        echo "No reference file.  Skipping"
        soso=1
    fi

    pass=`expr $pass + $good`
    fail=`expr $fail + $bad`
    skip=`expr $skip + $soso`

    cd $here

    # Delete the run directory in prep for the next test
    test "$debug" = "no" && rm -fr ${rundir}

done

echo "Passed $pass, failed $fail, skipped $skip out of $tot tests."

rc=0
if test $pass -ne $tot ; then
    rc=`expr $tot - $pass`

fi

exit $rc
