#!/bin/sh
#
# This script runs the tests called out for in test.list
#

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
        REGEN=1
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

GOLDEN_DIR=${srcdir}/outputs/${backend}

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
    all_tests=`awk '/^#/{next} /^[ \t]*$/{next} {print $0}' $TESTLIST | sed 's; ;;g'`
else
    all_tests=$*
fi
echo All tests = $all_tests

cat << EOF

Starting tests in $here
srcdir:     $srcdir
GOLDEN_DIR: ${GOLDEN_DIR}
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

    refcode=${GOLDEN_DIR}/${t}.retcode

    tot=`expr $tot + 1`

    ref=${GOLDEN_DIR}/${t}-output.net

    REGEN="${REGEN}" debug="${debug}" \
    ${srcdir}/run-test "${t}" "${backend}" "${ref}" "${refcode}"

    case "$?" in
        0) pass=`expr $pass + 1` ;;
        1) fail=`expr $fail + 1` ;;
        77) skip=`expr $skip + 1` ;;
    esac

done

echo "Passed $pass, failed $fail, skipped $skip out of $tot tests."

exit $fail
