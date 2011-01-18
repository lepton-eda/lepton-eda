#!/bin/sh
#
# This script runs run_backend_tests.sh on each backend listed in backends.list
#

regen=no

usage() {
cat << EOF

$0 -- Testsuite program for various backends

Usage

  $0 [-h | --help]

Options

  -h | --help     Prints this help message and exits.
  -r | --regen    Regenerates the reference files.  If you use
                  this option, YOU MUST HAND VERIFY THE RESULTS
                  BEFORE COMMITTING to the repository.

Description

$0 reads a file, backends.list,  describing backends to run tests on.
For each backend, run_backend_tests.sh is run with that backend name
as a parameter.

Examples

$0
$0 --regen

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

BACKENDSLIST=${srcdir}/backends.list

if test ! -f $BACKENDSLIST ; then
    echo "ERROR: ($0)  Test list $BACKENDSLIST does not exist"
    exit 1
fi

# fail/pass/total counts
fail=0
pass=0
tot=0
all_tests=`cat $BACKENDSLIST | sed /^#/d`
echo All backends = $all_tests

# Now run through all backends in backends.list, calling run_backend_tests.sh
for backend in $all_tests ; do

    tot=`expr $tot + 1`

    if test "X$regen" = "Xyes" ; then
      echo "Regenerating test results on backend \"$backend\""
      $srcdir/run_backend_tests.sh --regen $backend 2> $backend.stderr.log > $backend.stdout.log
      rc=$?
    else
      echo "Running tests on backend \"$backend\""
      $srcdir/run_backend_tests.sh $backend 2> $backend.stderr.log > $backend.stdout.log
      rc=$?
    fi

    if test $rc -ne 0 ; then
        echo "FAILED:  run_backend_tests.sh returned $rc failures"
        fail=`expr $fail + 1`
        continue
    fi

    pass=`expr $pass + 1`
done

echo "Passed $pass, failed $fail, out of $tot tests."

rc=0
if test $pass -ne $tot ; then
    rc=`expr $tot - $pass`
fi

exit $rc

