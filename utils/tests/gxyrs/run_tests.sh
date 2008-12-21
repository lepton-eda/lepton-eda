#!/bin/sh

# Copyright (C) 2008 Carlos Nieves Onega
#    adapted from refdes_renum test suite by Dan McMahill.
# Copyright (C) 2008 other contributors
#                        (see ChangeLog or SCM history for details)
 
# This file is part of gxyrs.

# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation, version 2
# of the License.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  
# 02110-1301, USA.

regen=no

usage() {
cat << EOF

$0 -- Testsuite program for gxyrs

Usage

  $0 [-h | --help]
  $0 [-r | --regen] [test1 [test2 [....]]]

Options

  -h | --help     Prints this help message and exits.

  -r | --regen    Regenerates the reference files.  If you use
                  this option, YOU MUST HAND VERIFY THE RESULTS
                  BEFORE COMMITTING to the repository.

Description

$0 reads a file, tests.list,  describing tests to run on geda_filter.
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

top_srcdir=${top_srcdir:-$here/../..}
top_srcdir=`cd $top_srcdir && pwd`
top_builddir=${top_builddir:-$here/../..}
top_builddir=`cd $top_builddir && pwd`
gxyrs_srcdir=`cd $top_srcdir/gxyrs && pwd`

# the perl program
PERL=${PERL:-perl}

rundir=${here}/run

GOLDEN_DIR=${srcdir}/outputs
INPUT_DIR=${srcdir}/inputs


TESTLIST=${srcdir}/tests.list

if test ! -f $TESTLIST ; then
    echo "ERROR: ($0)  Test list $TESTLIST does not exist"
    exit 1
fi

GXYRS_SCRIPT=${top_builddir}/gxyrs/gxyrs
if test ! -f $GXYRS_SCRIPT ; then
    echo "ERROR: ($0)  gxyrs script $GXYRS_SCRIPT does not exist"
    exit 1
fi


# fail/pass/total counts
fail=0
pass=0
skip=0
tot=0

if test -z "$1" ; then
    all_tests=`awk 'BEGIN{FS="|"} /^#/{next} /^[ \t]*$/{next} {print $1}' $TESTLIST | sed 's; ;;g'`
else
    all_tests=$*
fi

cat << EOF

Starting tests in $here
srcdir:     $srcdir
top_srcdir: $top_srcdir
gxyrs srcdir: $gxyrs_srcdir
INPUT_DIR:  ${INPUT_DIR}
GOLDEN_DIR: ${GOLDEN_DIR}
script to test: ${top_srcdir}/gxyrs/gxyrs
all_tests:

${all_tests}

EOF

for t in $all_tests ; do

    # strip any leading garbage
    t=`echo $t | sed 's;^\*;;g'`

    # figure out what files we need to copy for this test and what
    # arguments to feed refdes_renum
    files=`grep "^[ \t]*${t}[ \t]*|" $TESTLIST | awk 'BEGIN{FS="|"} {print $2}'`
    adjust_file=`grep "^[ \t]*${t}[ \t]*|" $TESTLIST | awk 'BEGIN{FS="|"} {print $3}'`
    args=`grep "^[ \t]*${t}[ \t]*|" $TESTLIST | awk 'BEGIN{FS="|"} {print $4}'`
    code=`grep "^[ \t]*${t}[ \t]*|" $TESTLIST | awk 'BEGIN{FS="|"} {print $5}'`
    out_file='out.txt'
    error_file='error.txt'
    if test "X$code" = "X" ; then
	code=0
    fi


    tot=`expr $tot + 1`

    # create temporary run directory
    if test ! -d $rundir ; then
	mkdir -p $rundir
    fi

    # Create the files needed
    if test ! -z "$files" ; then
	for f in $files $adjust_file; do
	    cp ${INPUT_DIR}/${f} ${rundir}
	    chmod 644 ${rundir}/${f}
	done
    fi
    
    # run gxyrs
    #
    
    if test "X$adjust_file" = "X" -o "X$adjust_file" = "X " ; then
	command="cd ${rundir} && ${PERL} -I${gxyrs_srcdir} -w ${GXYRS_SCRIPT} $args $files --output $out_file 2> $error_file"
    else
	command="cd ${rundir} && ${PERL} -I${gxyrs_srcdir} -w ${GXYRS_SCRIPT} $args $files --adjust $adjust_file --output $out_file 2> $error_file"
    fi
    echo "Running test ${t}"
    echo $command
    eval $command
    rc=$?
    if test $rc -ne $code ; then
	echo "FAILED:  gxyrs returned $rc which did not match the expected $code"
	fail=`expr $fail + 1`
	continue
    fi

    good=1
    bad=0
    soso=0
    for f in ${files} ; do
	ref=${GOLDEN_DIR}/${t}-${f}
	out=${rundir}/$out_file
	ref_error=${GOLDEN_DIR}/${t}-${f}-error
	error=${rundir}/$error_file

	if test "X$regen" = "Xyes" ; then
	    cp ${out} ${ref}
	    echo "Regenerated ${ref}"
	    cp ${error} ${ref_error}
	    echo "Regenerated ${ref_error}"
	elif test -f ${ref} ; then
	    if diff -w ${ref} ${out} >/dev/null ; then
		if diff -w ${ref_error} ${error} >/dev/null ; then
		    echo "PASS"
		else
		    echo "FAILED:  See diff -w ${ref_error} ${error}"
		    fail=`expr $fail + 1`
		    bad=1
		    good=0
	    fi
	    else
		echo "FAILED:  See diff -w ${ref} ${out}"
		fail=`expr $fail + 1`
		bad=1
		good=0
	    fi
	else
	    echo "No reference file.  Skipping"
	    good=0
	    soso=1
	fi
    done
    if test $soso -ne 0 ; then
	good=0
	bad=0
    fi
    pass=`expr $pass + $good`
    fail=`expr $fail + $bad`
    skip=`expr $skip + $soso`

    cd $here
    
    # clean up the rundirectory
    rm -fr ${rundir}

done

echo "Passed $pass, failed $fail, skipped $skip out of $tot tests."

rc=0
if test $pass -ne $tot ; then
    rc=`expr $tot - $pass`

fi

exit $rc
