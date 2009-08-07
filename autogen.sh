#!/bin/sh
#                                                   -*-Shell-script-*-
# Developer helper script for setting up gEDA build environment
# Copyright (C) 2009  Peter Brett <peter@peter-b.co.uk>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

#####################################################################
# Setup variables
#####################################################################

ac_script=configure.ac
am_version=1.6.0
aclocal_flags="$ACLOCAL_FLAGS -I m4"
tooldir=build-tools
podirs="libgeda/po" #gschem/po gattrib/po

srcdir=`dirname $0`
if test "x$srcdir" = x ; then srcdir=.; fi

script_name=`echo $0 | sed -e's:.*/::'`

#####################################################################
# Define some functions
#####################################################################

# check_dist_file FILENAME
# ------------------------
# Check that a file that should be provided by the tarball or git
# checkout is present.
check_dist_file() {
  printf "checking for $srcdir/$1 ... " >&2
  if test -f "$srcdir/$1" ; then
    echo yes >&2
  else
    echo no >&2
    cat >&2 <<EOF

$script_name: $srcdir/$1 is missing. Check that your source tarball
or git checkout is intact.

EOF
    ! :
  fi
}

# check_tool TOOLS PKG [URL]
# --------------------------
# Check that a build tool is present. TOOLS is a list of candidates to
# search for in the path, and PKG is the package which provides the
# tool. If URL is specified, recommend to the user that he get obtain
# the package there. Prints the location of the tool on standard
# output.
check_tool() {
  for tool in $1; do
    printf "checking for $tool ... " >&2
    found_tool=`which $tool 2> /dev/null` && break
    echo no >&2
  done
  if test "x$found_tool" != x ; then
    echo $found_tool >&2
    echo $found_tool
  else
    echo >&2
    echo "$script_name: You must have $2 installed." >&2
    if test "x$3" != x ; then
      cat >&2 <<EOF

If your operating system distribution doesn't provide a package, you
can get download it from <$3>.

EOF
    fi
    ! : # false
  fi
}

# run_tool TOOL [ARG]...
# ----------------------
# Run TOOL with the given ARGs.
run_tool() {
  echo "$script_name: running $1 ..."
  if "$@"; then
    :
  else
    echo "***Error*** $script_name: $1 failed with exit status $?"
    ! : # false
  fi
}

# autopoint_fix [PO_DIR]...
# ------------------------
# GNU gettext has a tool called autopoint which is used for copying
# the gettext build infrastructure into the package. Unfortunately, it
# only recognizes the top level po directory -- which gEDA doesn't use
# at all. We therefore run autopoint to populate the top-level po
# directory, and then copy the files to the po directories that we
# actually use.
#
# N.b. when this function is called we've cd'd into $srcdir.
autopoint_fix() {
  top_po="po"

  # For safety, refuse to continue if the top level po dir exists.
  if test -d top_po; then
    echo "***Error*** $script_name: $top_po exists. Remove it and re-run $script_name"
    ! : #false
  elif run_tool $AUTOPOINT --force; then
    {
      for d in $podirs; do
        echo "$script_name: copying gettext files to $d ..."
        cp -a $top_po/* $d || break
      done
    } && rm -rf $top_po
  fi
}

#####################################################################
# Do some checks for directories and tools
#####################################################################

check_dist_file $ac_script || die=1
check_dist_file $tooldir/desktop-i18n &&
  DESKTOP_I18N=$tooldir/desktop-i18n || die=1

AUTOCONF=`check_tool autoconf "GNU autoconf" ftp://ftp.gnu.org/pub/gnu/` 2>&1 || die=1

AUTOHEADER=`check_tool autoheader "GNU autoconf" ftp://ftp.gnu.org/pub/gnu/` 2>&1 || die=1

AUTOMAKE=`check_tool automake "GNU automake" ftp://ftp.gnu.org/pub/gnu/` 2>&1 || die=1

ACLOCAL=`check_tool aclocal "GNU automake" ftp://ftp.gnu.org/pub/gnu/` 2>&1 || die=1

LIBTOOLIZE=`check_tool "libtoolize glibtoolize" "GNU libtool" "ftp://ftp.gnu.org/pub/gnu/"` 2>&1 || die=1

AUTOPOINT=`check_tool autopoint "GNU gettext" "http://www.gnu.org/software/gettext"` 2>&1 || die=1

#####################################################################
# Check automake version
#####################################################################

# Exit now if we don't have automake at all
if test "x$AUTOMAKE" = x ; then
  echo "***Error*** $script_name: Some required tools could not be found."
  exit $die
fi

printf "checking for automake >= $am_version ... "
am_have_version=`$AUTOMAKE --version | sed -n -e 's:[^0-9]* \([0-9]*\.[0-9]*\.*[0-9]*\).*$:\1:p'`
echo $am_have_version

need_major=`echo $am_version | awk -F . '{print $1}'`
need_minor=`echo $am_version | awk -F . '{print $2}'`
need_point=`echo $am_version | awk -F . '{print $3}'`

have_major=`echo $am_have_version | awk -F . '{print $1}'`
have_minor=`echo $am_have_version | awk -F . '{print $2}'`
have_point=`echo $am_have_version | awk -F . '{print $3}'`

if test "x$have_point" = x; then have_point="0"; fi

if test $need_major -gt $have_major ||
  test $need_major -eq $have_major -a $need_minor -gt $have_minor ||
  test $need_major -eq $have_major -a $need_minor -eq $have_minor \
       -a $need_point -gt $have_point; then
  cat >&2 <<EOF

You have Automake $am_have_version installed, but Automake $am_version
or later is required.

If your operating system doesn't provide a package, you can download
it from ftp://ftp.gnu.org/pub/gnu/

EOF
  die=1
fi

#####################################################################
# Die if checks failed
#####################################################################

if test "x$die" != x ; then
  echo "***Error*** $script_name: Some required tools could not be found."
  exit $die
fi

#####################################################################
# Run tools
#####################################################################

( cd $srcdir &&
  autopoint_fix &&
  run_tool "$DESKTOP_I18N" --setup &&
  run_tool "$LIBTOOLIZE" --force --copy &&
  run_tool "$ACLOCAL" $aclocal_flags &&
  run_tool "$AUTOHEADER" &&
  run_tool "$AUTOMAKE" --copy --add-missing --gnu &&
  run_tool "$AUTOCONF" )
