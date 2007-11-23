#!/bin/sh
# Run this to generate all the initial makefiles, etc.

# This file came from glade-2.0.0 with modifications for gEDA/gaf.  
# Ales Hvezda 11/09/2003 

srcdir=`dirname $0`
test -z "$srcdir" && srcdir=.
configure_script=configure.ac

# Automake required version
AM_1=1  # Major number
AM_2=6
AM_3=0  # Minor number

# Possible names for libtool/libtoolize
libtoolize_candidates="libtoolize glibtoolize"

DIE=0

(test -f $srcdir/$configure_script) || {
    echo -n "**Error**: Directory [ $srcdir ] does not look like the"
    echo " top-level package directory"
    exit 1
}

(autoconf --version) < /dev/null > /dev/null 2>&1 || {
  echo
  echo "**Error**: You must have \`autoconf' installed."
  echo "Download the appropriate package for your distribution,"
  echo "or get the source tarball at ftp://ftp.gnu.org/pub/gnu/"
  DIE=1
}

(grep "^AM_PROG_LIBTOOL" $srcdir/$configure_script >/dev/null) && {
  LIBTOOLIZE=`which $libtoolize_candidates 2>/dev/null | head -n1`
  (! test -z "$LIBTOOLIZE") || {
    echo
    echo "**Error**: You must have \`libtool' installed."
    echo "You can get it from: ftp://ftp.gnu.org/pub/gnu/"
    DIE=1
  }
}

(grep "^AM_GNU_GETTEXT" $srcdir/$configure_script >/dev/null) && {
  (grep "sed.*POTFILES" $srcdir/$configure_script) > /dev/null || \
  (autopoint --version) < /dev/null > /dev/null 2>&1 || {
    echo
    echo "**Error**: You must have \`gettext' installed."
    echo "You can get it from: http://www.gnu.org/software/gettext"
    DIE=1
  }
}

(automake --version) < /dev/null > /dev/null 2>&1 || {
  echo
  echo "**Error**: You must have \`automake' installed."
  echo "You can get it from: ftp://ftp.gnu.org/pub/gnu/"
  DIE=1
  NO_AUTOMAKE=yes
}

# check automake version. Test came from gpsd version 2.34. cnieves 2007-02-10
if [ -z "$NO_AUTOMAKE" ]; then
  AM_VERSION=`automake --version | sed -n -e 's#[^0-9]* \([0-9]*\)\.\([0-9]*\)\.*\([0-9]*\).*$#\1 \2 \3#p'`
  AM_V1=`echo $AM_VERSION | awk '{print $1}'`
  AM_V2=`echo $AM_VERSION | awk '{print $2}'`
  AM_V3=`echo $AM_VERSION | awk '{print $3}'`

  if [ "$AM_1" -gt "$AM_V1" ]; then
    AM_ERROR=1 
  else
    if [ "$AM_1" -eq "$AM_V1" ]; then
      if [ "$AM_2" -gt "$AM_V2" ]; then
        AM_ERROR=1 
      else
        if [ "$AM_2" -eq "$AM_V2" ]; then
          if [ -n "$AM_V3" -a "$AM_3" -gt "$AM_V3" ]; then
            AM_ERROR=1 
          fi
        fi
      fi
    fi
  fi

  if [ -n "$AM_ERROR" ]; then
    echo
    echo "**Error**: Found automake version $AM_V1.$AM_V2.$AM_V3"
    echo "You must have \`automake' version $AM_1.$AM_2.$AM_3 or greater installed."
    echo "You can get it from: ftp://ftp.gnu.org/pub/gnu/"
    DIE=1
  fi 
fi



# if no automake, don't bother testing for aclocal
test -n "$NO_AUTOMAKE" || (aclocal --version) < /dev/null > /dev/null 2>&1 || {
  echo
  echo "**Error**: Missing \`aclocal'.  The version of \`automake'"
  echo "installed doesn't appear recent enough."
  echo "You can get automake from ftp://ftp.gnu.org/pub/gnu/"
  DIE=1
}

if test "$DIE" -eq 1; then
  exit 1
fi

# Don't run configure automatically.
#if test -z "$*"; then
#  echo "**Warning**: I am going to run \`configure' with no arguments."
#  echo "If you wish to pass any to it, please specify them on the"
#  echo \`$0\'" command line."
#  echo
#fi

case $CC in
xlc )
  am_opt=--include-deps;;
esac

for coin in $srcdir/$configure_script
do 
  dr=`dirname $coin`
  if test -f $dr/NO-AUTO-GEN; then
    echo skipping $dr -- flagged as no auto-gen
  else
    echo processing $dr
    ( cd $dr

      aclocalinclude="$ACLOCAL_FLAGS"

      if grep "^AM_GNU_GETTEXT" $configure_script >/dev/null; then
	echo "autogen.sh running: autopoint ..." 
	echo "no" | autopoint --force 
	#echo "Creating $dr/po/Makevars ..."
        #mv -f $dr/po/Makevars.template $dr/po/Makevars
      fi
      if grep "^AM_PROG_LIBTOOL" $configure_script >/dev/null; then
	echo "autogen.sh running: libtoolize ..."
	$LIBTOOLIZE --force --copy
      fi
      echo "autogen.sh running: aclocal $aclocalinclude ..."
      aclocal $aclocalinclude
      if grep "^AM_CONFIG_HEADER" $configure_script >/dev/null; then
	echo "autogen.sh running: autoheader ..."
	autoheader
      fi
      echo "autogen.sh running: automake $am_opt ..."
      automake --copy --add-missing --gnu $am_opt
      echo "autogen.sh running: autoconf ..."
      autoconf 
    )
  fi
done


# Don't run configure.
#conf_flags="--enable-maintainer-mode"
#
#if test x$NOCONFIGURE = x; then
#  echo Running $srcdir/configure $conf_flags "$@" ...
#  $srcdir/configure $conf_flags "$@" \
#  && echo Now type \`make\' to compile. || exit 1
#else
#  echo Skipping configure process.
#fi
