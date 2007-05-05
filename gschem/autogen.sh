#!/bin/sh
# Run this to generate all the initial makefiles, etc.

# This file came from glade-2.0.0 with modifications for gEDA/gaf.  
# Ales Hvezda 11/09/2003 

srcdir=`dirname $0`
test -z "$srcdir" && srcdir=.
configure_script=configure.ac.in

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
  (libtool --version) < /dev/null > /dev/null 2>&1 || {
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

# Create the configure.ac from the configure.ac.in file.  
# The below line to get the gettext version isn't the most robust construct
# because if gettext changes its version output format, this will break.
installed_gettext_version=`gettext --version | grep gettext | awk '{print $4}'`
cat $configure_script | \
  sed "s/%INSTALLED_GETTEXT_VERSION%/$installed_gettext_version/" > configure.ac
configure_script=configure.ac

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
	if test -z "$NO_LIBTOOLIZE" ; then 
	  echo "autogen.sh running: libtoolize ..."
	  libtoolize --force --copy
	fi
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
