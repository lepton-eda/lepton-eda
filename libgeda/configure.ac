# Process this file with autoconf to produce a configure script.
AC_INIT
AC_CONFIG_SRCDIR([include/libgeda.h])
AC_PREREQ(2.54)

PACKAGE=libgeda
VERSION=20041228
SHARED_LIBRARY_VERSION=22:0:0
echo Configuring $PACKAGE version $VERSION

# Init automake
AM_INIT_AUTOMAKE($PACKAGE, $VERSION, no-define)
AM_CONFIG_HEADER([config.h])

# Init libtool
AM_PROG_LIBTOOL

# Init maintainer mode
AM_MAINTAINER_MODE

#########################################################################
# Command line flags start
# 
# --disable-gdgeda : turn off libgdgeda support
AC_ARG_ENABLE(gdgeda, [  --disable-gdgeda        Disable libgdgeda support (don't use libgdgeda at all)], 
  [ if test $enableval = "no"; then no_gdgeda=yes; fi ])

# Change default location for rc files
AC_ARG_WITH(rcdir, [  --with-rcdir=path       Change where the system-*rc files are installed], [opt_rcdir=$withval])
 
# Force a specific version of gtk+          
AC_ARG_WITH(gtk12, [  --with-gtk12            Force the use of gtk+ 1.2.x], [opt_gtkver=1.2])

# 
# Command line flags end
#########################################################################

# Checks for libraries.
#########################################################################
# 
# Misc win32 / mingw checks and variables start

# Figure out if we are building on win32 and what environment.
case $host_os in
  *cygwin*  ) AC_MSG_ERROR([Building gEDA/gaf under cygwin is not supported]) ;;
  *mingw32* ) echo "Configuring for mingw"; MINGW=yes ;;
esac

if ! test "$MINGW" = "no" -o "$MINGW"x = x; then
   MINGW_CFLAGS="-mms-bitfields -mwindows"
   MINGW_LIBS="-lregex -liberty"
   MINGW="yes"
   PATHSEP=\\\\
   OTHERPATHSEP=/
else
   # Unix host
   MINGW_CFLAGS=
   MINGW_LIBS=
   MINGW="no"
   PATHSEP=/
   OTHERPATHSEP=\\\\
fi

# 
# Misc win32 / mingw checks and variables end
#########################################################################

# Checks for programs.
AC_PROG_CC
AC_PROG_CPP
AC_PROG_MAKE_SET

############################################################################
# Check for guile start
#
GUILE_FLAGS

# Manually search for guile-config.  This is provided by GUILE_PROGS, but
# guile-1.4 does not have GUILE_PROGS, so we have to do it manually.
AC_PATH_PROG(GUILE_CONFIG,guile-config)
if test "$GUILE_CONFIG" = "" ; then
    AC_MSG_ERROR([guile-config required but not found])
fi
AC_SUBST(GUILE_CONFIG)

# must have guile flags in CFLAGS for AC_COMPILE_IFELSE to work right
saved_CFLAGS="${CFLAGS}"
CFLAGS="${CFLAGS} ${GUILE_CFLAGS}"

# 1-argument scm_eval_x (guile-1.4) or 2-argument form (guile-1.6)?
AC_MSG_CHECKING(
   [if scm_eval_x() takes a second module argument])
   AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <libguile.h>]], 
   [[SCM foo,bar; scm_eval_x(foo,bar);]])],
   [AC_DEFINE([HAVE_SCM_EVAL_X_MODULE], 1, 
   [Define if the SCM_EVAL_X function takes a second argument "module"])
   AC_MSG_RESULT(yes) ], [AC_MSG_RESULT(no)])
CFLAGS="${saved_CFLAGS}"

# Check that scm_make_smob_type exist
AC_CHECK_LIB(guile, scm_make_smob_type,,
   AC_MSG_ERROR(Upgrade Guile, At least version >= 1.3.2),
   $GUILE_LDFLAGS)

# Restore LIBS. Apperantly AC_CHECK_LIB destroys it.
LIBS="$ac_save_LIBS"

# Check that scm_set_smob_mark exist
AC_CHECK_LIB(guile, scm_set_smob_mark,,
   AC_MSG_ERROR(Upgrade Guile, At least version >= 1.3.2),
   $GUILE_LDFLAGS)

# Restore LIBS. Apperantly AC_CHECK_LIB destroys it.
LIBS="$ac_save_LIBS"

# Check that scm_set_smob_free exist
AC_CHECK_LIB(guile, scm_set_smob_free,,
   AC_MSG_ERROR(Upgrade Guile, At least version >= 1.3.2),
   $GUILE_LDFLAGS)

# Restore LIBS. Apperantly AC_CHECK_LIB destroys it.
LIBS="$ac_save_LIBS"

# Check that scm_set_smob_print exist
AC_CHECK_LIB(guile, scm_set_smob_print,,
   AC_MSG_ERROR(Upgrade Guile, At least version >= 1.3.2),
   $GUILE_LDFLAGS)

# Restore LIBS. Apperantly AC_CHECK_LIB destroys it.
LIBS="$ac_save_LIBS"

GUILE_VERSION=`$GUILE_CONFIG info guileversion`

#
# Check for guile end
############################################################################

############################################################################
# Check for mics things start
# 
# Checking for rint in math library
AC_CHECK_LIB(m, rint, AC_DEFINE(HAS_RINT, 1, [If your math library has rint in it, define this]), no_RINT="yes")

# Checking for dynamic lib
AC_CHECK_LIB(dl, dlopen, DL_LIB="-ldl", DL_LIB="")
#
# Check for mics things start
############################################################################

############################################################################
# Check for libgdgeda start
# 
AC_PATH_PROG(LIBGDGEDACONFIG, libgdgeda-config, no, ${PATH})

if test "$LIBGDGEDACONFIG" = "no"
then
   GDGEDA_CFLAGS=
   GDGEDA_LIBS=
   GDGEDA_VERSION=
   GDGEDA="no"
else
   if test "$no_gdgeda" = "yes" 
   then
     echo found libgdgeda, but disabling gdgeda support.
     GDGEDA_CFLAGS=
     GDGEDA_LIBS=
     GDGEDA_VERSION=
     GDGEDA="no"
   else 
     GDGEDA_CFLAGS=`$LIBGDGEDACONFIG --cflags`
     GDGEDA_LIBS1=`$LIBGDGEDACONFIG --ldflags`
     GDGEDA_LIBS2=`$LIBGDGEDACONFIG --libs`
     GDGEDA_LIBS="$GDGEDA_LIBS1 $GDGEDA_LIBS2"
     GDGEDA_VERSION=`$LIBGDGEDACONFIG --version`
     GDGEDA="yes"

     # This is not really used to figure out paths, but just to make 
     # sure >= 2.0.15 is installed
     PKG_CHECK_MODULES(libgdgeda, libgdgeda >= 2.0.15)
     AC_DEFINE(HAS_LIBGDGEDA, 1, [If libgdgeda has been installed, define this])
   fi
fi

# 
# Check for libgdgeda end
############################################################################

############################################################################
# Check for X11 start
# 

if test "$MINGW" = "no"
then
   AC_PATH_X
   AC_PATH_XTRA 
    
   X_EXTRA_LIBS="$X_EXTRA_LIBS"
   X_LDFLAGS="$X_LDFLAGS $X_LIBS $X_EXTRA_LIBS $X_PRE_LIBS -lX11 -lm"
fi

# 
# Check for X11 end
############################################################################

############################################################################
# Check for gtk+ 1.2 and 2.2 start
# 

# Check for pkg-config
AC_PATH_PROG(PKG_CONFIG, pkg-config, no)
if test $PKG_CONFIG = no; then
   AC_MSG_ERROR([Cannot find pkg-config, make sure it is installed and in your PATH])
fi

# Search for gtk+ 2.2.x first (only if we are not forcing the gtk+ to 1.2)
if test "$opt_gtkver" != "1.2"
then
   PKG_CHECK_MODULES(GTK22, gtk+-2.0 >= 2.2.0, GTK22="yes", no_GTK22="yes")
fi

# This next bit of code figures out what gtk we need to use.
if test "$GTK22" = "yes" -a "$opt_gtkver" != "1.2"
then

   AC_DEFINE(HAS_GTK22, 1, [If gtk+ 2.2.x has been installed, define this])
   GTK_CFLAGS=$GTK22_CFLAGS
   GTK_LIBS=$GTK22_LIBS
   GTK_VERSION=`$PKG_CONFIG gtk+-2.0 --modversion`

   # Search for glib
   PKG_CHECK_MODULES(GLIB22, glib-2.0 >= 2.2.0, GLIB22="yes", no_GLIB22="yes")
   if test "$GLIB22" != "yes"
   then
       AC_MSG_ERROR([Cannot find glib 2.2.x, install it and rerun ./configure.])
   fi
   GLIB_CFLAGS=$GLIB22_CFLAGS
   GLIB_LIBS=$GLIB22_LIBS
   GLIB_VERSION=`$PKG_CONFIG glib-2.0 --modversion`

else

   # Search for gtk+ 1.2 now 
   PKG_CHECK_MODULES(GTK12, gtk+ >= 1.2.3, GTK12="yes", no_GTK12="yes")

   AC_DEFINE(HAS_GTK12, 1, [If gtk+ 1.2.x has been installed, define this])
   GTK_CFLAGS=$GTK12_CFLAGS
   GTK_LIBS=$GTK12_LIBS
   GTK_VERSION=`$PKG_CONFIG gtk+ --modversion`

   # Search for glib
   PKG_CHECK_MODULES(GLIB12, glib >= 1.2.3, GLIB12="yes", no_GLIB12="yes")
   if test "$GLIB12" != "yes"
   then
       AC_MSG_ERROR([Cannot find glib >= 1.2.3, install it and rerun ./configure.])
   fi
   GLIB_CFLAGS=$GLIB12_CFLAGS
   GLIB_LIBS=$GLIB12_LIBS
   GLIB_VERSION=`$PKG_CONFIG glib --modversion`
fi

if test "$GTK_VERSION" = ""
then
   AC_MSG_ERROR([Cannot find gtk+ 2.2.x or gtk+ 1.2.x, install one of them.])
fi

# 
# Check for gtk+ 1.2 and 2.2 end
############################################################################

############################################################################
# Check for notangle, noweave, texi2html, indent start
# 

# Tangling is required if there is no C source files in the src/ 
# directory. Therefore they need to be extracted from the documented
# sources in noweb/ either with notangle or notangle_guile.scm.
#
# notangle is the utility that comes with the noweb package.
# notangle_guile.scm is a script that act the same as notangle
# but only require a guile interpreter with built-in module regex
# and getopt-long

# search for notangle
AC_PATH_PROG(NOTANGLE, notangle, no, ${PATH})
cfiles=`\ls -1 src/libgeda.c 2> /dev/null`
exit_status=$?

if test "$NOTANGLE" = "no" -a "$exit_status" = "1"; then

   # notangle not found, and no C sources in src 
   # need notangle_guile.scm
   echo "** Cannot find notangle (part of noweb) : using notangle_guile.scm instead **"
	
   # find the guile interpreter
   AC_PATH_PROG(GUILEINTERP, guile, no, ${PATH})
   if test "$GUILEINTERP" = "no"; then
      AC_MSG_ERROR([guile interpreter is required])
   fi
  
   # check (ice-9 getopt-long) module in guile
   AC_MSG_CHECKING([for getopt-long module in guile])
   guilegetopt=`$GUILEINTERP -c  '(use-modules (ice-9 getopt-long))' 2> /dev/null`
   exit_status=$?
   if test "$exit_status" = "1"; then
      AC_MSG_RESULT([no])
      echo "no support for getopt-long module in guile interpreter"
      AC_MSG_ERROR([this module is required by notangle_guile.scm but is not available in the interpreter])
   fi
   AC_MSG_RESULT([yes])
 
   # check (ice-9 regex) module in guile
   AC_MSG_CHECKING([for regex module in guile])
   guileregex=`$GUILEINTERP -c  '(use-modules (ice-9 regex))' 2> /dev/null`
   exit_status=$?
   if test ! "$exit_status" = "0"; then
      AC_MSG_RESULT([no])
      echo "no support for regex module in guile interpreter"
      AC_MSG_ERROR([this module is required by notangle_guile.scm but is not available in the interpreter])
   fi
   AC_MSG_RESULT([yes])

   # set NOTANGLE and NOTANGLEROOT to use notangle_guile.scm
   # as the result of the previous tests
   #  - NOTANGLE is the relative pathname to script from the source dir
   #  - NOTANGLEROOT is the option to give the root code chunk name
   #  - NOTANGLEFLAGS are other flags to notangle_guile.scm
   NOTANGLE="../scripts/notangle_guile.scm"
   NOTANGLEROOT="--rootname="
   NOTANGLEFLAGS=""

   echo "--            notangle_guile.scm should work fine on this system           --"

else

   # NOTANGLE has been set by AC_PATH_PROG above
   NOTANGLEROOT="-R"
   NOTANGLEFLAGS="-L"

fi

# Weaving is extracting the documentation from the noweb files.
# The noweb package provides noweave to weave these files.
# 
# The documentation is here written in Texinfo. A backend is 
# provided in scripts to format the code chunks in Texinfo.
#
# The noweave program is therefore required to produce the 
# documentation but building documentation is optionnal.
#

# search for noweave
AC_PATH_PROG(NOWEAVE, noweave, no, ${PATH})
# search for AWK
#  the noweb texinfo backend is based on awk. If awk is not available,
#  the backend will not work, and therefore it disables the 
#  documentation creation.
AC_PROG_AWK

if test "$NOWEAVE" = "no" -o "$AWK" = "no"; then 
   # noweave is not available on the system
   echo "** Cannot find noweave (part of noweb)! **"
   echo "**   Documentation creation disabled    **"
   NOWEAVE=echo

   # prevent from weaving the nw files
   # but does not prevent from processing gschemdoc
   builddoc=false
else 
   # noweave is available on the system.
   # now checking if the tools for Texinfo files processing
   # is installed.
   # The docs can be either processed to produce dvi, html or info
   # with texi2dvi, texi2html or makeinfo respectively
   # Note : these tests on makeinfo and texi2dvi are no more 
   #        needed as automake makes them for us

   # enable the weaving of the nw files to get documentation
   builddoc=true
fi

# depending on variable builddoc, weaving is performed or not
# using conditionnal in docs/Makefile.in
AM_CONDITIONAL(BUILDDOC, test x$builddoc = xtrue)

# search for texi2html
# it looks like automake does not take care of html doc creation
AC_PATH_PROG(TEXI2HTML, texi2html, no, ${PATH})

if test "$TEXI2HTML" = "no"; then 
   echo "**   Cannot find texi2html     **"
   echo "** HTML file creation disabled **"
   TEXI2HTML=echo
fi

# search for indent
AC_PATH_PROG(INDENT, indent, no, ${PATH})

if ! test "$INDENT" = "no"; then 
   indentversion=`$INDENT --version | grep GNU`
else 
   indentversion=
fi

if test "$INDENT" = "no" -o "$indentversion"x = x; then 
  AC_PATH_PROG(GINDENT, gindent, no, ${PATH})
  if test "$GINDENT" = "no"; then 
    echo "** Cannot find GNU indent or gindent, C code formatting disabled **"
    INDENT=echo
  else
    INDENT=$GINDENT
  fi
fi

# 
# Check for notangle, noweave, texi2html, indent end
#########################################################################

#########################################################################
# Checks for header files start
# 

AC_HEADER_STDC
AC_HEADER_SYS_WAIT
AC_HEADER_DIRENT
AC_CHECK_HEADERS(unistd.h string.h stdlib.h stdarg.h assert.h fcntl.h \
                 errno.h)

AC_MSG_CHECKING([for optarg in unistd.h])
AC_TRY_COMPILE(
[#include <unistd.h>],
[ char *string = optarg; int i = optind; ],
optarg_found=yes,
optarg_found=no)
AC_MSG_RESULT($optarg_found)
if test $optarg_found = yes; then
   AC_DEFINE(OPTARG_IN_UNISTD, 1, [Define if you have optarg in unistd.h])
fi

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST

# Checks for library functions.
AC_TYPE_SIGNAL
AC_CHECK_FUNCS(getcwd strstr vsnprintf snprintf)

# 
# Checks for header files end
#########################################################################

#########################################################################
# gEDA/gaf specific setup start
#

DATADIR=gEDA
GEDADATADIR=$datadir/$DATADIR

if eval "test x$opt_rcdir = x"; then
	# path was not specified with --with-rcdir
        AC_DEFINE_UNQUOTED(GEDARCDIR, "none", [gEDA/gaf's rc directory])
	GEDARCDIR=$GEDADATADIR
else
	# path WAS specified with --with-rcdir
        AC_DEFINE_UNQUOTED(GEDARCDIR, "$opt_rcdir", [gEDA/gaf's rc directory])
	GEDARCDIR="$opt_rcdir"
fi

# Defs for libary symbol files
#
# Expand the prefix variable
if eval "test x$prefix = xNONE"; then
 dprefix=$ac_default_prefix
else
 dprefix=$prefix
fi

gedatopdir=$dprefix/share/$DATADIR
expandgedadatadir=`echo $gedatopdir`

# this has to be expanded ( no ${prefix} ) --
AC_DEFINE_UNQUOTED(GEDADATADIR, "$gedatopdir", [gEDA/gaf's data directory])
AC_DEFINE_UNQUOTED(VERSION, "$VERSION", [Currently running version of gEDA/gaf])

#
# gEDA/gaf specify things which need to setup
#########################################################################


# Finally create the final CFLAGS and LDFLAGS for use in the makefiles
LIBGEDA_CFLAGS="$MINGW_CFLAGS $GUILE_CFLAGS $GDGEDA_CFLAGS $GTK_CFLAGS $X_CFLAGS"
LIBGEDA_LDFLAGS="$GUILE_LDFLAGS $GDGEDA_LIBS $GLIB_LIBS $MINGW_LIBS"
LIBTOOL_FLAGS="-version-info $SHARED_LIBRARY_VERSION"

# Makefile.in variable substitution
AC_SUBST(VERSION)
AC_SUBST(LIBGEDA_CFLAGS)
AC_SUBST(LIBGEDA_LDFLAGS)
AC_SUBST(LIBTOOL_FLAGS)
AC_SUBST(GEDADATADIR)
AC_SUBST(GEDARCDIR)
AC_SUBST(PATHSEP)
AC_SUBST(OTHERPATHSEP)
AC_SUBST(NOTANGLE)
AC_SUBST(NOTANGLEROOT)
AC_SUBST(NOTANGLEFLAGS)
AC_SUBST(GUILEINTERP)
AC_SUBST(NOWEAVE)
AC_SUBST(TEXI2HTML)
AC_SUBST(INDENT)

# Create all the necessary derived files
AC_CONFIG_FILES([Makefile
                 src/Makefile
                 noweb/Makefile
                 scripts/Makefile
                 include/Makefile
                 docs/Makefile
	         libgeda.pc
	         scripts/geda_totexi
	         scripts/notangle_guile.scm ])

AC_CONFIG_COMMANDS([geda_totexi-chmod], 
                   [[chmod +x scripts/geda_totexi]],[[]])
AC_CONFIG_COMMANDS([notangle_guile.scm-chmod],
                   [[chmod +x scripts/notangle_guile.scm]],[[]])

AC_OUTPUT

expandedGEDADATADIR=`eval "echo $GEDADATADIR"`
expandedGEDARCDIR=`eval "echo $GEDARCDIR"`

AC_MSG_RESULT([
** Configuration summary for $PACKAGE $VERSION:

   GLIB library version:             $GLIB_VERSION
   GTK+ library version:             $GTK_VERSION
   GUILE library version:            $GUILE_VERSION
   libgdgeda (png output):           $GDGEDA $GDGEDA_VERSION
   mingw build:                      $MINGW
   data directory:                   $expandedGEDADATADIR
   rc directory:                     $expandedGEDARCDIR
   LIBGEDA .so version:              $SHARED_LIBRARY_VERSION
])

