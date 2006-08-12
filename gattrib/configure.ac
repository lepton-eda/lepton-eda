dr Process this file with autoconf to produce a configure script.
AC_INIT
AC_CONFIG_SRCDIR([src/gattrib.c])
AC_PREREQ(2.53)

PACKAGE=geda-gattrib
GEDA_VERSION=20060123
GATTRIB_VERSION=20060123
echo Configuring $PACKAGE version $GATTRIB_VERSION
echo Part of gEDA version $GEDA_VERSION

# Initialize automake -- use base gEDA version here
AM_INIT_AUTOMAKE($PACKAGE, $GEDA_VERSION)
AM_CONFIG_HEADER([config.h])

# Initialize maintainer mode
AM_MAINTAINER_MODE

# Create option to debug
AC_ARG_ENABLE(debug,
[ --enable-debug  Sets the DEBUG flag which causes lots of debug spew to be generated],
[ AC_DEFINE(DEBUG, 1, DEBUG) ]
)

#########################################################################
# Command line flags start
# 
# Change default location for rc files
AC_ARG_WITH(rcdir, [  --with-rcdir=path       Change where the system-*rc files are installed], [opt_rcdir=$withval])

# 
# Command line flags end
#########################################################################

# Checks for programs.
AC_PROG_CC
AM_CONDITIONAL(CCISGCC, test "$GCC" = "yes")	
AC_PROG_MAKE_SET
AC_PATH_PROGS(AWK, nawk gawk mawk awk, )

#########################################################################
# 
#  Misc win32 / mingw checks and variables start
#  Win32 stuff currently unsupported by SDB.  I left it in because we need
#  to set the PATHSEP var.
AC_CANONICAL_HOST

# Figure out if we are building on win32 and what environment.
case $host_os in
  *cygwin*  ) AC_MSG_ERROR([Building gEDA/gaf under cygwin is not supported]) ;;
  *mingw32* ) echo "Configuring for mingw"; MINGW=yes ;;
esac

if ! test "$MINGW" = "no" -o "$MINGW"x = x; then
   MINGW="yes"
   PATHSEP=\\\\
   OTHERPATHSEP=/
else
   # Unix host
   MINGW="no"
   PATHSEP=/
   OTHERPATHSEP=\\\\
fi

# 
# Misc win32 / mingw checks and variables end
#########################################################################


############################################################################
# Check for misc things. . . .
# 
# Checking for rint in math library
AC_CHECK_LIB(m, rint, AC_DEFINE(HAS_RINT, 1, [If your math library has rint in it, define this]), no_RINT="yes")

# Checking for dynamic lib
AC_CHECK_LIB(dl, dlopen, DL_LIB="-ldl", DL_LIB="")
#
############################################################################


############################################################################
##  This is looks for GTK2.2.
#                                                                                      
# Check for pkg-config
AC_PATH_PROG(PKG_CONFIG, pkg-config, no)
if test $PKG_CONFIG = no; then
   AC_MSG_ERROR([Cannot find pkg-config, make sure it is installed and in your PATH])
fi

# Search for gtk+ 2.2.x 
PKG_CHECK_MODULES(GTK22, gtk+-2.0 >= 2.2.0, GTK22="yes", no_GTK22="yes")

# This next bit of code figures out what gtk we need to use.
if test "$GTK22" = "yes" 
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

##   GTKITEMENTRY_SOURCE=gtkitementry_2_2.c gtkitementry_2_2.h
    
else
   GTK_VERSION=""
fi

                                                                                      
if test "$GTK_VERSION" = ""
then
   AC_MSG_ERROR([Cannot find gtk+ 2.2.x or later, please install gtk+.])
fi

##  These tell Makefile.am which package to compile
AM_CONDITIONAL(GTK22_SOURCE, test "$GLIB22" = "yes")
                                                                                      
#
# Check for gtk+ 2.2 end
############################################################################
                                                                                      

############################################################################
# Check for libgeda start
# 
PKG_CHECK_MODULES(LIBGEDA, libgeda >= $GEDA_VERSION, LIBGEDA="yes", 
                  no_LIBGEDA="yes")

if test "$LIBGEDA" = "yes" 
then
   LIBGEDA_VERSION=`$PKG_CONFIG libgeda --modversion`
else
   AC_MSG_ERROR([Cannot find libgeda>=$GEDA_VERSION (or libgeda.pc), please install libgeda-$GEDA_VERSION])
fi

# 
# Check for libgeda end
############################################################################

#########################################################################
# Checks for header files start
# 
AC_HEADER_STDC
AC_HEADER_SYS_WAIT
AC_HEADER_DIRENT
AC_CHECK_HEADERS(unistd.h string.h stdlib.h \
                 stdarg.h assert.h fcntl.h errno.h \
		 dirent.h sys/param.h getopt.h)

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST

# Checks for library functions.
AC_TYPE_SIGNAL
AC_CHECK_FUNCS(getcwd strstr vsnprintf snprintf getopt_long)


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

# 
# Checks for header files end
#########################################################################

#########################################################################
#

# ------------- dmalloc -------------------
dnl dmalloc checks
with_dmalloc="no"
AC_MSG_CHECKING([if dmalloc debugging should be enabled])
AC_ARG_ENABLE([dmalloc],
[  --enable-dmalloc        Compile and link with dmalloc for malloc debugging [[default=no]]],
[
if test "X$enable_dmalloc" != "Xno" ; then
        AC_MSG_RESULT([yes])
        AC_CHECK_HEADER(dmalloc.h,,
                AC_ERROR([You have requested dmalloc debugging but dmalloc.h could not be found]))
        AC_CHECK_LIB(dmalloc,main,,
                AC_ERROR([You have requested dmalloc debugging but -ldmalloc could not be found]))
        DMALLOC_LIBS="-ldmalloc"
        with_dmalloc="yes"
else
        AC_MSG_RESULT([no])
        DMALLOC_LIBS=""
fi
],
[
        AC_MSG_RESULT([no])
        DMALLOC_LIBS=""
])

# ------------- ElectricFence -------------------
dnl ElectricFence checks
with_efence="no"
AC_MSG_CHECKING([if ElectricFence debugging should be enabled])
AC_ARG_ENABLE([efence],
[  --enable-efence         Link with ElectricFence for malloc debugging [[default=no]]],
[
if test "X$enable_efence" != "Xno" ; then
        AC_MSG_RESULT([yes])
	if test "X$with_dmalloc" = "Xyes" ; then
		AC_ERROR([You have requested both dmalloc and Electric Fence, however only 1 is allowed.])
	fi
        AC_CHECK_LIB(efence,main,,
                AC_ERROR([You have requested ElectricFence debugging but -lefence could not be found]))
        with_efence="yes"
else
        AC_MSG_RESULT([no])
fi
],
[
AC_MSG_RESULT([no])
])

#
#########################################################################

#########################################################################
# gEDA/gaf specify setup start
#

DATADIR=gEDA
GEDADATADIR=$datadir/$DATADIR

if eval "test x$opt_rcdir = x"; then
	# path not was specified with --with-rcdir
        AC_DEFINE_UNQUOTED(GEDARCDIR, "none", [gEDA/gaf's rc directory])
	GEDARCDIR=$GEDADATADIR
else
	# path WAS specified with --with-rcdir
        AC_DEFINE_UNQUOTED(GEDARCDIR, "$opt_rcdir", [gEDA/gaf's rc directory])
	GEDARCDIR="$opt_rcdir"
fi

# Expand the prefix variable
# I don't like the way this is done, but it works (I hope).
if eval "test x$prefix = xNONE"; then
 dprefix=$ac_default_prefix
else
 dprefix=$prefix
fi

gedatopdir=$dprefix/share/$DATADIR
expandgedadatadir=`echo $gedatopdir`

# this has to be expanded ( no ${prefix} ) --
AC_DEFINE_UNQUOTED(GEDADATADIR, "$gedatopdir", [gEDA/gaf's data directory])
AC_DEFINE_UNQUOTED(VERSION, "$GEDA_VERSION", [Currently running version of gEDA/gaf])

#
# gEDA/gaf specify things which need to setup
#########################################################################

# Finally create the final CFLAGS and LDFLAGS for use in the makefiles
GATTRIB_CFLAGS="$GTK_CFLAGS $GLIB_CFLAGS $LIBGEDA_CFLAGS"
GATTRIB_LDFLAGS="$GTK_LIBS $GLIB_LIBS $LIBGEDA_LIBS"

# Makefile.in variable substitution
AC_SUBST(GATTRIB_CFLAGS)
AC_SUBST(GATTRIB_LDFLAGS)
AC_SUBST(GEDARCDIR)
AC_SUBST(GEDADATADIR)
AC_SUBST(PATHSEP)
AC_SUBST(OTHERPATHSEP)

# Create all the necessary derived files
AC_CONFIG_FILES([Makefile 
		 src/Makefile
		 lib/Makefile 
		 design/Makefile
		 include/Makefile 
		 lib/system-gattribrc
		])

AC_OUTPUT

expandedGEDADATADIR=`eval "echo $GEDADATADIR"`
expandedGEDARCDIR=`eval "echo $GEDARCDIR"`

AC_MSG_RESULT([
***** Configuration summary for $PACKAGE $GATTRIB_VERSION *****

== gattrib version:                  $GATTRIB_VERSION
== base gEDA version:                $GEDA_VERSION
== libgeda library version:          $LIBGEDA_VERSION
== data directory:                   $expandedGEDADATADIR
== rc directory:                     $expandedGEDARCDIR
== compiler flags:                   $GATTRIB_CFLAGS
== linker flags:                     $GATTRIB_LDFLAGS
== glib compiler flags:              $GLIB_CFLAGS
== glib linker libs:                 $GLIB_LIBS
== glib library version:             $GLIB_VERSION
== gtk compiler flags:               $GTK_CFLAGS
== gtk linker libs:                  $GTK_LIBS
== gtk library version:              $GTK_VERSION
== dmalloc debugging:                $with_dmalloc
== Electric Fence debugging:         $with_efence

************************************************************
])

