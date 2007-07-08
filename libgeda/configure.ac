# Process this file with autoconf to produce a configure script.
AC_INIT
AC_CONFIG_SRCDIR([include/libgeda.h])
AC_PREREQ(2.54)

PACKAGE=libgeda
DOTTED_VERSION=1.1.1
DATE_VERSION=20070708
SHARED_LIBRARY_VERSION=30:1:0
echo Configuring $PACKAGE version $DOTTED_VERSION.$DATE_VERSION

# Init automake
AM_INIT_AUTOMAKE($PACKAGE, $DOTTED_VERSION, no-define)
AM_CONFIG_HEADER([config.h])

# Init libtool
AM_PROG_LIBTOOL

#########################################################################
# Command line flags start
# 

# Change default location for rc files
AC_ARG_WITH(rcdir, [  --with-rcdir=path       Change where the system-*rc files are installed], [opt_rcdir=$withval])
 
# 
# Command line flags end
#########################################################################

# Checks for libraries.
#########################################################################
# 
# Misc win32 / mingw checks and variables start

# Figure out if we are building on win32 and what environment.
case $host_os in
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
AM_CONDITIONAL(CCISGCC, test "$GCC" = "yes")    
AC_PROG_CPP
AC_PROG_MAKE_SET

############################################################################
# Check for guile start
#
GUILE_FLAGS

# Check Guile version
guile_need_major=1
guile_need_minor=8
guile_need_version="$guile_need_major[].$guile_need_minor.0"

AC_MSG_CHECKING([Guile version >= $guile_need_version])
GUILE_VERSION=`$GUILE_CONFIG info guileversion`

guile_major=`echo "$GUILE_VERSION" | sed 's/\([[^.]][[^.]]*\).*/\1/'`
guile_minor=`echo "$GUILE_VERSION" | sed 's/[[^.]][[^.]]*.\([[^.]][[^.]]*\).*/\1/'`
AC_MSG_RESULT($GUILE_VERSION)

if test $guile_need_major -gt $guile_major \
   || test $guile_need_minor -gt $guile_minor; then
  AC_MSG_ERROR([Guile version >= $guile_need_version is required.])
fi

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
# Check for gtk+ 2.4 start
# 

# Check for pkg-config
AC_PATH_PROG(PKG_CONFIG, pkg-config, no)
if test $PKG_CONFIG = no; then
   AC_MSG_ERROR([Cannot find pkg-config, make sure it is installed and in your PATH])
fi

PKG_CHECK_MODULES(GTK24, gtk+-2.0 >= 2.4.0, GTK24="yes", no_GTK24="yes")

# This next bit of code figures out what gtk we need to use.
if test "$GTK24" = "yes"
then

   AC_DEFINE(HAS_GTK24, 1, [If gtk+ 2.4.x has been installed, define this])
   GTK_CFLAGS=$GTK24_CFLAGS
   GTK_LIBS=$GTK24_LIBS
   GTK_VERSION=`$PKG_CONFIG gtk+-2.0 --modversion`

   # Search for glib
   PKG_CHECK_MODULES(GLIB24, glib-2.0 >= 2.4.0, GLIB24="yes", no_GLIB24="yes")
   if test "$GLIB24" != "yes"
   then
       AC_MSG_ERROR([Cannot find glib 2.4.x, install it and rerun ./configure.
You may find the following error output (if any) useful:
$GLIB24_PKG_ERRORS
])
   fi
   GLIB_CFLAGS=$GLIB24_CFLAGS
   GLIB_LIBS=$GLIB24_LIBS
   GLIB_VERSION=`$PKG_CONFIG glib-2.0 --modversion`

else
   GTK_VERSION=""
fi

if test "$GTK_VERSION" = ""
then
   AC_MSG_ERROR([Cannot find gtk+ 2.4.x or later, please install gtk+.])
fi

# 
# Check for gtk+ 2.4 end
############################################################################

############################################################################
# Check for gdk 2.4 or later
# 

# Check for pkg-config
AC_PATH_PROG(PKG_CONFIG, pkg-config, no)
if test $PKG_CONFIG = no; then
   AC_MSG_ERROR([Cannot find pkg-config, make sure it is installed and in your PATH])
fi

PKG_CHECK_MODULES(GDK, gdk-2.0 >= 2.4.0, GDK="yes", no_GDK="yes")

# This next bit of code figures out what gtk we need to use.
if test "$GDK" = "yes" 
then

   AC_DEFINE(HAS_GDK, 1, [If gdk has been installed, define this])
   GDK_CFLAGS=$GDK_CFLAGS
   GDK_LIBS=$GDK_LIBS
   GDK_VERSION=`$PKG_CONFIG gdk-2.0 --modversion`

fi

if test "$GDK_VERSION" = ""
then
   AC_MSG_ERROR([Cannot find gdk, please make sure it is installed.])
fi

# 
# Check for gdk end
############################################################################

############################################################################
# Check for gdk-pixbuf 
# 

# Check for pkg-config
AC_PATH_PROG(PKG_CONFIG, pkg-config, no)
if test $PKG_CONFIG = no; then
   AC_MSG_ERROR([Cannot find pkg-config, make sure it is installed and in your PATH])
fi

PKG_CHECK_MODULES(GDK_PIXBUF, gdk-pixbuf-2.0 >= 0.15.0, GDK_PIXBUF="yes", no_GDK_PIXBUF="yes")

# This next bit of code figures out what gtk we need to use.
if test "$GDK_PIXBUF" = "yes" 
then

   AC_DEFINE(HAS_GDK_PIXBUF, 1, [If gdk-pixbuf has been installed, define this])
   GDK_PIXBUF_CFLAGS=$GDK_PIXBUF_CFLAGS
   GDK_PIXBUF_LIBS=$GDK_PIXBUF_LIBS
   GDK_PIXBUF_VERSION=`$PKG_CONFIG gdk-pixbuf-2.0 --modversion`

fi

if test "$GDK_PIXBUF_VERSION" = ""
then
   AC_MSG_ERROR([Cannot find gdk-pixbuf, please make sure it is installed.])
fi

# 
# Check for gdk-pixbuf end
############################################################################



############################################################################
# Check for doxygen start
# 

# Doxygen is a utility for generating html and latex documentation
# from c source code files.

# search for Doxygen 
AC_PATH_PROG(DOXYGEN, doxygen, no, ${PATH})

if test "$DOXYGEN" = "no"; then 
   # doxygen is not available on the system
   echo "** Cannot find Doxygen! **"
   echo "**   Documentation creation disabled    **"
   DOXYGEN=echo

   # prevent from creating html and latex documents.
   # but does not prevent from processing gschemdoc
   builddoc=false
else 
   # doxygen is available on the system.
   # now checking if the tools for Texinfo files processing
   # is installed.
   # The docs can be either processed to produce dvi, html or info
   # with texi2dvi, texi2html or makeinfo respectively
   # Note : these tests on makeinfo and texi2dvi are no more 
   #        needed as automake makes them for us

   # enable the creation of html and latex documents.
   builddoc=true
fi

# depending on variable builddoc, weaving is performed or not
# using conditionnal in docs/Makefile.in
AM_CONDITIONAL(BUILDDOC, test x$builddoc = xtrue)

# 
# Check for doxygen end
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
AC_CHECK_FUNCS(getcwd strstr vsnprintf snprintf chown)

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
AC_DEFINE_UNQUOTED(DATE_VERSION, "$DATE_VERSION", [Currently running date version of gEDA/gaf])
AC_DEFINE_UNQUOTED(DOTTED_VERSION, "$DOTTED_VERSION", [Currently running dotted version of gEDA/gaf])
AC_DEFINE_UNQUOTED(PACKAGE, "$PACKAGE", [Name of this program's package])

#
# gEDA/gaf specify things which need to setup
#########################################################################


# Finally create the final CFLAGS and LDFLAGS for use in the makefiles
LIBGEDA_CFLAGS="$MINGW_CFLAGS $GUILE_CFLAGS $GTK_CFLAGS $X_CFLAGS $GDK_PIXBUF_CFLAGS $GDK_CFLAGS"
LIBGEDA_LDFLAGS="$GUILE_LDFLAGS $GLIB_LIBS $MINGW_LIBS $DMALLOC_LIBS $GDK_PIXBUF_LIBS $GDK_LIBS"
LIBTOOL_FLAGS="-version-info $SHARED_LIBRARY_VERSION"

# Makefile.in variable substitution
AC_SUBST(DATE_VERSION)
AC_SUBST(DOTTED_VERSION)
AC_SUBST(LIBGEDA_CFLAGS)
AC_SUBST(LIBGEDA_LDFLAGS)
AC_SUBST(LIBTOOL_FLAGS)
AC_SUBST(GEDADATADIR)
AC_SUBST(GEDARCDIR)
AC_SUBST(PATHSEP)
AC_SUBST(OTHERPATHSEP)
AC_SUBST(GUILEINTERP)
AC_SUBST(INDENT)

# Create all the necessary derived files
AC_CONFIG_FILES([Makefile
                 src/Makefile
                 scripts/Makefile
                 include/Makefile
                 docs/Makefile
	         libgeda.pc
		 share/Makefile
		 lib/Makefile
		 scheme/Makefile ])

AC_OUTPUT

expandedGEDADATADIR=`eval "echo $GEDADATADIR"`
expandedGEDARCDIR=`eval "echo $GEDARCDIR"`

AC_MSG_RESULT([
** Configuration summary for $PACKAGE $DOTTED_VERSION.$DATE_VERSION

   GLIB library version:             $GLIB_VERSION
   GTK+ library version:             $GTK_VERSION
   GUILE library version:            $GUILE_VERSION
   GDK-PIXBUF (png output):          $GDK_PIXBUF_VERSION
   mingw build:                      $MINGW
   data directory:                   $expandedGEDADATADIR
   rc directory:                     $expandedGEDARCDIR
   LIBGEDA .so version:              $SHARED_LIBRARY_VERSION
   dmalloc debugging:                $with_dmalloc
   ElectricFence debugging:          $with_efence
])

