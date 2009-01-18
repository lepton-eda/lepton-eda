# Process this file with autoconf to produce a configure script.
AC_INIT
AC_CONFIG_SRCDIR([src/gattrib.c])
AC_PREREQ(2.54)

PACKAGE=geda-gattrib
DOTTED_VERSION=1.5.1
DATE_VERSION=20081221
echo Configuring $PACKAGE version $DOTTED_VERSION.$DATE_VERSION

# Initialize automake
AM_INIT_AUTOMAKE($PACKAGE, $DOTTED_VERSION, no-define)
AM_CONFIG_HEADER([config.h])

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

# Change default location for XDG files (.desktop and icons)
AC_ARG_WITH(xdgdatadir, [  --with-xdgdatadir=path  Change where the .desktop file and theme icons are installed [[DATADIR]]], [opt_xdgdatadir=$withval])

# 
# Command line flags end
#########################################################################

# Checks for programs.
AC_PROG_CC
AM_CONDITIONAL(CCISGCC, test "$GCC" = "yes")	
AC_PROG_MAKE_SET
AC_PATH_PROGS(AWK, nawk gawk mawk awk, )
IT_PROG_INTLTOOL(0.35.0)

############################################################################
# Update desktop database utility start
#

AC_ARG_ENABLE(update-desktop-database,
   AC_HELP_STRING([--disable-update-desktop-database],
                   [do not update desktop file database after installation]),,
                   enable_update_desktop_database=yes)

AM_CONDITIONAL(ENABLE_UPDATE_DESKTOP_DATABASE,
               test x$enable_update_desktop_database = xyes)

if test x$enable_update_desktop_database = xyes ; then
  AC_PATH_PROG(UPDATE_DESKTOP_DATABASE, [update-desktop-database], no)
  if test $UPDATE_DESKTOP_DATABASE = no; then
     AC_MSG_ERROR([Cannot find update-desktop-database, make sure it is installed and in your PATH, or configure with --disable-update-desktop-database])
  fi
fi

#
# Update desktop database utility end
############################################################################

# Set USE_NLS
AM_NLS

# Set package name for translations
GETTEXT_PACKAGE=$PACKAGE
AC_SUBST(GETTEXT_PACKAGE)

# Check pre-requsites for the PO dir.
# (Needed without explicitly initialising gettext)
AM_PO_SUBDIRS

#########################################################################
# 
#  Misc win32 / mingw checks and variables start
#  Win32 stuff currently unsupported by SDB.
AC_CANONICAL_HOST

# Figure out if we are building on win32 and what environment.
case $host_os in
  *mingw32* ) echo "Configuring for mingw"; MINGW=yes ;;
esac

if ! test "$MINGW" = "no" -o "$MINGW"x = x; then
   MINGW="yes"
   MINGW_CFLAGS="-mms-bitfields -mwindows"
else
   # Unix host
   MINGW="no"
   MINGW_CFLAGS=
fi

# 
# Misc win32 / mingw checks and variables end
#########################################################################

############################################################################
# Check for guile start

# Check for pkg-config
AC_PATH_PROG(PKG_CONFIG, pkg-config, no)
if test $PKG_CONFIG = no; then
   AC_MSG_ERROR([Cannot find pkg-config, make sure it is installed and in your PATH])
fi

PKG_CHECK_MODULES(GUILE, [guile-1.8], GUILE_PC="yes", no_GUILE_PC="yes")

if test "$GUILE_PC" = "yes" 
then

   GUILE_VERSION=`$PKG_CONFIG guile-1.8 --modversion`

else

   # Find about the installed guile
   GUILE_FLAGS
   GUILE_VERSION=`$GUILE_CONFIG info guileversion`

fi

# Check Guile version
guile_need_major=1
guile_need_minor=6
guile_need_version="$guile_need_major[].$guile_need_minor.0"

AC_MSG_CHECKING([Guile version >= $guile_need_version])

guile_major=`echo "$GUILE_VERSION" | sed 's/\([[^.]][[^.]]*\).*/\1/'`
guile_minor=`echo "$GUILE_VERSION" | sed 's/[[^.]][[^.]]*.\([[^.]][[^.]]*\).*/\1/'`
AC_MSG_RESULT($GUILE_VERSION)

if test "$guile_need_major" -gt "$guile_major" \
   || (test "$guile_need_major" -eq "$guile_major" \
       && test "$guile_need_minor" -gt "$guile_minor"); then
  AC_MSG_ERROR([Guile version >= $guile_need_version is required.])
fi

# Guile 1.6 compatability

CFLAGS_temp_save="$CFLAGS"
CFLAGS="$CFLAGS $GUILE_CFLAGS"
AC_CHECK_DECLS([scm_is_string, scm_is_integer, scm_to_int,
scm_from_int,  scm_is_true,    scm_is_false,
scm_from_locale_string, scm_to_locale_string],,,
[#include <libguile.h>])
CFLAGS="$CFLAGS_temp_save"

AH_VERBATIM(SCM_IS_STRING, [#if !HAVE_DECL_SCM_IS_STRING
#  define scm_is_string(x) SCM_STRINGP(x)
#endif])
AH_VERBATIM(SCM_IS_INTEGER, [#if !HAVE_DECL_SCM_IS_INTEGER
#  define scm_is_integer(x) SCM_INUMP(x)
#endif])
AH_VERBATIM(SCM_TO_INT, [#if !HAVE_DECL_SCM_TO_INT
#  define scm_to_int(x)     SCM_INUM(x)
#endif])
AH_VERBATIM(SCM_FROM_INT, [#if !HAVE_DECL_SCM_FROM_INT
#  define scm_from_int(x)   SCM_MAKINUM(x)
#endif])
AH_VERBATIM(SCM_IS_TRUE, [#if !HAVE_DECL_SCM_IS_TRUE
#  define scm_is_true(x)    SCM_NFALSEP(x)
#endif])
AH_VERBATIM(SCM_IS_FALSE, [#if !HAVE_DECL_SCM_IS_FALSE
#  define scm_is_false(x)   SCM_FALSEP(x)
#endif])
AH_VERBATIM(SCM_FROM_LOCALE_STRING, [#if !HAVE_DECL_SCM_FROM_LOCALE_STRING
#  define scm_from_locale_string(x) scm_makfrom0str(x)
#endif])
AH_VERBATIM(SCM_TO_LOCALE_STRING, [#if !HAVE_DECL_SCM_TO_LOCALE_STRING
#  define scm_to_locale_string(x)   strdup(SCM_STRING_CHARS(x))
#endif])

#
# Check for guile end
############################################################################

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
##  This is looks for GTK
#                                                                                      
# Search for gtk+ 2.8.x
PKG_CHECK_MODULES(GTK, [gtk+-2.0 >= 2.8.0], GTK="yes", no_GTK="yes")

# This next bit of code figures out what gtk we need to use.
if test "$GTK" = "yes"
then
   GTK_VERSION=`$PKG_CONFIG gtk+-2.0 --modversion`

   # Search for glib
   PKG_CHECK_MODULES(GLIB, [glib-2.0 >= 2.8.0], GLIB="yes", no_GLIB="yes")
   if test "$GLIB" != "yes"
   then
       AC_MSG_ERROR([Cannot find glib 2.8.0 or later, please install it and rerun ./configure.])
   fi
   GLIB_VERSION=`$PKG_CONFIG glib-2.0 --modversion`

##   GTKITEMENTRY_SOURCE=gtkitementry_2_2.c gtkitementry_2_2.h

   # Search for gthread
   PKG_CHECK_MODULES(GTHREAD, [gthread-2.0], GTHREAD="yes", no_GTHREAD="yes")
   if test "$GTHREAD" = "yes"
   then
       AC_DEFINE(HAVE_GTHREAD, 1, [If gthread support is installed, define this])
   fi

else
   GTK_VERSION=""
fi

if test "$GTK_VERSION" = ""
then
   AC_MSG_ERROR([Cannot find gtk+ 2.8.0 or later, please install it and rerun ./configure.])
fi

#
# Check for gtk+ end
############################################################################


############################################################################
# Check for libgeda start
# 
PKG_CHECK_MODULES(LIBGEDA, [libgeda >= $DATE_VERSION], LIBGEDA="yes", 
                  no_LIBGEDA="yes")

if test "$LIBGEDA" = "yes" 
then
   LIBGEDA_VERSION=`$PKG_CONFIG libgeda --modversion`
else
   AC_MSG_ERROR([libgeda detection error: $LIBGEDA_PKG_ERRORS])
fi

# 
# Check for libgeda end
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
AC_CHECK_HEADERS(unistd.h string.h stdlib.h \
                 stdarg.h assert.h fcntl.h errno.h \
		 dirent.h sys/param.h getopt.h)

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST

# Checks for library functions.
AC_TYPE_SIGNAL
AC_CHECK_FUNCS(getopt_long)


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

if test x$opt_xdgdatadir = x; then
        # path was not specified with --with-xdgdatadir
        XDGDATADIR='${datadir}'
else
        # path WAS specified with --with-xdgdatadir
        XDGDATADIR="$opt_xdgdatadir"
fi
AC_SUBST(XDGDATADIR)

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
AC_DEFINE_UNQUOTED(DATE_VERSION, "$DATE_VERSION", [Currently running date version of gEDA/gaf])
AC_DEFINE_UNQUOTED(DOTTED_VERSION, "$DOTTED_VERSION", [Currently running dotted version of gEDA/gaf])
AC_DEFINE_UNQUOTED(PACKAGE, "$PACKAGE", [Name of this program's package])

#
# gEDA/gaf specify things which need to setup
#########################################################################

# Finally create the final CFLAGS and LDFLAGS for use in the makefiles
GATTRIB_CFLAGS="$MINGW_CFLAGS $GTK_CFLAGS $GLIB_CFLAGS $LIBGEDA_CFLAGS $GTHREAD_CFLAGS $GUILE_CFLAGS"
GATTRIB_LDFLAGS="$GTK_LIBS $GLIB_LIBS $LIBGEDA_LIBS $GTHREAD_LIBS $GUILE_LIBS"

# Makefile.in variable substitution
AC_SUBST(DATE_VERSION)
AC_SUBST(DOTTED_VERSION)
AC_SUBST(GATTRIB_CFLAGS)
AC_SUBST(GATTRIB_LDFLAGS)
AC_SUBST(GEDARCDIR)
AC_SUBST(GEDADATADIR)

# Create all the necessary derived files
AC_CONFIG_FILES([Makefile 
		 src/Makefile
		 lib/Makefile 
		 docs/Makefile
		 design/Makefile
		 include/Makefile 
		 data/Makefile
		 po/Makefile.in
		 po/Makefile
		 lib/system-gattribrc
		])

AC_OUTPUT

expandedGEDADATADIR=`eval "echo $GEDADATADIR"`
expandedGEDARCDIR=`eval "echo $GEDARCDIR"`
expandedXDGDATADIR=`eval "echo $XDGDATADIR"`

AC_MSG_RESULT([
***** Configuration summary for $PACKAGE $DOTTED_VERSION.$DATE_VERSION *****

== libgeda library version:          $LIBGEDA_VERSION
== data directory:                   $expandedGEDADATADIR
== rc directory:                     $expandedGEDARCDIR
== xdg directory:                    $expandedXDGDATADIR
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

