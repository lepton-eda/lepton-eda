dnl Process this file with autoconf to produce a configure script.
AC_INIT(configure.ac)

dnl Initialize automake stuff
PACKAGE=geda-setup

# new way of doing version
VERSION=20041228

echo Configuring $PACKAGE version $VERSION

AM_INIT_AUTOMAKE($PACKAGE, $VERSION)
AM_CONFIG_HEADER(config.h)

AC_ISC_POSIX
AC_PROG_CC
AM_CONDITIONAL(CCISGCC, test "$GCC" = "yes")    
AM_PROG_CC_STDC
AC_HEADER_STDC

AM_PATH_GTK(1.2.0, ,
            AC_MSG_ERROR(Cannot find GTK: Is gtk-config in path?))

dnl Checks for programs.
dnl Checks for libraries.
dnl Checks for header files.
dnl Checks for typedefs, structures, and compiler characteristics.
dnl Checks for library functions.
dnl Checks for Additional stuffs.

dnl Languages which your application supports
ALL_LINGUAS=""
AM_GNU_GETTEXT
AM_GNU_GETTEXT_VERSION(0.12.1)

dnl Set PACKAGE_LOCALE_DIR in config.h.
if test "x${prefix}" = "xNONE"; then
  AC_DEFINE_UNQUOTED(PACKAGE_LOCALE_DIR, "${ac_default_prefix}/${DATADIRNAME}/locale", [Package locale directory])
else
  AC_DEFINE_UNQUOTED(PACKAGE_LOCALE_DIR, "${prefix}/${DATADIRNAME}/locale", [Package locale directory])
fi


dnl Set PACKAGE SOURCE DIR in config.h.
packagesrcdir=`cd $srcdir && pwd`

dnl Set PACKAGE PREFIX
if test "x${prefix}" = "xNONE"; then
  packageprefix=${ac_default_prefix}
else
  packageprefix=${prefix}
fi

dnl Set PACKAGE DATA & DOC DIR
packagedatadir=share/${PACKAGE}
packagedocdir=doc/${PACKAGE}

dnl Set PACKAGE DIRS in config.h.
packagepixmapsdir=${packagedatadir}/pixmaps
packagehelpdir=${packagedatadir}/help
packagemenudir=${packagedatadir}

dnl Subst PACKAGE_DATA_DIR.
NO_PREFIX_PACKAGE_DATA_DIR="${packagedatadir}"
AC_SUBST(NO_PREFIX_PACKAGE_DATA_DIR)
PACKAGE_DATA_DIR="${packageprefix}/${packagedatadir}"
AC_SUBST(PACKAGE_DATA_DIR)

dnl Subst PACKAGE_DOC_DIR.
NO_PREFIX_PACKAGE_DOC_DIR="${packagedocdir}"
AC_SUBST(NO_PREFIX_PACKAGE_DOC_DIR)
PACKAGE_DOC_DIR="${packageprefix}/${packagedocdir}"
AC_SUBST(PACKAGE_DOC_DIR)

dnl Subst PACKAGE_PIXMAPS_DIR.
NO_PREFIX_PACKAGE_PIXMAPS_DIR="${packagepixmapsdir}"
AC_SUBST(NO_PREFIX_PACKAGE_PIXMAPS_DIR)
PACKAGE_PIXMAPS_DIR="${packageprefix}/${packagepixmapsdir}"
AC_SUBST(PACKAGE_PIXMAPS_DIR)

dnl Subst PACKAGE_HELP_DIR.
NO_PREFIX_PACKAGE_HELP_DIR="${packagehelpdir}"
AC_SUBST(NO_PREFIX_PACKAGE_HELP_DIR)
PACKAGE_HELP_DIR="${packageprefix}/${packagehelpdir}"
AC_SUBST(PACKAGE_HELP_DIR)

dnl Subst PACKAGE_MENU_DIR.
NO_PREFIX_PACKAGE_MENU_DIR="${packagemenudir}"
AC_SUBST(NO_PREFIX_PACKAGE_MENU_DIR)
PACKAGE_MENU_DIR="${packageprefix}/${packagemenudir}"
AC_SUBST(PACKAGE_MENU_DIR)

AC_DEFINE_UNQUOTED(PACKAGE_DATA_DIR, "${packageprefix}/${packagedatadir}", [Package data directory])
AC_DEFINE_UNQUOTED(PACKAGE_DOC_DIR, "${packageprefix}/${packagedocdir}", [Package doc directory])
AC_DEFINE_UNQUOTED(PACKAGE_PIXMAPS_DIR, "${packageprefix}/${packagepixmapsdir}", [Package pixmaps directory])
AC_DEFINE_UNQUOTED(PACKAGE_HELP_DIR, "${packageprefix}/${packagehelpdir}", [Package help directory])
AC_DEFINE_UNQUOTED(PACKAGE_MENU_DIR, "${packageprefix}/${packagemenudir}", [Package menu directory])
AC_DEFINE_UNQUOTED(PACKAGE_SOURCE_DIR, "${packagesrcdir}", [Package source directory])

AC_OUTPUT([ Makefile
	  intl/Makefile
          m4/Makefile 
          po/Makefile.in
          src/Makefile ])

