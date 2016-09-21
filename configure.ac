# Process this file with autoconf to produce a configure script.
# Copyright (C) 2013-2016 Roland Lutz
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
# along with this program; if not, write to the Free Software Foundation,
# Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

AC_PREREQ([2.61])
LT_PREREQ([2.4.2])

AC_INIT([Xorn], [0.0], [bug-xorn@hedmen.org], [], [http://hedmen.org/xorn/])
AC_CONFIG_SRCDIR([src/cpython/module.c])
AC_CONFIG_MACRO_DIR([m4])

AM_INIT_AUTOMAKE([-Wall -Werror silent-rules subdir-objects])
m4_ifdef([AM_PROG_AR], [AM_PROG_AR])
LT_INIT([])

AC_PROG_CXX

AM_PATH_PYTHON([2.7])
PKG_CHECK_MODULES([PYTHON], [python2 >= 2.7])
PKG_CHECK_MODULES([GUILE], [guile-2.0])

AM_GNU_GETTEXT([external])
AM_GNU_GETTEXT_VERSION([0.18.2])

# gEDA is planning to move this to $datarootdir/geda-gaf
AC_MSG_CHECKING([where to look for gEDA shared data])
AC_ARG_WITH(
	[geda-datadir],
	[AS_HELP_STRING(
		[--with-geda-datadir@<:@=DIR@:>@],
		[where to look for shared gEDA data @<:@DATAROOTDIR/gEDA@:>@])], [
	if test "x$with_geda_datadir" == "xno"
	then	gedadatadir=""
		AC_MSG_RESULT([no default])
	else	if test "x$with_geda_datadir" = "xyes"
		then	gedadatadir="$datarootdir/gEDA"
		else	gedadatadir="$with_geda_datadir"
		fi
		AC_MSG_RESULT([$gedadatadir])
	fi
], [
	gedadatadir="$datarootdir/gEDA"
	AC_MSG_RESULT([$gedadatadir])
])
AS_IF([test "x$gedadatadir" = "x"],
      [AC_MSG_ERROR([No gEDA shared data location given])])
AC_SUBST([gedadatadir])

# gEDA is planning to move this to $sysconfdir/geda-gaf
AC_MSG_CHECKING([where to look for gEDA rc files])
AC_ARG_WITH(
	[geda-sysconfdir],
	[AS_HELP_STRING(
		[--with-geda-sysconfdir@<:@=DIR@:>@],
		[where to look for system-wide gEDA configuration @<:@SYSCONFDIR/gEDA@:>@])], [
	if test "x$with_geda_sysconfdir" == "xno"
	then	gedasysconfdir=""
		AC_MSG_RESULT([no default])
	else	if test "x$with_geda_sysconfdir" = "xyes"
		then	gedasysconfdir="$sysconfdir/gEDA"
		else	gedasysconfdir="$with_geda_sysconfdir"
		fi
		AC_MSG_RESULT([$gedasysconfdir])
	fi
], [
	gedasysconfdir="$sysconfdir/gEDA"
	AC_MSG_RESULT([$gedasysconfdir])
])
AC_SUBST([gedasysconfdir])

AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([
	Makefile
	po/Makefile.in
	src/storage/Makefile
	src/cpython/Makefile
	src/cpython/guile/Makefile
	src/python/Makefile
	src/command/Makefile
	src/backend/Makefile
	tests/Makefile
	tests/netlist/Makefile
])
AC_OUTPUT
