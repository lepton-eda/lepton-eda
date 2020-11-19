# lepton-attrib.m4                                       -*-Autoconf-*-
# serial 1.0

dnl Check for dependencies for attrib and optionally disable it.
dnl Copyright (C) 2009  Dan McMahill <dan@mcmahill.net>
dnl Copyright (C) 2019  Lepton EDA Contributors
dnl
dnl This program is free software; you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free Software Foundation; either version 2 of the License, or
dnl (at your option) any later version.
dnl
dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with this program; if not, write to the Free Software
dnl Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

# Check if attrib should be disabled
AC_DEFUN([AX_OPTION_ATTRIB],
[
  AC_PREREQ([2.60])dnl

  AC_MSG_CHECKING([whether to enable lepton-attrib])
  AC_ARG_ENABLE([attrib],
    [AS_HELP_STRING([--disable-attrib], [turn off building and installing lepton-attrib])],
    [], [enable_attrib="yes"])

  if test "X$enable_attrib" = "Xyes"; then
    AC_MSG_RESULT([yes])
  else
    AC_MSG_RESULT([no])
  fi

  AM_CONDITIONAL([ENABLE_ATTRIB], test "X$enable_attrib" = "Xyes")

  AS_IF([test "X$enable_attrib" = "Xyes"],
  [
    AS_CASE(["$with_gtk3"],
      # Use GTK3 and libgtksheet.
      [yes],
        [PKG_CHECK_MODULES(GTKSHEET, [gtksheet-4.0 >= 4.0.0], ,
          AC_MSG_ERROR([GTKSHEET 4.0.0 or later is required.]))],
      # Default case: use GTK2 and libgtkextra.
      [PKG_CHECK_MODULES(GTKEXTRA, [gtkextra-3.0 >= 3.0.0], ,
        AC_MSG_ERROR([GTKEXTRA 3.0.0 or later is required.]))])
  ])

])dnl AX_OPTION_ATTRIB
