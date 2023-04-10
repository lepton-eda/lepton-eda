# lepton-doc.m4                                       -*-Autoconf-*-
# serial 1.0

dnl Optionally disable documentation.
dnl Copyright (C) 2023 Lepton EDA Contributors
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

# Check if documentation should be disabled
AC_DEFUN([AX_OPTION_DOC],
[
  AC_PREREQ([2.60])dnl

  AC_MSG_CHECKING([whether to enable documentation])
  AC_ARG_ENABLE([doc],
    [AS_HELP_STRING([--disable-doc], [turn off building and installing documentation])],
    [], [enable_doc="yes"])

  if test "X$enable_doc" = "Xyes"; then
    AC_MSG_RESULT([yes])
  else
    AC_MSG_RESULT([no])
  fi

  AM_CONDITIONAL([ENABLE_DOC], test "X$enable_doc" = "Xyes")

])dnl AX_OPTION_DOC
