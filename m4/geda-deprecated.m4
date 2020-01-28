# geda-deprecated.m4                                              -*-Autoconf-*-
# serial 1.0

dnl Option for deprecated features
dnl Copyright (C) 2016  Peter Brett <peter@peter-b.co.uk>
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

# Check whether to enable deprecated features or behaviour
AC_DEFUN([AX_OPTION_DEPRECATED],dnl
[AC_PREREQ([2.60])dnl

  AC_MSG_CHECKING([whether to enable deprecated features])
  AC_ARG_ENABLE([deprecated],
    [AS_HELP_STRING([--disable-deprecated], [disable deprecated features])],
    [], [enable_deprecated="yes"])

  if test "X$enable_deprecated" != "Xno"; then
    AC_MSG_RESULT([yes])
    AC_DEFINE([ENABLE_DEPRECATED], 1,
      [Define to 1 if deprecated features or behaviour should be enabled.])
  else
    AC_MSG_RESULT([no])
  fi
])dnl AX_OPTION_DEPRECATED
