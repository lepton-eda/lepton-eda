# geda-contrib.m4                 -*- Autoconf -*-
# serial 1

dnl Optionally disable build and install of contributed software.
dnl Copyright (C) 2017 Gareth Edwards <gareth@edwardsfamily.org.uk>
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

# Check whether to enable building of contributed software
AC_DEFUN([AX_OPTION_CONTRIB],dnl
[AC_PREREQ([2.60])dnl

  AC_MSG_CHECKING([whether to enable build of contributed software])
  AC_ARG_ENABLE([contrib],
    [AS_HELP_STRING([--enable-contrib], [enable building of contributed software])])

  if test "X$enable_contrib" = "Xyes"; then
    AC_MSG_RESULT([yes])
  else
    AC_MSG_RESULT([no])
  fi

  AM_CONDITIONAL([ENABLE_CONTRIB], test "X$enable_contrib" = "Xyes")

])dnl AX_OPTION_CONTRIB
