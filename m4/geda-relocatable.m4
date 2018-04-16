# geda-relocatable.m4                                             -*-Autoconf-*-
# serial 1.0

dnl Option for relocatable mode
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

# Check whether to hardcode paths into the built gEDA programs
AC_DEFUN([AX_OPTION_RELOCATABLE],dnl
[AC_PREREQ([2.60])dnl

  AC_MSG_CHECKING([whether to do a relocatable build])
  AC_ARG_ENABLE([relocatable],
    [AS_HELP_STRING([--enable-relocatable], [build relocatable binaries (not supported on BSD systems)])],
    [], [enable_relocatable="no"])

  if test "X$enable_relocatable" = "Xyes"; then
    AC_MSG_RESULT([yes])
    AC_DEFINE([ENABLE_RELOCATABLE], 1,
      [Define to 1 if the build should be relocatable.])
  else
    AC_MSG_RESULT([no])
  fi
])dnl AX_OPTION_RELOCATABLE
