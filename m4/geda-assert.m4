# geda-assert.m4                                        -*-Autoconf-*-
# serial 1.0

dnl Optionally disable assertions
dnl Copyright (C) 2009  Peter Brett <peter@peter-b.co.uk>
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

# Check if assertions should be disabled.
AC_DEFUN([AX_OPTION_ASSERT],
[
  AC_PREREQ([2.60])dnl
  AC_PROVIDE([AC_HEADER_ASSERT])dnl

  AC_MSG_CHECKING([whether to enable assertions])
  AC_ARG_ENABLE([assert],
    [AS_HELP_STRING([--disable-assert], [turn off assertions])],
    [], [enable_assert="yes"])

  if test "X$enable_assert" = "Xyes"; then
    AC_MSG_RESULT([yes])
  else
    AC_MSG_RESULT([no])
    AC_DEFINE([NDEBUG], 1, [Define to 1 if assertions should be disabled.])
    AC_DEFINE([G_DISABLE_ASSERT], 1, [Define to 1 if GLib assertions should be disabled.])
  fi
])dnl AX_OPTION_ASSERT
