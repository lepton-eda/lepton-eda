# geda-gcc-flags.m4                                     -*-Autoconf-*-
# serial 1.0

dnl Add GCC-specific C compiler flags
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
dnl Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

# Adds the argument to GCC_CFLAGS if the compiler is GCC.
AC_DEFUN([AX_GCC_FLAGS],
[
  AC_PREREQ([2.60])dnl
  AC_REQUIRE([AC_PROG_CC])dnl

  if test "X$GCC" = "Xyes"; then
    GCC_CFLAGS="$1 $GCC_CFLAGS"
  fi

  AC_SUBST([GCC_CFLAGS])
])
