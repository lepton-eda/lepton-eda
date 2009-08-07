# geda-groff.m4                                         -*-Autoconf-*-
# serial 1

dnl Carry out configuration tasks needed by desktop-i18n tool
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

AC_DEFUN([AX_PROG_GROFF],
[
  AC_PREREQ([2.60])dnl
  AC_ARG_VAR([GROFF], [Path to groff executable])
  AC_CHECK_PROG([GROFF], [groff], [groff], [no])

  AM_CONDITIONAL([ENABLE_GROFF],
                 [test "X$groff" != Xno])
])dnl AX_PROG_GROFF
