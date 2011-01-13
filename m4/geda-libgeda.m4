# geda-libgeda.m4                                       -*-Autoconf-*-
# serial 1.0

dnl libgeda-specific setup
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

# Work out the gettext domain that libgeda should use
AC_DEFUN([AX_LIBGEDA],
[
  AC_PREREQ([2.60])dnl

  # First argument is the shared library version to use.
  AC_MSG_CHECKING([libgeda shared library version])
  AC_MSG_RESULT($1)
  AC_SUBST([LIBGEDA_SHLIB_VERSION], $1)

  # Work out the gettext domain to use
  AC_MSG_CHECKING([libgeda gettext domain])
  so_major=`echo $LIBGEDA_SHLIB_VERSION | sed -e "s/:.*//"`
  LIBGEDA_GETTEXT_DOMAIN="libgeda$so_major"
  AC_MSG_RESULT([$LIBGEDA_GETTEXT_DOMAIN])
  AC_SUBST([LIBGEDA_GETTEXT_DOMAIN])
  AC_DEFINE_UNQUOTED([LIBGEDA_GETTEXT_DOMAIN], ["$LIBGEDA_GETTEXT_DOMAIN"],
    "Name of libgeda's gettext domain.")
])
