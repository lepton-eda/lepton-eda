# liblepton.m4                                       -*-Autoconf-*-
# serial 1.0

dnl liblepton-specific setup
dnl Copyright (C) 2009  Peter Brett <peter@peter-b.co.uk>
dnl Copyright (C) 2019-2021 Lepton EDA Contributors
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

# Work out the gettext domain that liblepton should use
AC_DEFUN([AX_LIBLEPTON],
[
  AC_PREREQ([2.60])dnl

  # First argument is the shared library version to use.
  AC_MSG_CHECKING([liblepton shared library version])
  AC_MSG_RESULT($1)
  AC_SUBST([LIBLEPTON_SHLIB_VERSION], $1)

  VER_MAJOR=`echo ${LIBLEPTON_SHLIB_VERSION} | sed 's;:.*;;g'`
  AC_SUBST([LIBLEPTON_MAJOR], $VER_MAJOR)

  # Work out the gettext domain to use
  AC_MSG_CHECKING([liblepton gettext domain])
  LIBLEPTON_GETTEXT_DOMAIN="liblepton"
  AC_MSG_RESULT([$LIBLEPTON_GETTEXT_DOMAIN])
  AC_SUBST([LIBLEPTON_GETTEXT_DOMAIN])
  AC_DEFINE_UNQUOTED([LIBLEPTON_GETTEXT_DOMAIN], ["$LIBLEPTON_GETTEXT_DOMAIN"],
    "Name of liblepton's gettext domain.")
])
