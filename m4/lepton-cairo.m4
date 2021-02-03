# lepton-cairo.m4                                           -*-Autoconf-*-
# serial 1

dnl Check for CAIRO
dnl Copyright (C) 2012  Peter Brett <peter@peter-b.co.uk>
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

AC_DEFUN([AX_CHECK_CAIRO],
[
  AC_PREREQ([2.60])dnl

  # In Cairo >= 1.10, need to check for Cairo PDF/SVG/PS/PNG support
  # separately.
  PKG_CHECK_MODULES([CAIRO], [cairo >= 1.10], [CAIRO=yes], [CAIRO=no])
  if test "$CAIRO" = "yes"; then
    PKG_CHECK_MODULES([CAIRO_PNG], [cairo-png >= 1.10], ,
      AC_MSG_ERROR([Cairo PNG support 1.10.0 or later is required.]))
    PKG_CHECK_MODULES([CAIRO_PDF], [cairo-pdf >= 1.10], ,
      AC_MSG_ERROR([Cairo PDF support 1.10.0 or later is required.]))
    PKG_CHECK_MODULES([CAIRO_PS], [cairo-ps >= 1.10], ,
      AC_MSG_ERROR([Cairo PostScript support 1.10.0 or later is required.]))
    PKG_CHECK_MODULES([CAIRO_SVG], [cairo-svg >= 1.10], ,
      AC_MSG_ERROR([Cairo SVG support 1.10.0 or later is required.]))
  else
    PKG_CHECK_MODULES([CAIRO], [cairo >= 1.8], [],
      AC_MSG_ERROR([Cairo 1.8.0 or later is required.]))
  fi

])dnl AX_CHECK_CAIRO
