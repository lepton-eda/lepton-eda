# geda-doxygen.m4                                       -*-Autoconf-*-
# serial 2

dnl Optional Doxygen API documentation support
dnl Copyright (C) 2009-2011  Peter Brett <peter@peter-b.co.uk>
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

# Check if doxygen documentation is requested, and if so, find doxygen program.
AC_DEFUN([AX_OPTION_DOXYGEN],
[
  AC_PREREQ([2.60])dnl
  AC_ARG_VAR([DOXYGEN], [Path to doxygen executable])

  # Check if the user enabled Doxygen
  AC_MSG_CHECKING([whether to enable generation of Doxygen API documentation])
  AC_ARG_ENABLE([doxygen],
    [AS_HELP_STRING([--enable-doxygen], [enable generation of Doxygen API documentation])])

  # If user enabled doxygen, find the path to the doxygen
  # executable. Also check for other required tools.
  if test "X$enable_doxygen" = "Xyes"; then
    AC_MSG_RESULT([yes])
    AC_CHECK_PROG([DOXYGEN], [doxygen], [doxygen], [no])
    if test "X$DOXYGEN" = "Xno"; then
      AC_MSG_ERROR([API documentation generation was requested, but doxygen was not
found. Ensure it is installed and in your path, or configure without
--enable-doxygen.])
    fi

    # Check for Inkscape
    AC_CHECK_PROG([INKSCAPE], [inkscape], [inkscape], [no])
    # Check for ImageMagick
    AC_CHECK_PROG([CONVERT], [convert], [convert], [no])

    # We need at least one way of converting SVG files to PNG files!
    if (test "X$INKSCAPE" = "Xno") && (test "X$CONVERT" = "Xno"); then
      AC_MSG_ERROR([API documentation generation was requested, but
neither Inkscape nor ImageMagick were found. Ensure one of these is
installed and in your path, or configure without --enable-doxygen.])
    fi

    # We need pdflatex to create PDF format API docs.
    AC_CHECK_PROG([PDFLATEX], [pdflatex], [pdflatex], [no])
    if test "X$PDFLATEX" = "Xno"; then
      AC_MSG_ERROR([API documentation generation was requested, but pdflatex was not
found.  Ensure it is installed and in your path, or configure without
--enable-doxygen.])
    fi

  else
    AC_MSG_RESULT([no])
  fi

  AM_CONDITIONAL([ENABLE_DOXYGEN],
                 test "X$enable_doxygen" = "Xyes")
  AC_SUBST([DOXYGEN])
  AC_SUBST([INKSCAPE])
  AC_SUBST([CONVERT])
])dnl AX_OPTION_DOXYGEN
