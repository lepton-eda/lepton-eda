# geda-groff.m4                                         -*-Autoconf-*-
# serial 1

dnl Look for GNU troff
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

AC_DEFUN([AX_PROG_GROFF],
[
  AC_PREREQ([2.60])dnl
  AC_ARG_VAR([GROFF], [Path to groff executable])
  AC_CHECK_PROG([GROFF], [groff], [groff], [no])

  # Some distros are annoying and package groff into a crippled
  # version without many of the drivers, along with a separate full
  # version.  So explicitly check for the html driver.
  if test "X$GROFF" != "Xno" ; then
    AC_MSG_CHECKING([whether $GROFF has an html driver])
    echo '.TH conftest 1 "January 1st, 1970" Version 1' > conftest.1
    if LC_NUMERIC=C $GROFF -man -T html > /dev/null 2>&AS_MESSAGE_LOG_FD; then
      HAVE_GROFF_HTML=yes
    else
      HAVE_GROFF_HTML=no
    fi
    rm -f conftest.1
    AC_MSG_RESULT([$HAVE_GROFF_HTML])
  fi

  AM_CONDITIONAL([ENABLE_GROFF_HTML],
                 [test "X$HAVE_GROFF_HTML" != Xno])
])dnl AX_PROG_GROFF
