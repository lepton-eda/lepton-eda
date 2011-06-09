# geda-git.m4                                           -*-Autoconf-*-
# serial 2

dnl Generic checks relating to git tools & repositories
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
dnl Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

# Check if git is available, and which git to use.
AC_DEFUN([AX_PROG_GIT],
[
  AC_PREREQ([2.60])dnl
  AC_ARG_VAR([GIT], [Path to git executable])
  AC_CHECK_PROG([GIT], [git], [git], [no])
])dnl AX_PROG_GIT

# Check if the source directory is a git repository
AC_DEFUN([AX_GIT_REPO],
[
  AC_PREREQ([2.60])dnl
  AC_REQUIRE([AX_PROG_GIT])dnl

  HAVE_GIT_REPO=no
  if test "X$GIT" != "Xno"; then
    AC_MSG_CHECKING([if the source directory is a git repository])
    if (cd $srcdir && $GIT rev-parse --git-dir > /dev/null 2>&1); then
      AC_MSG_RESULT([yes])
      HAVE_GIT_REPO=yes
    else
      AC_MSG_RESULT([no])
    fi
  fi

  AC_SUBST([HAVE_GIT_REPO])
])dnl AX_GIT_REPO
