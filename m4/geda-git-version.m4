# geda-git-version.m4                                   -*-Autoconf-*-
# serial 2

dnl Extract gEDA version parameters from a git repository, if present.
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

# AX_GIT_VERSION DATE-VERSION
# Check for a git repository. If present, sanity check the latest tag
# name. This probably isn't strictly necessary.
AC_DEFUN([AX_GIT_VERSION],
[
  AC_PREREQ([2.60])dnl
  AC_REQUIRE([AX_GIT_REPO])dnl

  # First split version specified with AC_INIT into dotted and date
  # parts
changequote(,)
  init_sed_pattern="^\([\.0-9]*\)\.\([0-9]*\)$"
changequote([,])
  DOTTED_VERSION="$PACKAGE_VERSION"
  DATE_VERSION="$1"

  # If we're in a git repository, get the detailed version from git
  # describe.
  if test "X$HAVE_GIT_REPO" = "Xyes"; then
    AC_MSG_CHECKING([version from git repository])
    GIT_VERSION=`cd $srcdir && $GIT describe`
    AC_MSG_RESULT([$GIT_VERSION])
  fi

  # If there's an annotated tag available, test that the git version
  # and AC_INIT versions agree.
  if (cd $srcdir && git describe > /dev/null); then
changequote(,)
    git_sed_pattern="^\([^-]*\)-\([^-]*\).*"
changequote([,])
    GIT_DOTTED_VERSION=`echo $GIT_VERSION | sed -e"s/$git_sed_pattern/\1/"`
    GIT_DATE_VERSION=`echo $GIT_VERSION | sed -e"s/$git_sed_pattern/\2/"`

    if (test "X$GIT_DOTTED_VERSION" != "X$DOTTED_VERSION") ||
       (test "X$GIT_DATE_VERSION" != "X$DATE_VERSION");
    then

      AC_MSG_WARN([The latest git tag name doesn't appear to match the version specified
by the configure script.])
    fi
  fi

  AC_SUBST([DOTTED_VERSION])
  AC_SUBST([DATE_VERSION])

  # We generate some files differently if a git repo is present
  AM_CONDITIONAL([HAVE_GIT_REPO], test "X$HAVE_GIT_REPO" = "Xyes")

])dnl AX_GIT_VERSION
