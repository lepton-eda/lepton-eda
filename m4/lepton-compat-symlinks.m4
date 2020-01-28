# lepton-compat-symlinks.m4                                       -*-Autoconf-*-
# serial 1.0

dnl Option for installing compatibility symlinks for gEDA/gaf programs
dnl Copyright (C) 2017  Peter Brett <peter@peter-b.co.uk>
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
dnl
dnl How to use this in a Makefile.am file to install a compatibility
dnl symlink:
dnl
dnl     if INSTALL_COMPAT_SYMLINKS
dnl     install-exec-hook:
dnl             cd $(DESTDIR)$(bindir) && \
dnl             ln -s lepton-cli$(EXEEXT) gaf$(EXEEXT)
dnl     uninstall-hook:
dnl             rm -f $(DESTDIR)$(bindir)/gaf$(EXEEXT)
dnl     endif INSTALL_COMPAT_SYMLINKS

# Check whether to install compatibility symlinks for gEDA/gaf programs
AC_DEFUN([AX_OPTION_COMPAT_SYMLINKS],[dnl
  AC_PREREQ([2.60])dnl
  AC_REQUIRE([AC_PROG_LN_S])dnl

  AC_MSG_CHECKING([whether to install geda-gaf compatibility symlinks])
  AC_ARG_ENABLE([compat-symlinks],
    [AS_HELP_STRING([--enable-compat-symlinks], [install geda-gaf compatibility symlinks])],
    [], [enable_compat_symlinks="no"])

  if test "X$enable_compat_symlinks" = "Xyes"; then
    AC_MSG_RESULT([yes])
  else
    AC_MSG_RESULT([no])
  fi

  AM_CONDITIONAL([INSTALL_COMPAT_SYMLINKS],
                 [test "X$enable_compat_symlinks" = "Xyes"])
])dnl AX_OPTION_COMPAT_SYMLINKS
