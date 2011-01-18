# geda-desktop-i18n.m4                                  -*-Autoconf-*-
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
dnl Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

AC_DEFUN([AX_DESKTOP_I18N],
[
  AC_PREREQ([2.60])dnl
  AC_REQUIRE([AM_NLS])dnl
  AC_REQUIRE([AM_PO_SUBDIRS])dnl
  dnl Complain if desktop-i18n is missing
  AC_REQUIRE_AUX_FILE([desktop-i18n])

  # We need to check that GNU gettext is available
  AM_PATH_PROG_WITH_TEST([GETTEXT], [gettext],
    [$ac_dir/$ac_word --version | grep GNU > /dev/null], :)
  if test "x$GETTEXT" = x; then
    AC_MSG_ERROR([The GNU gettext program could not be found. Please ensure that GNU
gettext is correctly installed, and rerun configure.])
  fi
  AC_SUBST([GETTEXT])

  # How should desktop-i18n be run?
  DESKTOP_I18N_SCRIPT="$am_aux_dir/desktop-i18n"
  DESKTOP_I18N="\${SHELL} $DESKTOP_I18N_SCRIPT"
  AC_SUBST([DESKTOP_I18N_SCRIPT])
  AC_SUBST([DESKTOP_I18N])

  # We have a private directory to temporarily install locale data
  # into.
  DESKTOP_I18N_LOCALE_DIR='$(top_builddir)/.desktop-i18n'
  AC_SUBST([DESKTOP_I18N_LOCALE_DIR])

  # Substitute our own evil, corrupted version of xgettext.
  # Mwahahahaha!
  XGETTEXT="$DESKTOP_I18N --extract --xgettext=$XGETTEXT --"

  # Define some rules for substitution into Makefiles
  DESKTOP_I18N_CREATE='$(DESKTOP_I18N) --create --gettext=$(GETTEXT) --domain=$(DOMAIN) \
    --localedir=$(DESKTOP_I18N_LOCALE_DIR)/share/locale $(DESKTOP_I18N_LANGS)'
  DESKTOP_I18N_LANGS_RULE='DESKTOP_I18N_LANGS = $(addprefix --lang=,$(shell cat $(DESKTOP_I18N_LOCALE_DIR)/$(DOMAIN).LINGUAS))'
  DESKTOP_I18N_DESKTOP_RULE='%.desktop: %.desktop.in ; $(DESKTOP_I18N_CREATE) $< [$]@'
  DESKTOP_I18N_XML_RULE='%.xml: %.xml.in ; $(DESKTOP_I18N_CREATE) $< [$]@'

  AC_SUBST([DESKTOP_I18N_CREATE])
  _IT_SUBST([DESKTOP_I18N_LANGS_RULE])
  _IT_SUBST([DESKTOP_I18N_DESKTOP_RULE])
  _IT_SUBST([DESKTOP_I18N_XML_RULE])
])dnl AX_DESKTOP_I18N

# _IT_SUBST(VARIABLE)
# -------------------
# Abstract macro to do either _AM_SUBST_NOTMAKE or AC_SUBST
# Stolen from intltool.m4 (GPL v2)
AC_DEFUN([_IT_SUBST],
[
AC_SUBST([$1])
m4_ifdef([_AM_SUBST_NOTMAKE], [_AM_SUBST_NOTMAKE([$1])])
])dnl _IT_SUBST
