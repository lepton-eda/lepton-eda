# Copyright (C) 2013-2016 Roland Lutz
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software Foundation,
# Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

AM_CPPFLAGS = -I/usr/include/python$(PYTHON_VERSION) \
	      -pthread -I/usr/include/guile/2.0
AM_CFLAGS = -Wall -W -Wno-unused-parameter -fno-strict-aliasing

pkgpyexec_LTLIBRARIES = guilemodule.la
guilemodule_la_SOURCES = \
	module.c \
	module.h \
	proc.c \
	py2scm.c \
	scm2py.c
guilemodule_la_LDFLAGS = -module -avoid-version \
			 -export-symbols-regex '^initguile$$' \
			 -lguile-2.0 -lgc
