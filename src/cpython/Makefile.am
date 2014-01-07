# Copyright (C) 2013, 2014 Roland Lutz
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

AM_CPPFLAGS = -I/usr/include/python$(PYTHON_VERSION) -I$(top_srcdir)/include
AM_CFLAGS = -Wall -W -Wno-unused-parameter -fno-strict-aliasing

AM_V_M4 = $(am__v_M4_@AM_V@)
am__v_M4_ = $(am__v_M4_@AM_DEFAULT_V@)
am__v_M4_0 = @echo "  M4      " $@;
am__v_M4_1 =

m4sources = \
	data_arc.m4 \
	data_box.m4 \
	data_circle.m4 \
	data_component.m4 \
	data_line.m4 \
	data_net.m4 \
	data_path.m4 \
	data_picture.m4 \
	data_text.m4 \
	data_lineattr.m4 \
	data_fillattr.m4

m4output = \
	data.h \
	data_arc.c \
	data_box.c \
	data_circle.c \
	data_component.c \
	data_line.c \
	data_net.c \
	data_path.c \
	data_picture.c \
	data_text.c \
	data_lineattr.c \
	data_fillattr.c

data.h: include_h.m4 $(m4sources)
	$(AM_V_M4)m4 -P $^ > $@

.m4.c:
	$(AM_V_M4)m4 -P $(srcdir)/include_c.m4 $< > $@

data_arc.c: include_c.m4
data_box.c: include_c.m4
data_circle.c: include_c.m4
data_component.c: include_c.m4
data_line.c: include_c.m4
data_net.c: include_c.m4
data_path.c: include_c.m4
data_picture.c: include_c.m4
data_text.c: include_c.m4
data_lineattr.c: include_c.m4
data_fillattr.c: include_c.m4

EXTRA_DIST = include_h.m4 include_c.m4 $(m4output)

BUILT_SOURCES = data.h

pkgpyexec_LTLIBRARIES = storagemodule.la
storagemodule_la_SOURCES = \
	$(m4sources) \
	module.c \
	module.h \
	object.c \
	revision.c \
	selection.c
storagemodule_la_LIBADD = ../storage/libxornstorage.la
storagemodule_la_LDFLAGS = -module -avoid-version \
			   -export-symbols-regex '^initstorage$$'
