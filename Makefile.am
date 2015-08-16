# Copyright (C) 2013-2015 Roland Lutz
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

SUBDIRS = \
	po \
	src/storage \
	src/cpython \
	src/python \
	src/command \
	src/backend \
	tests

include_HEADERS = include/xornstorage.h

nodist_noinst_PYTHON = built-packages/xorn.py
all-local: $(nodist_noinst_PYTHON)
built-packages/xorn.py: Makefile
	$(AM_V_GEN)$(MKDIR_P) built-packages && { \
	    echo '__path__ = ["$(abs_builddir)/src/cpython/.libs",'; \
	    echo '            "$(abs_builddir)/src/python",'; \
	    echo '            "$(abs_srcdir)/src/python"]'; \
	} > $@

CLEANFILES = \
	built-packages/xorn.py \
	built-packages/xorn.pyc

ACLOCAL_AMFLAGS = -I m4

EXTRA_DIST = \
	pprint-chlog \
	doc/HACKING \
	doc/mainpage.md \
	doc/storage.md \
	doc/api/Doxyfile \
	doc/api/storage.dox \
	doc/api/using-storage.md \
	doc/geda/colors.md \
	doc/geda/coordinatespace.jpg \
	doc/geda/data.dox \
	doc/geda/file_format_spec \
	doc/geda/fileformat_textgraphic.jpg \
	doc/geda/filltype.svg \
	doc/geda/linetype.svg \
	doc/geda/path_example_and_gate-smaller.png

dist-hook:
	if test -d $(srcdir)/.git; \
	then	chmod u+w $(distdir)/ChangeLog; \
		( cd $(srcdir); ./pprint-chlog ) > $(distdir)/ChangeLog; \
	fi

doxygen:
	cd $(srcdir)/doc/api && doxygen

.PHONY: doxygen
