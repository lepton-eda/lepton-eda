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

AM_CPPFLAGS = -I$(top_srcdir)/include
AM_CFLAGS = -Wall -W -Werror -Wno-empty-body
LDADD = $(top_builddir)/src/storage/libxornstorage.la

TEST_EXTENSIONS = .py

AM_TESTS_ENVIRONMENT = \
	PYTHONPATH='$(top_builddir)/built-packages'; \
	export PYTHONPATH;
PY_LOG_COMPILER = $(PYTHON)

check_PROGRAMS = \
	storage/snippets/example \
	storage/snippets/functions \
	storage/snippets/motivation \
	storage/copy_attached \
	storage/copy_object \
	storage/copy_objects \
	storage/delete_sel \
	storage/exist \
	storage/get_attribute \
	storage/get_location \
	storage/get_obdata \
	storage/get_objects \
	storage/get_obtype \
	storage/invalid_obtype \
	storage/is_selected \
	storage/multiple_assignments \
	storage/null \
	storage/pointer \
	storage/query_attached \
	storage/reloc_attach \
	storage/reloc_order \
	storage/select_by_attribute \
	storage/selection \
	storage/set_attribute \
	storage/set_obdata \
	storage/stat \
	storage/string \
	storage/transient

pythontests = \
	cpython/snippets/functions.py \
	cpython/snippets/guile.py \
	cpython/copy_attached.py \
	cpython/copy_object.py \
	cpython/copy_objects.py \
	cpython/data_ctors.py \
	cpython/delete_sel.py \
	cpython/exist.py \
	cpython/get_loc_attach.py \
	cpython/get_loc_order.py \
	cpython/get_obdata.py \
	cpython/get_objects.py \
	cpython/get_obtype.py \
	cpython/is_selected.py \
	cpython/module.py \
	cpython/ob_equality.py \
	cpython/pointer.py \
	cpython/query_attached.py \
	cpython/reloc_attach.py \
	cpython/reloc_order.py \
	cpython/selection.py \
	cpython/set_obdata.py \
	cpython/stat.py \
	cpython/string.py \
	cpython/transient.py \
	cpython/guile.py \
	python/base64.py \
	python/proxy.py \
	python/geda/attrib.py \
	python/geda/complex.py \
	python/geda/parse_attrib.py \
	python/geda/read_log.py \
	python/geda/ripperdir.py

testdata = \
	python/geda/ripperdir.sch

TESTS = $(check_PROGRAMS) $(pythontests)

EXTRA_DIST = storage/Setup.h cpython/Setup.py $(pythontests) $(testdata)
