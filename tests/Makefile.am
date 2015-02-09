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

AM_CPPFLAGS = -I$(top_srcdir)/include
AM_CFLAGS = -Wall -W -Werror -Wno-empty-body
LDADD = $(top_builddir)/src/storage/libxornstorage.la

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

TESTS = $(check_PROGRAMS)
EXTRA_DIST = storage/Setup.h
