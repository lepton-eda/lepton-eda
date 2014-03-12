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

AM_CPPFLAGS = -I$(top_srcdir)/include
AM_CFLAGS = -Wall -W -Werror -Wno-empty-body
LDADD = ../../src/storage/libxornstorage.la

TESTS = snippets/example \
	snippets/functions \
	snippets/motivation \
	copy_object \
	copy_objects \
	delete_sel \
	exist \
	get_attribute \
	get_location \
	get_obdata \
	get_objects \
	get_obtype \
	invalid_obtype \
	is_selected \
	multiple_assignments \
	null \
	query_attached \
	reloc_attach \
	reloc_order \
	select_by_attribute \
	selection \
	set_attribute \
	set_obdata \
	stat \
	string \
	transient

check_PROGRAMS = $(TESTS)
EXTRA_DIST = Setup.h
