# Copyright (C) 2013 Roland Lutz
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
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
AM_CFLAGS = -Wall -W -Werror
LDADD = ../lib/libxornstorage.la

TESTS = tc_copy_object \
	tc_copy_objects \
	tc_delete_sel \
	tc_exist \
	tc_get_obdata \
	tc_get_objects \
	tc_get_obtype \
	tc_invalid_obtype \
	tc_selection \
	tc_stat \
	tc_string

check_PROGRAMS = $(TESTS)
EXTRA_DIST = setup.h
