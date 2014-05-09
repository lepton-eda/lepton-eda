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

TESTS = snippets/functions.py \
	copy_attached.py \
	copy_object.py \
	copy_objects.py \
	data_ctors.py \
	delete_sel.py \
	exist.py \
	get_loc_attach.py \
	get_loc_order.py \
	get_obdata.py \
	get_objects.py \
	get_obtype.py \
	is_selected.py \
	module.py \
	pointer.py \
	query_attached.py \
	reloc_attach.py \
	reloc_order.py \
	selection.py \
	set_obdata.py \
	stat.py \
	string.py \
	transient.py

EXTRA_DIST = $(TESTS) Setup.py

AM_TESTS_ENVIRONMENT = \
	PYTHONPATH='$(top_builddir)/built-packages'; \
	export PYTHONPATH;
LOG_COMPILER = $(PYTHON)
