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

nobase_pkgpython_PYTHON = \
	__init__.py \
	base64.py \
	command.py \
	fileutils.py \
	proxy.py \
	geda/__init__.py \
	geda/attrib.py \
	geda/clib.py \
	geda/fileformat.py \
	geda/plainformat.py \
	geda/plainread.py \
	geda/plainwrite.py \
	geda/read.py \
	geda/ref.py \
	geda/write.py \
	geda/netlist/__init__.py \
	geda/netlist/backend.py \
	geda/netlist/blueprint.py \
	geda/netlist/conn.py \
	geda/netlist/guile.py \
	geda/netlist/instance.py \
	geda/netlist/net.py \
	geda/netlist/netlist.py \
	geda/netlist/package.py \
	geda/netlist/pp_graphical.py \
	geda/netlist/pp_hierarchy.py \
	geda/netlist/pp_netattrib.py \
	geda/netlist/pp_slotting.py \
	geda/netlist/slib.py

noinst_PYTHON = storage.py

nodist_pkgpython_PYTHON = config.py
EXTRA_DIST = config.py.in
CLEANFILES = config.py config.pyc

# Make sure config.py is available when running from builddir
all-local: $(nodist_pkgpython_PYTHON)

config.py: $(srcdir)/config.py.in Makefile
	$(AM_V_GEN)sed \
	    -e 's,[@]PACKAGE[@],$(PACKAGE),g' \
	    -e 's,[@]PACKAGE_BUGREPORT[@],$(PACKAGE_BUGREPORT),g' \
	    -e 's,[@]PACKAGE_NAME[@],$(PACKAGE_NAME),g' \
	    -e 's,[@]PACKAGE_STRING[@],$(PACKAGE_STRING),g' \
	    -e 's,[@]PACKAGE_TARNAME[@],$(PACKAGE_TARNAME),g' \
	    -e 's,[@]PACKAGE_URL[@],$(PACKAGE_URL),g' \
	    -e 's,[@]PACKAGE_VERSION[@],$(PACKAGE_VERSION),g' \
	    -e 's,[@]VERSION[@],$(VERSION),g' \
	    -e 's,[@]localedir[@],$(localedir),g' \
		$(srcdir)/$@.in > $@
