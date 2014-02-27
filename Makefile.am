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

SUBDIRS = src/storage tests/storage

include_HEADERS = include/xornstorage.h

ACLOCAL_AMFLAGS = -I m4

EXTRA_DIST = pprint-chlog doc/HACKING doc/api/Doxyfile

dist-hook:
	if test -d $(srcdir)/.git; \
	then	chmod u+w $(distdir)/ChangeLog; \
		( cd $(srcdir); ./pprint-chlog ) > $(distdir)/ChangeLog; \
	fi

doxygen:
	cd $(srcdir)/doc/api && doxygen

.PHONY: doxygen
