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

lib_LTLIBRARIES = libxornstorage.la
libxornstorage_la_SOURCES = xornstorage.cc key_iterator.h
libxornstorage_la_CPPFLAGS = -I$(top_srcdir)/include
libxornstorage_la_CXXFLAGS = -Wall -W
libxornstorage_la_LDFLAGS = -export-symbols-regex '^xorn_'
