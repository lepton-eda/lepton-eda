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

import xorn.geda.attrib

def try_parse(string):
    try:
        name, value = xorn.geda.attrib.parse_string(string)
    except xorn.geda.attrib.MalformedAttributeError:
        return None
    else:
        return name, value

assert try_parse('') == None
assert try_parse('foobar') == None
assert try_parse('=') == None
assert try_parse('=bar') == None
assert try_parse('foo=') == None
assert try_parse('foo=bar') == ('foo', 'bar')
assert try_parse('==') == None
assert try_parse('==baz') == None
assert try_parse('=bar=') == None
assert try_parse('=bar=baz') == None
assert try_parse('foo==') == ('foo', '=')
assert try_parse('foo==baz') == ('foo', '=baz')
assert try_parse('foo=bar=') == ('foo', 'bar=')
assert try_parse('foo=bar=baz') == ('foo', 'bar=baz')
assert try_parse(' foo=bar') == (' foo', 'bar')
assert try_parse('foo =bar') == None
assert try_parse('foo= bar') == None
assert try_parse('foo=bar ') == ('foo', 'bar ')
