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

import StringIO
import xorn.xml_writer

def throws(fun, *args):
    try:
        fun(*args)
    except Exception as e:
        return type(e)

assert throws(xorn.xml_writer.valid_name, int()) == TypeError
assert throws(xorn.xml_writer.valid_name, str()) == TypeError
assert throws(xorn.xml_writer.valid_name, unicode()) == None

assert xorn.xml_writer.valid_name(u'') == False
assert xorn.xml_writer.valid_name(u'foo') == True
assert xorn.xml_writer.valid_name(u'f\xf6\xf6') == True
assert xorn.xml_writer.valid_name(u'!foo') == False
assert xorn.xml_writer.valid_name(u'foo!') == False
assert xorn.xml_writer.valid_name(u'-foo') == False
assert xorn.xml_writer.valid_name(u'foo-') == True

assert throws(xorn.xml_writer.escape, int()) == TypeError
assert throws(xorn.xml_writer.escape, str()) == TypeError
assert throws(xorn.xml_writer.escape, unicode()) == None

assert xorn.xml_writer.escape(u'"&<>foo') == u'&quot;&amp;&lt;&gt;foo'

FIRST_LINE = u'<?xml version="1.0" encoding="UTF-8"?>'

def perform_test(*commands):
    f = StringIO.StringIO()
    w = xorn.xml_writer.XMLWriter(f.write)

    for command in commands:
        assert not w.is_done()
        getattr(w, command[0])(*command[1:])

    assert w.is_done()
    data = f.getvalue()
    f.close()

    assert data.startswith(FIRST_LINE)
    assert data.endswith('\n')
    return data[len(FIRST_LINE):]

# nothing written
assert throws(perform_test,
) == AssertionError

# not all elements closed
assert throws(perform_test,
    ('start_element', 'root'),
) == AssertionError

# closing element at root level
assert throws(perform_test,
    ('end_element', ),
) == ValueError

assert perform_test(
    ('start_element', 'root'),
    ('end_element', )
) == '''
<root/>
'''

# invalid element name
assert throws(perform_test,
    ('start_element', '"root"'),
    ('end_element', )
) == ValueError

# multiple root elements
f = StringIO.StringIO()
w = xorn.xml_writer.XMLWriter(f.write)
w.start_element('root')
w.end_element()
assert throws(w.start_element, 'root') == ValueError
f.close()

assert perform_test(
    ('start_element', 'root'),
      ('start_element', 'foo'),
      ('end_element', ),
    ('end_element', )
) == '''
<root>
  <foo/>
</root>
'''

assert perform_test(
    ('start_element', 'root', False),
      ('start_element', 'foo'),
      ('end_element', ),
    ('end_element', )
) == '''
<root>
  <foo/>
</root>
'''

assert perform_test(
    ('start_element', 'root', True),
      ('start_element', 'foo'),
      ('end_element', ),
    ('end_element', )
) == '''
<root><foo/></root>
'''

### writing attributes ###

# attribute outside of element
assert throws(perform_test,
    ('write_attribute', 'foo', 'bar'),
) == ValueError

assert perform_test(
    ('start_element', 'root'),
      ('write_attribute', 'foo', 'bar'),
    ('end_element', )
) == '''
<root foo="bar"/>
'''

assert perform_test(
    ('start_element', 'root'),
      ('write_attribute', 'foo', 'bar'),
      ('write_attribute', 'bar', 'baz'),
    ('end_element', )
) == '''
<root foo="bar" bar="baz"/>
'''

# duplicate attribute
assert throws(perform_test,
    ('start_element', 'root'),
      ('write_attribute', 'foo', 'bar'),
      ('write_attribute', 'foo', 'bar'),
    ('end_element', )
) == ValueError

# invalid attribute name
assert throws(perform_test,
    ('start_element', 'root'),
      ('write_attribute', '"foo"', 'bar'),
    ('end_element', )
) == ValueError

assert perform_test(
    ('start_element', 'root'),
      ('write_attribute', 'foo', '"bar"'),
    ('end_element', )
) == '''
<root foo="&quot;bar&quot;"/>
'''

# attribute after character data
assert throws(perform_test,
    ('start_element', 'root'),
      ('write_character_data', ''),
      ('write_attribute', 'foo', 'bar'),
    ('end_element', )
) == ValueError

assert perform_test(
    ('start_element', 'root'),
      ('write_attribute', 'foo', 'bar'),
      ('start_element', 'baz'),
      ('end_element', ),
    ('end_element', )
) == '''
<root foo="bar">
  <baz/>
</root>
'''

assert perform_test(
    ('start_element', 'root'),
      ('start_element', 'foo'),
        ('write_attribute', 'bar', 'baz'),
      ('end_element', ),
    ('end_element', )
) == '''
<root>
  <foo bar="baz"/>
</root>
'''

### writing character data ###

assert perform_test(
    ('start_element', 'root'),
      ('write_character_data', 'foo'),
      ('write_character_data', 'bar'),
      ('start_element', 'baz'),
      ('end_element', ),
    ('end_element', )
) == '''
<root>
  foobar
  <baz/>
</root>
'''

assert perform_test(
    ('start_element', 'root'),
      ('write_character_data', 'foo'),
      ('start_element', 'bar'),
        ('write_character_data', 'baz'),
      ('end_element', ),
    ('end_element', )
) == '''
<root>
  foo
  <bar>
    baz
  </bar>
</root>
'''

assert perform_test(
    ('start_element', 'root'),
      ('start_element', 'foo'),
        ('write_character_data', 'bar'),
        ('write_character_data', 'baz'),
      ('end_element', ),
    ('end_element', )
) == '''
<root>
  <foo>
    barbaz
  </foo>
</root>
'''

assert perform_test(
    ('start_element', 'root'),
      ('start_element', 'foo'),
        ('write_character_data', 'bar'),
      ('end_element', ),
      ('write_character_data', 'baz'),
    ('end_element', )
) == '''
<root>
  <foo>
    bar
  </foo>
  baz
</root>
'''

assert perform_test(
    ('start_element', 'root'),
      ('start_element', 'foo'),
      ('end_element', ),
      ('write_character_data', 'bar'),
      ('write_character_data', 'baz'),
    ('end_element', )
) == '''
<root>
  <foo/>
  barbaz
</root>
'''

assert perform_test(
    ('start_element', 'root'),
      ('write_character_data', 'foo'),
      ('start_element', 'bar'),
      ('end_element', ),
      ('write_character_data', 'baz'),
    ('end_element', )
) == '''
<root>
  foo
  <bar/>
  baz
</root>
'''

assert perform_test(
    ('start_element', 'root'),
      ('write_character_data', 'foo&bar'),
    ('end_element', )
) == '''
<root>
  foo&amp;bar
</root>
'''

assert perform_test(
    ('start_element', 'root'),
      ('write_character_data', 'foo'),
      ('start_element', 'element'),
        ('write_character_data', 'bar'),
      ('end_element', ),
      ('write_character_data', 'baz'),
    ('end_element', )
) == '''
<root>
  foo
  <element>
    bar
  </element>
  baz
</root>
'''

assert perform_test(
    ('start_element', 'root', True),
      ('write_character_data', 'foo'),
      ('start_element', 'element'),
        ('write_character_data', 'bar'),
      ('end_element', ),
      ('write_character_data', 'baz'),
    ('end_element', )
) == '''
<root>foo<element>bar</element>baz</root>
'''

assert perform_test(
    ('start_element', 'root'),
      ('write_character_data', 'foo'),
      ('start_element', 'element', True),
        ('write_character_data', 'bar'),
      ('end_element', ),
      ('write_character_data', 'baz'),
    ('end_element', )
) == '''
<root>
  foo
  <element>bar</element>
  baz
</root>
'''

# character data before root element
assert throws(perform_test,
    ('write_character_data', 'foo'),
    ('start_element', 'root'),
    ('end_element', )
) == ValueError

# character data after root element
f = StringIO.StringIO()
w = xorn.xml_writer.XMLWriter(f.write)
w.start_element('root')
w.end_element()
assert throws(w.write_character_data, 'foo') == ValueError
f.close()

### writing CDATA section ###

assert perform_test(
    ('start_element', 'root'),
      ('write_cdata_section', 'foo'),
    ('end_element', )
) == '''
<root>
  <![CDATA[foo]]>
</root>
'''

assert perform_test(
    ('start_element', 'root'),
      ('write_cdata_section', '<![CDATA[foo]]>'),
    ('end_element', )
) == '''
<root>
  <![CDATA[<![CDATA[foo]]>]]&gt;<![CDATA[]]>
</root>
'''

# CDATA section before root element
assert throws(perform_test,
    ('write_cdata_section', 'foo'),
    ('start_element', 'root'),
    ('end_element', )
) == ValueError

# CDATA section after root element
f = StringIO.StringIO()
w = xorn.xml_writer.XMLWriter(f.write)
w.start_element('root')
w.end_element()
assert throws(w.write_cdata_section, 'foo') == ValueError
f.close()
