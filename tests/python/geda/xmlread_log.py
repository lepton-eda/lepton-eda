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

import cStringIO
import xorn.geda.fileformat
import xorn.geda.read

class TestLog:
    def __init__(self, messages):
        self.messages = messages
        self.lineno = 0

    def error(self, message):
        message = '%d: error: %s' % (self.lineno + 1, message)
        print message
        assert self.messages[0] == message
        del self.messages[0]

    def warn(self, message):
        message = '%d: warning: %s' % (self.lineno + 1, message)
        print message
        assert self.messages[0] == message
        del self.messages[0]

def assert_read(data, messages):
    log = TestLog(messages)
    try:
        rev = xorn.geda.read.read_file(
            cStringIO.StringIO(data), '<test data>',
            xorn.geda.fileformat.FORMAT_SCH_XML, log)
    except xorn.geda.read.ParseError:
        pass
    assert not log.messages

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
  </content>
</schematic>
""", [
])

assert_read("""\
<?xml encoding="UTF-8"?>
""", [
    '1: error: XML declaration not well-formed: line 1, column 6'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic>
</schematic>
""", [
    '2: error: element name "schematic" without namespace'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/">
</schematic>
""", [
    '2: error: invalid namespace "https://hedmen.org/xorn/"'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<symbol xmlns="https://hedmen.org/xorn/schematic/">
</schematic>
""", [
    '3: error: mismatched tag: line 3, column 2'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<content xmlns="https://hedmen.org/xorn/schematic/">
</content>
""", [
    '2: error: invalid root element "content"'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
</schematic>
""", [
    '3: error: content missing'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
  </content>
  <content>
  </content>
</schematic>
""", [
    '5: error: duplicate content tag'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <box x="0" y="0" width="10" height="10"/>
</schematic>
""", [
    '3: error: unexpected element "box"',
    '4: error: content missing'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/" foo="x">
  <content bar="y">
    <box x="0" y="0" width="10" height="10" baz="z"/>
  </content>
</schematic>
""", [
    '2: error: unexpected attribute(s) "foo"',
    '3: error: unexpected attribute(s) "bar"',
    '4: error: unexpected attribute(s) "baz"'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  foo
  <content>
    bar
    <box x="0" y="0" width="10" height="10">
      baz
    </box>
  </content>
</schematic>
""", [
    '3: error: unexpected character data "foo"',
    '5: error: unexpected character data "bar"',
    '7: error: unexpected character data "baz"'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/"
           file-format-features="experimental hybridnum hybridnum foo">
  <content>
  </content>
</schematic>
""", [
    '2: error: duplicate file format feature',
    '2: error: unsupported file format feature "foo"'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <component x="0" y="0" symbol="foo"/>
    <picture x="0" y="0" width="10" height="10" pixmap="bar"/>
  </content>
</schematic>
""", [
    '4: error: undefined symbol "foo"',
    '5: error: undefined pixmap "bar"'
])

### line/fill attributes ###

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <path linewidth=".1" capstyle="square"/>
  </content>
</schematic>
""", [
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <path dashlength="1." dashspace="1."/>
  </content>
</schematic>
""", [
    '4: error: unexpected attribute(s) "dashlength", "dashspace"'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <path dashstyle="dotted" dashspace="1."/>
  </content>
</schematic>
""", [
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <path dashstyle="dotted" dashlength="1."/>
  </content>
</schematic>
""", [
    '4: error: dash space not specified',
    '4: error: unexpected attribute(s) "dashlength"'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <path dashstyle="dashed" dashlength="1." dashspace="1."/>
  </content>
</schematic>
""", [
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <path dashstyle="dashed"/>
  </content>
</schematic>
""", [
    '4: error: dash length not specified',
    '4: error: dash space not specified'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <path dashstyle="center" dashlength="1." dashspace="1."/>
  </content>
</schematic>
""", [
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <path dashstyle="center"/>
  </content>
</schematic>
""", [
    '4: error: dash length not specified',
    '4: error: dash space not specified'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <path dashstyle="phantom" dashlength="1." dashspace="1."/>
  </content>
</schematic>
""", [
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <path dashstyle="phantom"/>
  </content>
</schematic>
""", [
    '4: error: dash length not specified',
    '4: error: dash space not specified'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <path linewidth=".100" capstyle="0"
          dashstyle="dashed" dashlength="1.000" dashspace="1.000"/>
  </content>
</schematic>
""", [
    '4: error: invalid line width ".100"',
    '4: error: invalid cap style "0"',
    '4: error: invalid dash length "1.000"',
    '4: error: invalid dash space "1.000"'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <path filltype="fill"/>
  </content>
</schematic>
""", [
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <path filltype="fill" fillwidth=".1"
          angle0="20" pitch0="1." angle1="60" pitch1="1."/>
  </content>
</schematic>
""", [
    '4: error: unexpected attribute(s) "angle0", "angle1", "fillwidth", '
                                      '"pitch0", "pitch1"'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <path filltype="mesh" fillwidth=".1"
          angle0="20" pitch0="1." angle1="60" pitch1="1."/>
  </content>
</schematic>
""", [
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <path filltype="mesh"/>
  </content>
</schematic>
""", [
    '4: error: fill width not specified',
    '4: error: first fill angle not specified',
    '4: error: first fill pitch not specified',
    '4: error: second fill angle not specified',
    '4: error: second fill pitch not specified'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <path filltype="hatch" fillwidth=".1" angle0="20" pitch0="1."/>
  </content>
</schematic>
""", [
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <path filltype="hatch" angle1="60" pitch1="1."/>
  </content>
</schematic>
""", [
    '4: error: fill width not specified',
    '4: error: first fill angle not specified',
    '4: error: first fill pitch not specified',
    '4: error: unexpected attribute(s) "angle1", "pitch1"'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <path filltype="void"/>
  </content>
</schematic>
""", [
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <path filltype="void" fillwidth=".1"
          angle0="20" pitch0="1." angle1="60" pitch1="1."/>
  </content>
</schematic>
""", [
    '4: error: unexpected attribute(s) "angle0", "angle1", "fillwidth", '
                                      '"pitch0", "pitch1"'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <path filltype="mesh" fillwidth=".100"
          angle0="20." pitch0="1.000" angle1="60." pitch1="1.000"/>
  </content>
</schematic>
""", [
    '4: error: invalid fill width ".100"',
    '4: error: invalid first fill angle "20."',
    '4: error: invalid first fill pitch "1.000"',
    '4: error: invalid second fill angle "60."',
    '4: error: invalid second fill pitch "1.000"'
])

### objects ###

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <arc x="0." y="0." radius="10." startangle="0" sweepangle="120"
         linewidth=".1"/>
  </content>
</schematic>
""", [
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <arc/>
  </content>
</schematic>
""", [
    '4: error: X coordinate not specified',
    '4: error: Y coordinate not specified',
    '4: error: radius not specified',
    '4: error: start angle not specified',
    '4: error: sweep angle not specified'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <arc x="0.000" y="0.000" radius="10.000" startangle="0." sweepangle="120."
         color="freestyle5" filltype="fill">
      foo<br/>
    </arc>
  </content>
</schematic>
""", [
    '4: error: invalid X coordinate "0.000"',
    '4: error: invalid Y coordinate "0.000"',
    '4: error: invalid radius "10.000"',
    '4: error: invalid start angle "0."',
    '4: error: invalid sweep angle "120."',
    '4: error: invalid color "freestyle5"',
    '4: error: unexpected attribute(s) "filltype"',
    '6: error: unexpected character data "foo"',
    '6: error: unexpected element "br"'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <box x="0." y="0." width="10." height="10."
         linewidth=".1" filltype="fill"/>
  </content>
</schematic>
""", [
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <box/>
  </content>
</schematic>
""", [
    '4: error: X coordinate not specified',
    '4: error: Y coordinate not specified',
    '4: error: width not specified',
    '4: error: height not specified'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <box x="0.000" y="0.000" width="10.000" height="10.000"
         color="freestyle5">
      foo<br/>
    </box>
  </content>
</schematic>
""", [
    '4: error: invalid X coordinate "0.000"',
    '4: error: invalid Y coordinate "0.000"',
    '4: error: invalid width "10.000"',
    '4: error: invalid height "10.000"',
    '4: error: invalid color "freestyle5"',
    '6: error: unexpected character data "foo"',
    '6: error: unexpected element "br"'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <circle x="0." y="0." radius="10." linewidth=".1" filltype="fill"/>
  </content>
</schematic>
""", [
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <circle/>
  </content>
</schematic>
""", [
    '4: error: X coordinate not specified',
    '4: error: Y coordinate not specified',
    '4: error: radius not specified'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <circle x="0.000" y="0.000" radius="10.000" color="freestyle5">
      foo<br/>
    </circle>
  </content>
</schematic>
""", [
    '4: error: invalid X coordinate "0.000"',
    '4: error: invalid Y coordinate "0.000"',
    '4: error: invalid radius "10.000"',
    '4: error: invalid color "freestyle5"',
    '5: error: unexpected character data "foo"',
    '5: error: unexpected element "br"'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <component x="0." y="0." symbol="foo"/>
  </content>
  <symbol id="foo" name="bar.sym" mode="omitted"/>
</schematic>
""", [
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <component/>
  </content>
</schematic>
""", [
    '4: error: X coordinate not specified',
    '4: error: Y coordinate not specified',
    '4: error: symbol not specified'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <component x="0.000" y="0.000" selectable="0" angle="1" mirror="0"
               symbol="" linewidth=".1" filltype="fill">
      foo<br/>
    </component>
  </content>
  <symbol id="" name="bar.sym" mode="omitted"/>
</schematic>
""", [
    '4: error: invalid X coordinate "0.000"',
    '4: error: invalid Y coordinate "0.000"',
    '4: error: invalid selectability "0"',
    '4: error: invalid angle "1"',
    '4: error: invalid mirror flag "0"',
    '4: error: symbol id can\'t be empty',
    '4: error: unexpected attribute(s) "filltype", "linewidth"',
    '6: error: unexpected character data "foo"',
    '6: error: non-text element can\'t be attached',
    '9: error: symbol id can\'t be empty'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <component x="0" y="0" symbol="foo">
      <text x="0" y="0" size="10">foo</text>
      <attribute name="foo" x="0" y="0" size="10"
                 visible="yes" show="name-value">bar</attribute>
    </component>
  </content>
  <symbol id="foo" name="bar.sym" mode="omitted"/>
</schematic>
""", [
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <component x="0" y="0" symbol="foo">
      <arc x="0" y="0" radius="10" startangle="0" sweepangle="120"/>
      <box x="0" y="0" width="10" height="10"/>
      <circle x="0" y="0" radius="10"/>
      <component x="0" y="0" symbol="foo"/>
      <line x0="0" y0="0" x1="10" y1="10"/>
      <net x0="0" y0="0" x1="10" y1="10"/>
      <pin x0="0" y0="0" x1="10" y1="10"/>
      <path/>
      <picture x="0" y="0" width="10" height="10" pixmap="baz"/>
    </component>
  </content>
  <symbol id="foo" name="bar.sym" mode="omitted"/>
  <pixmap id="baz" name="baz.png" mode="omitted"/>
</schematic>
""", [
    '5: error: non-text element can\'t be attached',
    '6: error: non-text element can\'t be attached',
    '7: error: non-text element can\'t be attached',
    '8: error: non-text element can\'t be attached',
    '9: error: non-text element can\'t be attached',
    '10: error: non-text element can\'t be attached',
    '11: error: non-text element can\'t be attached',
    '12: error: non-text element can\'t be attached',
    '13: error: non-text element can\'t be attached'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <line x0="0." y0="0." x1="10." y1="10." linewidth=".1"/>
  </content>
</schematic>
""", [
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <line/>
  </content>
</schematic>
""", [
    '4: error: first X coordinate not specified',
    '4: error: first Y coordinate not specified',
    '4: error: second X coordinate not specified',
    '4: error: second Y coordinate not specified'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <line x0="0.000" y0="0.000" x1="10.000" y1="10.000" color="freestyle5"
          filltype="fill">
      foo<br/>
    </line>
  </content>
</schematic>
""", [
    '4: error: invalid first X coordinate "0.000"',
    '4: error: invalid first Y coordinate "0.000"',
    '4: error: invalid second X coordinate "10.000"',
    '4: error: invalid second Y coordinate "10.000"',
    '4: error: invalid color "freestyle5"',
    '4: error: unexpected attribute(s) "filltype"',
    '6: error: unexpected character data "foo"',
    '6: error: unexpected element "br"'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <net x0="0." y0="0." x1="10." y1="10."/>
    <pin x0="0." y0="0." x1="10." y1="10."/>
  </content>
</schematic>
""", [
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <net/>
    <pin/>
  </content>
</schematic>
""", [
    '4: error: first X coordinate not specified',
    '4: error: first Y coordinate not specified',
    '4: error: second X coordinate not specified',
    '4: error: second Y coordinate not specified',

    '5: error: first X coordinate not specified',
    '5: error: first Y coordinate not specified',
    '5: error: second X coordinate not specified',
    '5: error: second Y coordinate not specified'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <net x0="0.000" y0="0.000" x1="10.000" y1="10.000" color="freestyle5"
         type="0" inverted="0" linewidth=".1" filltype="fill">
      foo<br/>
    </net>
    <pin x0="0.000" y0="0.000" x1="10.000" y1="10.000" color="freestyle5"
         type="0" inverted="0" linewidth=".1" filltype="fill">
      foo<br/>
    </pin>
  </content>
</schematic>
""", [
    '4: error: invalid net/pin type "0"',
    '4: error: invalid first X coordinate "0.000"',
    '4: error: invalid first Y coordinate "0.000"',
    '4: error: invalid second X coordinate "10.000"',
    '4: error: invalid second Y coordinate "10.000"',
    '4: error: invalid color "freestyle5"',
    '4: error: unexpected attribute(s) "filltype", "inverted", "linewidth"',
    '6: error: unexpected character data "foo"',
    '6: error: non-text element can\'t be attached',

    '8: error: invalid net/pin type "0"',
    '8: error: invalid invertedness "0"',
    '8: error: invalid first X coordinate "0.000"',
    '8: error: invalid first Y coordinate "0.000"',
    '8: error: invalid second X coordinate "10.000"',
    '8: error: invalid second Y coordinate "10.000"',
    '8: error: invalid color "freestyle5"',
    '8: error: unexpected attribute(s) "filltype", "linewidth"',
    '10: error: unexpected character data "foo"',
    '10: error: non-text element can\'t be attached'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <net x0="0" y0="0" x1="10" y1="10">
      <text x="0" y="0" size="10">foo</text>
      <attribute name="foo" x="0" y="0" size="10"
                 visible="yes" show="name-value">bar</attribute>
    </net>
    <pin x0="0" y0="0" x1="10" y1="10">
      <text x="0" y="0" size="10">foo</text>
      <attribute name="foo" x="0" y="0" size="10"
                 visible="yes" show="name-value">bar</attribute>
    </pin>
  </content>
</schematic>
""", [
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <net x0="0" y0="0" x1="10" y1="10">
      <arc x="0" y="0" radius="10" startangle="0" sweepangle="120"/>
      <box x="0" y="0" width="10" height="10"/>
      <circle x="0" y="0" radius="10"/>
      <component x="0" y="0" symbol="foo"/>
      <line x0="0" y0="0" x1="10" y1="10"/>
      <net x0="0" y0="0" x1="10" y1="10"/>
      <pin x0="0" y0="0" x1="10" y1="10"/>
      <path/>
      <picture x="0" y="0" width="10" height="10" pixmap="baz"/>
    </net>
    <pin x0="0" y0="0" x1="10" y1="10">
      <arc x="0" y="0" radius="10" startangle="0" sweepangle="120"/>
      <box x="0" y="0" width="10" height="10"/>
      <circle x="0" y="0" radius="10"/>
      <component x="0" y="0" symbol="foo"/>
      <line x0="0" y0="0" x1="10" y1="10"/>
      <net x0="0" y0="0" x1="10" y1="10"/>
      <pin x0="0" y0="0" x1="10" y1="10"/>
      <path/>
      <picture x="0" y="0" width="10" height="10" pixmap="baz"/>
    </pin>
  </content>
</schematic>
""", [
    '5: error: non-text element can\'t be attached',
    '6: error: non-text element can\'t be attached',
    '7: error: non-text element can\'t be attached',
    '8: error: non-text element can\'t be attached',
    '9: error: non-text element can\'t be attached',
    '10: error: non-text element can\'t be attached',
    '11: error: non-text element can\'t be attached',
    '12: error: non-text element can\'t be attached',
    '13: error: non-text element can\'t be attached',

    '16: error: non-text element can\'t be attached',
    '17: error: non-text element can\'t be attached',
    '18: error: non-text element can\'t be attached',
    '19: error: non-text element can\'t be attached',
    '20: error: non-text element can\'t be attached',
    '21: error: non-text element can\'t be attached',
    '22: error: non-text element can\'t be attached',
    '23: error: non-text element can\'t be attached',
    '24: error: non-text element can\'t be attached'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <path linewidth=".1" filltype="fill">A<br/>B</path>
  </content>
</schematic>
""", [
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <path>&#xff;</path>
  </content>
</schematic>
""", [
    '4: error: non-ASCII character in path data'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <path color="freestyle5">A<br>B</br>C<overbar>D</overbar>E</path>
  </content>
</schematic>
""", [
    '4: error: invalid color "freestyle5"',
    '4: error: unexpected character data "B"',
    '4: error: unexpected element "overbar"'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <picture x="0." y="0." width="10." height="10." pixmap="foo"/>
  </content>
  <pixmap id="foo" name="bar.png" mode="omitted"/>
</schematic>
""", [
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <picture/>
  </content>
</schematic>
""", [
    '4: error: X coordinate not specified',
    '4: error: Y coordinate not specified',
    '4: error: width not specified',
    '4: error: height not specified',
    '4: error: pixmap not specified'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <picture x="0.000" y="0.000" width="10.000" height="10.000"
             angle="1" mirrored="0" pixmap="" linewidth=".1" filltype="fill">
      foo<br/>
    </picture>
  </content>
  <pixmap id="" name="bar.png" mode="omitted"/>
</schematic>
""", [
    '4: error: invalid X coordinate "0.000"',
    '4: error: invalid Y coordinate "0.000"',
    '4: error: invalid width "10.000"',
    '4: error: invalid height "10.000"',
    '4: error: invalid angle "1"',
    '4: error: invalid mirror flag "0"',
    '4: error: pixmap id can\'t be empty',
    '4: error: unexpected attribute(s) "filltype", "linewidth"',
    '6: error: unexpected character data "foo"',
    '6: error: unexpected element "br"',
    '9: error: pixmap id can\'t be empty'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <text x="0." y="0." size="10">foo</text>
    <attribute name="foo" x="0." y="0." size="10"
               visible="yes" show="name-value">bar</attribute>
  </content>
</schematic>
""", [
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <text/>
    <attribute/>
  </content>
</schematic>
""", [
    '4: error: X coordinate not specified',
    '4: error: Y coordinate not specified',
    '4: error: text size not specified',

    '5: error: X coordinate not specified',
    '5: error: Y coordinate not specified',
    '5: error: text size not specified',
    '5: error: text visibility not specified',
    '5: error: show name/value value not specified',
    '5: error: attribute name not specified'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <text x="0.000" y="0.000" color="freestyle5" size="10."
          visible="0" show="0" angle="1" alignment="0"
          linewidth=".1" filltype="fill"/>
    <attribute name="foo" x="0.000" y="0.000" color="freestyle5" size="10."
               visible="0" show="0" angle="1" alignment="0"
               linewidth=".1" filltype="fill"/>
  </content>
</schematic>
""", [
    '4: error: invalid X coordinate "0.000"',
    '4: error: invalid Y coordinate "0.000"',
    '4: error: invalid color "freestyle5"',
    '4: error: invalid text size "10."',
    '4: error: invalid text visibility "0"',
    '4: error: invalid show name/value value "0"',
    '4: error: invalid angle "1"',
    '4: error: invalid alignment "0"',
    '4: error: unexpected attribute(s) "filltype", "linewidth"',

    '7: error: invalid X coordinate "0.000"',
    '7: error: invalid Y coordinate "0.000"',
    '7: error: invalid color "freestyle5"',
    '7: error: invalid text size "10."',
    '7: error: invalid text visibility "0"',
    '7: error: invalid show name/value value "0"',
    '7: error: invalid angle "1"',
    '7: error: invalid alignment "0"',
    '7: error: unexpected attribute(s) "filltype", "linewidth"'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <text x="0" y="0" size="10">
      A\\B<br/>C\\D<overbar>E\\F<br/>G\\H</overbar>I\\J<br/>K\\L
    </text>
  </content>
</schematic>
""", [
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <text x="0" y="0" size="10">
      A<br>B</br>C
    </text>
  </content>
</schematic>
""", [
    '5: error: unexpected character data "B"'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <text x="0" y="0" size="10">
      <text x="0" y="0" size="10"/>
    </text>
  </content>
</schematic>
""", [
    '5: error: unexpected element "text"'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
    <text x="0" y="0" size="10">
      A<overbar>B<overbar>C</overbar>D</overbar>E
    </text>
  </content>
</schematic>
""", [
    '5: error: unexpected element "overbar"'
])

### symbol and pixmap references ###

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
  </content>
  <symbol id="foo" name="foo.sym" mode="referenced">
    <content>
    </content>
  </symbol>
  <pixmap id="bar" name="bar.png" mode="referenced"/>
</schematic>
""", [
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
  </content>
  <symbol id="foo" name="foo.sym" mode="omitted"/>
  <pixmap id="bar" name="bar.png" mode="omitted"/>
</schematic>
""", [
])
assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
  </content>
  <symbol id="foo" name="foo.sym"/>
  <pixmap id="bar" name="bar.png"/>
</schematic>
""", [
    '5: error: symbol mode not specified',
    '6: error: pixmap mode not specified'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
  </content>
  <symbol id="foo" name="foo.sym" mode="foobar"/>
  <pixmap id="bar" name="bar.png" mode="foobar"/>
</schematic>
""", [
    '5: error: invalid symbol mode "foobar"',
    '6: error: invalid pixmap mode "foobar"'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
  </content>
  <symbol id="foo" mode="omitted"/>
  <pixmap id="bar" mode="omitted"/>
</schematic>
""", [
    '5: error: symbol name not specified',
    '6: error: pixmap name not specified'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
  </content>
  <symbol id="foo" mode="embedded">
    <content>
    </content>
  </symbol>
  <pixmap id="bar" mode="embedded"/>
</schematic>
""", [
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
  </content>
  <symbol name="foo.sym" mode="omitted"/>
  <pixmap name="bar.png" mode="omitted"/>
</schematic>
""", [
    '5: error: symbol id not specified',
    '6: error: pixmap id not specified'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
  </content>
  <symbol id="foobar" name="foo.sym" mode="omitted"/>
  <pixmap id="foobar" name="bar.png" mode="omitted"/>
</schematic>
""", [
    '6: error: duplicate id "foobar"'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
  </content>
  <symbol id="foo" name="foo.sym" mode="omitted"/>
  <symbol id="foo" name="foo.sym" mode="omitted"/>
  <pixmap id="bar" name="bar.png" mode="omitted"/>
  <pixmap id="bar" name="bar.png" mode="omitted"/>
</schematic>
""", [
    '6: error: duplicate id "foo"',
    '8: error: duplicate id "bar"'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
  </content>
  <symbol id="foo" name="foo.sym" mode="omitted">
    <content>
    </content>
  </symbol>
  <pixmap id="bar" name="bar.png" mode="omitted">
Zm9v
  </pixmap>
</schematic>
""", [
    '6: error: unexpected element "content"',
    '10: error: unexpected character data "Zm9v"'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
  </content>
  <symbol id="foo" name="foo.sym" mode="referenced">
    <content>
    </content>
  </symbol>
  <pixmap id="bar" name="bar.png" mode="referenced">
    <content>
    </content>
  </pixmap>
</schematic>
""", [
    '10: error: unexpected element "content"'
])

assert_read("""\
<?xml version="1.0" encoding="UTF-8"?>
<schematic xmlns="https://hedmen.org/xorn/schematic/">
  <content>
  </content>
  <symbol id="foo" name="foo.sym" mode="referenced">
    <content>
    </content>
  </symbol>
  <pixmap id="bar" name="bar.png" mode="referenced">
AAA
  </pixmap>
</schematic>
""", [
    '11: error: base64 decoding error'
])
