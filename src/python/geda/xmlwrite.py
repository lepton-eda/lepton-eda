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

## \namespace xorn.geda.xmlwrite
## Writing gEDA schematic/symbol files in XML format.

import xorn.fixednum
import xorn.hybridnum
import xorn.storage
import xorn.xml_writer
import xorn.geda.attrib
from xorn.geda.xmlformat import *

class Writer:
    def __init__(self, raw_file, w, use_hybridnum, omit_symbols, omit_pixmaps):
        self.raw_file = raw_file
        self.w = w
        self.use_hybridnum = use_hybridnum
        self.omit_symbols = omit_symbols
        self.omit_pixmaps = omit_pixmaps

        self.symbol_ids = {}
        self.pixmap_ids = {}

    def fmt(self, x):
        if self.use_hybridnum:
            return xorn.hybridnum.format(x, 2)
        else:
            return xorn.fixednum.format(int(round(x)), 2)

    def write_line(self, line):
        if line.width:
            self.w.write_attribute('linewidth', self.fmt(line.width))
        if line.cap_style:
            self.w.write_attribute('capstyle', ENUM_CAPSTYLE[line.cap_style])
        if line.dash_style:
            self.w.write_attribute('dashstyle', ENUM_DASHSTYLE[line.dash_style])
        if line.dash_style != 0 and line.dash_style != 1:
            self.w.write_attribute('dashlength', self.fmt(line.dash_length))
        if line.dash_style != 0:
            self.w.write_attribute('dashspace', self.fmt(line.dash_space))

    def write_fill(self, fill):
        if fill.type:
            self.w.write_attribute('filltype', ENUM_FILLTYPE[fill.type])
        if fill.type == 2 or fill.type == 3:
            self.w.write_attribute('fillwidth', self.fmt(fill.width))
            self.w.write_attribute('angle0', str(fill.angle0))
            self.w.write_attribute('pitch0', self.fmt(fill.pitch0))
        if fill.type == 2:
            self.w.write_attribute('angle1', str(fill.angle1))
            self.w.write_attribute('pitch1', self.fmt(fill.pitch1))

    def write_object(self, ob):
        data = ob.data()
        if isinstance(data, xorn.storage.Arc):
            self.w.start_element('arc')
            self.w.write_attribute('x', self.fmt(data.x))
            self.w.write_attribute('y', self.fmt(data.y))
            self.w.write_attribute('radius', self.fmt(data.radius))
            self.w.write_attribute('startangle', str(data.startangle))
            self.w.write_attribute('sweepangle', str(data.sweepangle))
            if data.color != 3:
                self.w.write_attribute('color', ENUM_COLOR[data.color])
            self.write_line(data.line)
            self.w.end_element()

        elif isinstance(data, xorn.storage.Box):
            self.w.start_element('box')
            self.w.write_attribute('x', self.fmt(data.x))
            self.w.write_attribute('y', self.fmt(data.y))
            self.w.write_attribute('width', self.fmt(data.width))
            self.w.write_attribute('height', self.fmt(data.height))
            if data.color != 3:
                self.w.write_attribute('color', ENUM_COLOR[data.color])
            self.write_line(data.line)
            self.write_fill(data.fill)
            self.w.end_element()

        elif isinstance(data, xorn.storage.Circle):
            self.w.start_element('circle')
            self.w.write_attribute('x', self.fmt(data.x))
            self.w.write_attribute('y', self.fmt(data.y))
            self.w.write_attribute('radius', self.fmt(data.radius))
            if data.color != 3:
                self.w.write_attribute('color', ENUM_COLOR[data.color])
            self.write_line(data.line)
            self.write_fill(data.fill)
            self.w.end_element()

        elif isinstance(data, xorn.storage.Component):
            self.w.start_element('component')
            self.w.write_attribute('x', self.fmt(data.x))
            self.w.write_attribute('y', self.fmt(data.y))
            if not data.selectable:
                self.w.write_attribute('selectable',
                                       ENUM_BOOLEAN[data.selectable])
            if data.angle:
                self.w.write_attribute('angle', str(data.angle))
            if data.mirror:
                self.w.write_attribute('mirror', ENUM_BOOLEAN[data.mirror])
            self.w.write_attribute('symbol', self.symbol_ids[data.symbol])
            for ob in ob.attached_objects():
                self.write_object(ob)
            self.w.end_element()

        elif isinstance(data, xorn.storage.Line):
            self.w.start_element('line')
            self.w.write_attribute('x0', self.fmt(data.x))
            self.w.write_attribute('y0', self.fmt(data.y))
            self.w.write_attribute('x1', self.fmt(data.x + data.width))
            self.w.write_attribute('y1', self.fmt(data.y + data.height))
            if data.color != 3:
                self.w.write_attribute('color', ENUM_COLOR[data.color])
            self.write_line(data.line)
            self.w.end_element()

        elif isinstance(data, xorn.storage.Net):
            if data.is_pin:
                self.w.start_element('pin')
                default_color = 1
            else:
                self.w.start_element('net')
                if data.is_bus:
                    default_color = 10
                else:
                    default_color = 4
            self.w.write_attribute('x0', self.fmt(data.x))
            self.w.write_attribute('y0', self.fmt(data.y))
            self.w.write_attribute('x1', self.fmt(data.x + data.width))
            self.w.write_attribute('y1', self.fmt(data.y + data.height))
            if data.color != default_color:
                self.w.write_attribute('color', ENUM_COLOR[data.color])
            if data.is_bus:
                self.w.write_attribute('type', ENUM_NETTYPE[data.is_bus])
            if data.is_pin and data.is_inverted:
                self.w.write_attribute('inverted',
                                       ENUM_BOOLEAN[data.is_inverted])
            for ob in ob.attached_objects():
                self.write_object(ob)
            self.w.end_element()

        elif isinstance(data, xorn.storage.Path):
            self.w.start_element('path', preserve_whitespace = True)
            if data.color != 3:
                self.w.write_attribute('color', ENUM_COLOR[data.color])
            self.write_line(data.line)
            self.write_fill(data.fill)
            for i, line in enumerate(data.pathdata.split('\n')):
                if i:
                    self.w.start_element('br')
                    self.w.end_element()
                self.w.write_character_data(line)
            self.w.end_element()

        elif isinstance(data, xorn.storage.Picture):
            self.w.start_element('picture')
            self.w.write_attribute('x', self.fmt(data.x))
            self.w.write_attribute('y', self.fmt(data.y))
            self.w.write_attribute('width', self.fmt(data.width))
            self.w.write_attribute('height', self.fmt(data.height))
            if data.angle:
                self.w.write_attribute('angle', str(data.angle))
            if data.mirror:
                self.w.write_attribute('mirrored', ENUM_BOOLEAN[data.mirror])
            self.w.write_attribute('pixmap', self.pixmap_ids[data.pixmap])
            self.w.end_element()

        elif isinstance(data, xorn.storage.Text):
            try:
                name, value = xorn.geda.attrib.parse_string(data.text)
                if '\n' in name:
                    raise xorn.geda.attrib.MalformedAttributeError
            except xorn.geda.attrib.MalformedAttributeError:
                self.w.start_element('text', True)
                is_attribute = False
                text = data.text
            else:
                self.w.start_element('attribute', True)
                self.w.write_attribute('name', name)
                is_attribute = True
                text = value

            self.w.write_attribute('x', self.fmt(data.x))
            self.w.write_attribute('y', self.fmt(data.y))
            if data.color != (5 if is_attribute else 9):
                self.w.write_attribute('color', ENUM_COLOR[data.color])
            self.w.write_attribute('size', str(data.text_size))
            if is_attribute or not data.visibility:
                self.w.write_attribute(
                    'visible', ENUM_BOOLEAN[data.visibility])
            if is_attribute or data.show_name_value:
                self.w.write_attribute(
                    'show', ENUM_SHOW_NAME_VALUE[data.show_name_value])
            if data.angle:
                self.w.write_attribute('angle', str(data.angle))
            if data.alignment:
                self.w.write_attribute('alignment',
                                       ENUM_ALIGNMENT[data.alignment])

            # convert \_...\_ into <overbar>...</overbar>
            for i, part in enumerate(text.split('\\_')):
                if i % 2:
                    self.w.start_element('overbar')
                # replace \\ with \ and remove single \
                part = '\\'.join(s.replace('\\', '')
                                 for s in part.split('\\\\'))
                # replace newline characters with <br/>
                for j, line in enumerate(part.split('\n')):
                    if j:
                        self.w.start_element('br')
                        self.w.end_element()
                    self.w.write_character_data(line.decode('utf-8'))
                if i % 2:
                    self.w.end_element()

            self.w.end_element()
        else:
            raise AssertionError

    def write_symbol(self, symbol):
        if symbol.prim_objs is None and not self.omit_symbols:
            raise ValueError, 'symbol contents missing'
        self.w.start_element('symbol')
        self.w.write_attribute('id', self.symbol_ids[symbol])
        self.w.write_attribute('name', symbol.basename)
        if symbol.embedded:
            self.w.write_attribute('mode', 'embedded')
            self.write_rev(xorn.proxy.RevisionProxy(symbol.prim_objs))
        elif not self.omit_symbols:
            self.w.write_attribute('mode', 'referenced')
            self.write_rev(xorn.proxy.RevisionProxy(symbol.prim_objs))
        else:
            self.w.write_attribute('mode', 'omitted')
        self.w.end_element()

    def write_pixmap(self, pixmap):
        if pixmap.data is None and not self.omit_pixmaps:
            raise ValueError, 'pixmap contents missing'
        self.w.start_element('pixmap', preserve_whitespace = True)
        self.w.write_attribute('id', self.pixmap_ids[pixmap])
        self.w.write_attribute('name', pixmap.filename)
        if not self.omit_pixmaps:
            if pixmap.embedded:
                self.w.write_attribute('mode', 'embedded')
            else:
                self.w.write_attribute('mode', 'referenced')
            self.w.write_character_data('')  # close starting tag
            self.raw_file.write('\n')
            xorn.base64.encode(self.raw_file, pixmap.data)
            self.raw_file.write('  ' * (len(self.w.stack) - 1))
        else:
            self.w.write_attribute('mode', 'omitted')
        self.w.end_element()

    def write_rev(self, rev):
        ids = set()
        def get_unique_id(name):
            i, id = 0, name
            while id in ids:
                i += 1
                id = "%s.%d" % (name, i)
            ids.add(id)
            return id

        for ob in rev.all_objects():
            data = ob.data()
            if isinstance(data, xorn.storage.Component):
                if data.symbol not in self.symbol_ids:
                    tmp = data.symbol.basename
                    if tmp.lower().endswith('.sym'):
                        tmp = tmp[:-4]
                    if tmp.lower().endswith('.sym.xml'):
                        tmp = tmp[:-8]
                    self.symbol_ids[data.symbol] = get_unique_id(tmp)
            elif isinstance(data, xorn.storage.Picture):
                if data.pixmap not in self.pixmap_ids:
                    tmp = data.pixmap.filename.split('/')[-1]
                    # TODO: need to use the actual list of pixmap extensions
                    #       supported by gEDA/gaf here
                    if tmp.lower().endswith('.jpg') or \
                       tmp.lower().endswith('.png') or \
                       tmp.lower().endswith('.gif') or \
                       tmp.lower().endswith('.svg'):
                        tmp = tmp[:-4]
                    self.pixmap_ids[data.pixmap] = get_unique_id(tmp)

        self.w.start_element('content')
        for ob in rev.toplevel_objects():
            self.write_object(ob)
        self.w.end_element()

        written_symbols = set()
        written_pixmaps = set()
        for ob in rev.all_objects():
            data = ob.data()
            if isinstance(data, xorn.storage.Component):
                if data.symbol not in written_symbols:
                    self.write_symbol(data.symbol)
                    written_symbols.add(data.symbol)
            elif isinstance(data, xorn.storage.Picture):
                if data.pixmap not in written_pixmaps:
                    self.write_pixmap(data.pixmap)
                    written_pixmaps.add(data.pixmap)

def write_file(f, rev, is_symbol, use_hybridnum = False,
                                  omit_symbols = False,
                                  omit_pixmaps = False):
    w = xorn.xml_writer.XMLWriter(lambda s: f.write(s.encode('utf-8')))
    if is_symbol:
        w.start_element('symbol')
    else:
        w.start_element('schematic')
    w.write_attribute('xmlns', NAMESPACE)
    fff = ['experimental']
    if use_hybridnum:
        fff.append('hybridnum')
    if fff:
        w.write_attribute('file-format-features', ' '.join(fff))
    Writer(f, w, use_hybridnum, omit_symbols, omit_pixmaps).write_rev(rev)
    w.end_element()
    assert w.is_done()
