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

## \namespace xorn.geda.xmlread
## Reading gEDA schematic/symbol files in XML format.

import cStringIO, xml.parsers.expat
from gettext import gettext as _
import xorn.base64
import xorn.fixednum
import xorn.proxy
import xorn.storage
import xorn.geda.ref
from xorn.geda.xmlformat import *

NSSEP = '!'

class VoidHandler:
    def start_element(self, name, attributes):
        return self

    def end_element(self, name):
        pass

    def character_data(self, data):
        pass

class NullHandler:
    def __init__(self, log):
        self.log = log

    def start_element(self, name, attributes):
        self.log.error(_("unexpected element \"%s\"") % name)
        return VoidHandler()

    def end_element(self, name):
        pass

    def character_data(self, data):
        s = data.strip()
        if s:
            self.log.error(_("unexpected character data \"%s\"") % s)

class OverbarHandler(NullHandler):
    def __init__(self, log, text):
        self.log = log
        self.text = text

    def start_element(self, name, attributes):
        if name != 'br':
            return NullHandler.start_element(self, name, attributes)

        self.text.append('\n')
        return NullHandler(self.log)

    def character_data(self, data):
        self.text.append(data.replace('\\', '\\\\'))

    def end_element(self, name):
        self.text.append('\\_')

class TextHandler(NullHandler):
    def __init__(self, log, rev, attached_to, data, attribute_name):
        self.log = log
        self.rev = rev
        self.attached_to = attached_to
        self.data = data
        self.text = []
        if attribute_name is not None:
            self.text.append('%s=' % attribute_name)

    def start_element(self, name, attributes):
        if name == 'br':
            self.text.append('\n')
            return NullHandler(self.log)

        if name == 'overbar':
            self.text.append('\\_')
            return OverbarHandler(self.log, self.text)

        return NullHandler.start_element(self, name, attributes)

    def character_data(self, data):
        self.text.append(data.replace('\\', '\\\\'))

    def end_element(self, name):
        self.data.text = ''.join(self.text).encode('utf-8')
        ob = self.rev.add_object(self.data)
        if self.attached_to is not None:
            self.rev.relocate_object(ob, self.attached_to, None)

class PathHandler(NullHandler):
    def __init__(self, log, rev, data):
        self.log = log
        self.rev = rev
        self.data = data
        self.fragments = []

    def start_element(self, name, attributes):
        if name != 'br':
            return NullHandler.start_element(self, name, attributes)

        self.fragments.append('\n')
        return NullHandler(self.log)

    def character_data(self, data):
        try:
            self.fragments.append(data.encode())
        except UnicodeEncodeError:
            self.log.error(_("non-ASCII character in path data"))

    def end_element(self, name):
        self.data.pathdata = ''.join(self.fragments)
        self.rev.add_object(self.data)

def parse_angle(x):
    angle = int(x)
    if angle != 0 and angle != 90 and angle != 180 and angle != 270:
        raise ValueError
    return angle

class ContentHandler(NullHandler):
    def __init__(self, c, rev, attached_to):
        self.log = c.log
        self.c = c
        self.rev = rev
        self.attached_to = attached_to

    def start_element(self, name, attributes):
        if name == 'text' or name == 'attribute':
            is_attribute = name == 'attribute'
            data = xorn.storage.Text(
                x = self.c.parse_attribute(
                    attributes, 'x', None,
                    self.c.parse, 'X coordinate'),
                y = self.c.parse_attribute(
                    attributes, 'y', None,
                    self.c.parse, 'Y coordinate'),
                color = self.c.parse_attribute(
                    attributes, 'color',
                    5 if is_attribute else 9,
                    ENUM_COLOR.index, 'color'),
                text_size = self.c.parse_attribute(
                    attributes, 'size', None,
                    int, 'text size'),
                visibility = self.c.parse_attribute(
                    attributes, 'visible', None if is_attribute else 1,
                    ENUM_BOOLEAN.index, 'text visibility'),
                show_name_value = self.c.parse_attribute(
                    attributes, 'show', None if is_attribute else 0,
                    ENUM_SHOW_NAME_VALUE.index, 'show name/value value'),
                angle = self.c.parse_attribute(
                    attributes, 'angle', 0,
                    parse_angle, 'angle'),
                alignment = self.c.parse_attribute(
                    attributes, 'alignment', 0,
                    ENUM_ALIGNMENT.index, 'alignment'))
            if is_attribute:
                try:
                    name = attributes.pop('name')
                except KeyError:
                    self.c.log.error(_("attribute name not specified"))
                    name = None
            else:
                name = None
            return TextHandler(
                self.c.log, self.rev, self.attached_to, data, name)

        if self.attached_to:
            self.c.log.error(_("non-text element can't be attached"))
            return VoidHandler()

        if name == 'arc':
            self.rev.add_object(
                xorn.storage.Arc(
                    x = self.c.parse_attribute(
                        attributes, 'x', None,
                        self.c.parse, 'X coordinate'),
                    y = self.c.parse_attribute(
                        attributes, 'y', None,
                        self.c.parse, 'Y coordinate'),
                    radius = self.c.parse_attribute(
                        attributes, 'radius', None,
                        self.c.parse, 'radius'),
                    startangle = self.c.parse_attribute(
                        attributes, 'startangle', None,
                        int, 'start angle'),
                    sweepangle = self.c.parse_attribute(
                        attributes, 'sweepangle', None,
                        int, 'sweep angle'),
                    color = self.c.parse_attribute(
                        attributes, 'color', 3,
                        ENUM_COLOR.index, 'color'),
                    line = self.c.parse_line(attributes)))
            return NullHandler(self.c.log)

        if name == 'box':
            self.rev.add_object(
                xorn.storage.Box(
                    x = self.c.parse_attribute(
                        attributes, 'x', None,
                        self.c.parse, 'X coordinate'),
                    y = self.c.parse_attribute(
                        attributes, 'y', None,
                        self.c.parse, 'Y coordinate'),
                    width = self.c.parse_attribute(
                        attributes, 'width', None,
                        self.c.parse, 'width'),
                    height = self.c.parse_attribute(
                        attributes, 'height', None,
                        self.c.parse, 'height'),
                    color = self.c.parse_attribute(
                        attributes, 'color', 3,
                        ENUM_COLOR.index, 'color'),
                    line = self.c.parse_line(attributes),
                    fill = self.c.parse_fill(attributes)))
            return NullHandler(self.c.log)

        if name == 'circle':
            self.rev.add_object(
                xorn.storage.Circle(
                    x = self.c.parse_attribute(
                        attributes, 'x', None,
                        self.c.parse, 'X coordinate'),
                    y = self.c.parse_attribute(
                        attributes, 'y', None,
                        self.c.parse, 'Y coordinate'),
                    radius = self.c.parse_attribute(
                        attributes, 'radius', None,
                        self.c.parse, 'radius'),
                    color = self.c.parse_attribute(
                        attributes, 'color', 3,
                        ENUM_COLOR.index, 'color'),
                    line = self.c.parse_line(attributes),
                    fill = self.c.parse_fill(attributes)))
            return NullHandler(self.c.log)

        if name == 'component':
            ob = self.rev.add_object(
                xorn.storage.Component(
                    x = self.c.parse_attribute(
                        attributes, 'x', None,
                        self.c.parse, 'X coordinate'),
                    y = self.c.parse_attribute(
                        attributes, 'y', None,
                        self.c.parse, 'Y coordinate'),
                    selectable = self.c.parse_attribute(
                        attributes, 'selectable', True,
                        ENUM_BOOLEAN.index, 'selectability'),
                    angle = self.c.parse_attribute(
                        attributes, 'angle', 0,
                        parse_angle, 'angle'),
                    mirror = self.c.parse_attribute(
                        attributes, 'mirror', False,
                        ENUM_BOOLEAN.index, 'mirror flag')))
            try:
                symbol_id = attributes.pop('symbol')
            except KeyError:
                self.c.log.error(_("symbol not specified"))
            else:
                if not symbol_id:
                    self.c.log.error(_("symbol id can't be empty"))
                else:
                    self.c.symbol_refs.append(
                        (self.rev, ob, symbol_id, self.c.log.lineno))

            return ContentHandler(self.c, self.rev, ob)

        if name == 'line':
            x0 = self.c.parse_attribute(attributes, 'x0', None,
                                        self.c.parse, 'first X coordinate')
            y0 = self.c.parse_attribute(attributes, 'y0', None,
                                        self.c.parse, 'first Y coordinate')
            x1 = self.c.parse_attribute(attributes, 'x1', None,
                                        self.c.parse, 'second X coordinate')
            y1 = self.c.parse_attribute(attributes, 'y1', None,
                                        self.c.parse, 'second Y coordinate')
            self.rev.add_object(
                xorn.storage.Line(
                    x = x0, y = y0, width = x1 - x0, height = y1 - y0,
                    color = self.c.parse_attribute(
                        attributes, 'color', 3,
                        ENUM_COLOR.index, 'color'),
                    line = self.c.parse_line(attributes)))
            return NullHandler(self.c.log)

        if name == 'net' or name == 'pin':
            is_pin = name == 'pin'
            is_bus = self.c.parse_attribute(attributes, 'type', False,
                                            ENUM_NETTYPE.index, 'net/pin type')
            if is_pin:
                default_color = 1
                is_inverted = self.c.parse_attribute(
                    attributes, 'inverted', False,
                    ENUM_BOOLEAN.index, 'invertedness')
            else:
                if is_bus:
                    default_color = 10
                else:
                    default_color = 4
                is_inverted = False

            x0 = self.c.parse_attribute(attributes, 'x0', None,
                                        self.c.parse, 'first X coordinate')
            y0 = self.c.parse_attribute(attributes, 'y0', None,
                                        self.c.parse, 'first Y coordinate')
            x1 = self.c.parse_attribute(attributes, 'x1', None,
                                        self.c.parse, 'second X coordinate')
            y1 = self.c.parse_attribute(attributes, 'y1', None,
                                        self.c.parse, 'second Y coordinate')
            ob = self.rev.add_object(
                xorn.storage.Net(
                    x = x0, y = y0, width = x1 - x0, height = y1 - y0,
                    color = self.c.parse_attribute(
                        attributes, 'color', default_color,
                        ENUM_COLOR.index, 'color'),
                    is_bus = is_bus,
                    is_pin = is_pin,
                    is_inverted = is_inverted))
            return ContentHandler(self.c, self.rev, ob)

        if name == 'path':
            return PathHandler(self.c.log, self.rev, xorn.storage.Path(
                color = self.c.parse_attribute(attributes, 'color', 3,
                                               ENUM_COLOR.index, 'color'),
                line = self.c.parse_line(attributes),
                fill = self.c.parse_fill(attributes)))

        if name == 'picture':
            ob = self.rev.add_object(
                xorn.storage.Picture(
                    x = self.c.parse_attribute(
                        attributes, 'x', None,
                        self.c.parse, 'X coordinate'),
                    y = self.c.parse_attribute(
                        attributes, 'y', None,
                        self.c.parse, 'Y coordinate'),
                    width = self.c.parse_attribute(
                        attributes, 'width', None,
                        self.c.parse, 'width'),
                    height = self.c.parse_attribute(
                        attributes, 'height', None,
                        self.c.parse, 'height'),
                    angle = self.c.parse_attribute(
                        attributes, 'angle', 0,
                        parse_angle, 'angle'),
                    mirror = self.c.parse_attribute(
                        attributes, 'mirrored', False,
                        ENUM_BOOLEAN.index, 'mirror flag'),
                    pixmap = None))
            try:
                pixmap_id = attributes.pop('pixmap')
            except KeyError:
                self.c.log.error(_("pixmap not specified"))
            else:
                if not pixmap_id:
                    self.c.log.error(_("pixmap id can't be empty"))
                else:
                    self.c.pixmap_refs.append(
                        (self.rev, ob, pixmap_id, self.c.log.lineno))

            return NullHandler(self.c.log)

        self.c.log.error(_("unexpected element \"%s\"") % name)
        return VoidHandler()

class PixmapHandler(NullHandler):
    def __init__(self, log, pixmap):
        self.log = log
        self.pixmap = pixmap
        self.f = cStringIO.StringIO()

    def character_data(self, data):
        self.f.write(data)

    def end_element(self, name):
        self.f.seek(0)
        try:
            data = xorn.base64.decode(self.f)
        except xorn.base64.DecodingError:
            self.log.error(_("base64 decoding error"))
            return
        if self.pixmap.embedded:
            self.pixmap.data = data
        elif data != self.pixmap.data:
            self.log.warn(_("contents of pixmap file \"%s\" don't match "
                            "embedded data") % self.pixmap.filename)

class LoadContext:
    def __init__(self, log, load_symbol, load_pixmap):
        self.log = log
        self.ids = set()
        self.symbols = {}
        self.pixmaps = {}
        self.symbol_refs = []
        self.pixmap_refs = []
        self.load_symbol = load_symbol
        self.load_pixmap = load_pixmap

    def parse(self, x):
        return float(xorn.fixednum.parse(x, 2))

    def parse_attribute(self, d, key, default, processor, msg_fragment):
        try:
            x = d.pop(key)
        except KeyError:
            if default is not None:
                return default
            self.log.error(_("%s not specified") % msg_fragment)
        else:
            try:
                return processor(x)
            except (KeyError, ValueError):
                self.log.error(_("invalid %s \"%s\"") % (msg_fragment, x))

        # guess a well-formed return value from processor function
        return 0. if processor == self.parse else 0

    def parse_line(self, attributes):
        line = xorn.storage.LineAttr()
        line.width = self.parse_attribute(
            attributes, 'linewidth', 0, self.parse, 'line width')
        line.cap_style = self.parse_attribute(
            attributes, 'capstyle', 0, ENUM_CAPSTYLE.index, 'cap style')
        line.dash_style = self.parse_attribute(
            attributes, 'dashstyle', 0, ENUM_DASHSTYLE.index, 'dash style')
        if line.dash_style != 0 and line.dash_style != 1:
            line.dash_length = self.parse_attribute(
                attributes, 'dashlength', None, self.parse, 'dash length')
        else:
            line.dash_length = -1
        if line.dash_style != 0:
            line.dash_space = self.parse_attribute(
                attributes, 'dashspace', None, self.parse, 'dash space')
        else:
            line.dash_space = -1
        return line

    def parse_fill(self, attributes):
        fill = xorn.storage.FillAttr()
        fill.type = self.parse_attribute(
            attributes, 'filltype', 0, ENUM_FILLTYPE.index, 'fill type')
        if fill.type == 2 or fill.type == 3:
            fill.width = self.parse_attribute(
                attributes, 'fillwidth', None, self.parse, 'fill width')
            fill.angle0 = self.parse_attribute(
                attributes, 'angle0', None, int, 'first fill angle')
            fill.pitch0 = self.parse_attribute(
                attributes, 'pitch0', None, self.parse, 'first fill pitch')
        else:
            fill.width = -1
            fill.angle0 = -1
            fill.pitch0 = -1
        if fill.type == 2:
            fill.angle1 = self.parse_attribute(
                attributes, 'angle1', None, int, 'second fill angle')
            fill.pitch1 = self.parse_attribute(
                attributes, 'pitch1', None, self.parse, 'second fill pitch')
        else:
            fill.angle1 = -1
            fill.pitch1 = -1
        return fill

class RootElementHandler(NullHandler):
    def __init__(self, c):
        self.log = c.log
        self.c = c
        self.rev = xorn.storage.Revision()
        self.had_content = False

    def start_element(self, name, attributes):
        if name == 'content':
            if self.had_content:
                self.c.log.error(_("duplicate content tag"))
                return VoidHandler()
            self.had_content = True
            return ContentHandler(self.c, self.rev, None)

        if name == 'symbol':
            try:
                mode = attributes.pop('mode')
            except KeyError:
                self.c.log.error(_("symbol mode not specified"))
                return VoidHandler()
            if mode == 'omitted':
                read_symbol = False
                is_embedded = False
            elif mode == 'referenced':
                read_symbol = True
                is_embedded = False
            elif mode == 'embedded':
                read_symbol = True
                is_embedded = True
            else:
                self.c.log.error(_("invalid symbol mode \"%s\"") % mode)
                return VoidHandler()

            try:
                name = attributes.pop('name')
            except KeyError:
                if not is_embedded:
                    self.c.log.error(_("symbol name not specified"))
                    return VoidHandler()
                name = None
            if is_embedded:
                symbol = xorn.geda.ref.Symbol(name, None, True)
            else:
                symbol = self.c.load_symbol(name)
                assert not symbol.embedded
            try:
                symbol_id = attributes.pop('id')
            except KeyError:
                self.c.log.error(_("symbol id not specified"))
                return VoidHandler()
            if not symbol_id:
                self.c.log.error(_("symbol id can't be empty"))
                return VoidHandler()
            if symbol_id in self.c.ids:
                self.c.log.error(_("duplicate id \"%s\"") % symbol_id)
                return VoidHandler()
            self.c.ids.add(symbol_id)
            self.c.symbols[symbol_id] = symbol
            if not read_symbol:
                return NullHandler(self.c.log)

            reh = RootElementHandler(self.c)
            if is_embedded:
                symbol.prim_objs = reh.rev
            return reh

        if name == 'pixmap':
            try:
                mode = attributes.pop('mode')
            except KeyError:
                self.c.log.error(_("pixmap mode not specified"))
                return VoidHandler()
            if mode == 'omitted':
                read_pixmap = False
                is_embedded = False
            elif mode == 'referenced':
                read_pixmap = True
                is_embedded = False
            elif mode == 'embedded':
                read_pixmap = True
                is_embedded = True
            else:
                self.c.log.error(_("invalid pixmap mode \"%s\"") % mode)
                return VoidHandler()

            try:
                name = attributes.pop('name')
            except KeyError:
                if not is_embedded:
                    self.c.log.error(_("pixmap name not specified"))
                    return VoidHandler()
                name = None
            if is_embedded:
                pixmap = xorn.geda.ref.Pixmap(name, None, True)
            else:
                pixmap = self.c.load_pixmap(name)
                assert not pixmap.embedded
            try:
                pixmap_id = attributes.pop('id')
            except KeyError:
                self.c.log.error(_("pixmap id not specified"))
                return VoidHandler()
            if not pixmap_id:
                self.c.log.error(_("pixmap id can't be empty"))
                return VoidHandler()
            if pixmap_id in self.c.ids:
                self.c.log.error(_("duplicate id \"%s\"") % pixmap_id)
                return VoidHandler()
            self.c.ids.add(pixmap_id)
            self.c.pixmaps[pixmap_id] = pixmap
            if read_pixmap:
                return PixmapHandler(self.c.log, pixmap)
            else:
                return NullHandler(self.c.log)

        self.c.log.error(_("unexpected element \"%s\"") % name)
        return VoidHandler()

    def end_element(self, name):
        if not self.had_content:
            self.c.log.error(_("content missing"))

def read_file(f, name, log, load_symbol, load_pixmap):
    context = LoadContext(log, load_symbol, load_pixmap)
    reh = RootElementHandler(context)

    def start_root_element(name, attributes):
        if name != 'symbol' and name != 'schematic':
            log.error(_("invalid root element \"%s\"") % name)
            return VoidHandler()

        for feature in attributes.pop('file-format-features', '').split(' '):
            if not feature:
                continue
            if feature == 'experimental':
                pass
            else:
                log.error(_("unsupported file format feature \"%s\"")
                          % feature)

        return reh

    read_xml_file(f, log, NAMESPACE, start_root_element)

    for rev, ob, symbol_id, lineno in context.symbol_refs:
        if symbol_id not in context.symbols:
            log.lineno = lineno
            log.error(_("undefined symbol \"%s\"") % symbol_id)
            continue
        data = rev.get_object_data(ob)
        data.symbol = context.symbols[symbol_id]
        rev.set_object_data(ob, data)

    for rev, ob, pixmap_id, lineno in context.pixmap_refs:
        if pixmap_id not in context.pixmaps:
            log.lineno = lineno
            log.error(_("undefined pixmap \"%s\"") % pixmap_id)
            continue
        data = rev.get_object_data(ob)
        data.pixmap = context.pixmaps[pixmap_id]
        rev.set_object_data(ob, data)

    return xorn.proxy.RevisionProxy(reh.rev)

def read_xml_file(f, log, namespace, start_root_element):
    stack = []

    def strip_namespace(name, ignore_errors):
        try:
            pos = name.index(NSSEP)
        except ValueError:
            if not ignore_errors:
                log.error(_("element name \"%s\" without namespace") % name)
            return None

        if name[:pos] != namespace and not ignore_errors:
            log.error(_("invalid namespace \"%s\"") % name[:pos])
            return None

        return name[pos + 1:]

    def StartElementHandler(name, attributes):
        log.lineno = p.CurrentLineNumber - 1
        name = strip_namespace(name, False)
        if name is None:
            new_handler = VoidHandler()
        elif stack:
            new_handler = stack[-1].start_element(name, attributes)
        else:
            new_handler = start_root_element(name, attributes)
        stack.append(new_handler)

        if attributes and not isinstance(new_handler, VoidHandler):
            log.error(_("unexpected attribute(s) %s") % _(", ").join(
                _("\"%s\"") % attr for attr in sorted(attributes)))

    def EndElementHandler(name):
        log.lineno = p.CurrentLineNumber - 1
        name = strip_namespace(name, True)
        stack.pop().end_element(name)

    def CharacterDataHandler(data):
        log.lineno = p.CurrentLineNumber - 1
        stack[-1].character_data(data)

    def StartDoctypeDeclHandler(doctype_name, system_id, public_id,
                                has_internal_subset):
        log.lineno = p.CurrentLineNumber - 1
        log.error(_("unexpected XML document type declaration"))

    def ElementDeclHandler(name, model):
        log.lineno = p.CurrentLineNumber - 1
        log.error(_("unexpected XML element type declaration"))

    def AttlistDeclHandler(elname, attname, type, default, required):
        log.lineno = p.CurrentLineNumber - 1
        log.error(_("unexpected XML element type attribute declaration"))

    def ProcessingInstructionHandler(target, data):
        log.lineno = p.CurrentLineNumber - 1
        log.error(_("unexpected XML processing instruction"))

    def UnparsedEntityDeclHandler(entity_name, base, system_id, public_id,
                                  notationName):
        log.lineno = p.CurrentLineNumber - 1
        log.error(_("unexpected XML unparsed entity declaration"))

    def EntityDeclHandler(entity_name, is_parameter_entity, value, base,
                          system_id, public_id, notation_name):
        log.lineno = p.CurrentLineNumber - 1
        log.error(_("unexpected XML entity declaration"))

    def NotationDeclHandler(notation_name, base, system_id, public_id):
        log.lineno = p.CurrentLineNumber - 1
        log.error(_("unexpected XML notation declaration"))

    def StartCdataSectionHandler():
        log.lineno = p.CurrentLineNumber - 1
        log.error(_("unexpected XML CDATA section"))

    def DefaultHandler(data):
        log.lineno = p.CurrentLineNumber - 1
        log.error(_("unexpected characters in XML document"))

    def NotStandaloneHandler():
        log.lineno = p.CurrentLineNumber - 1
        log.error(_("XML document hasn't been declared as standalone"))

    def ExternalEntityRefHandler(context, base, systemId, publicId):
        log.lineno = p.CurrentLineNumber - 1
        log.error(_("unexpected reference to external XML entity"))

    p = xml.parsers.expat.ParserCreate(namespace_separator = '!')

    p.XmlDeclHandler = None
    p.StartDoctypeDeclHandler = StartDoctypeDeclHandler
    p.EndDoctypeDeclHandler = None
    p.ElementDeclHandler = ElementDeclHandler
    p.AttlistDeclHandler = AttlistDeclHandler
    p.StartElementHandler = StartElementHandler
    p.EndElementHandler = EndElementHandler
    p.ProcessingInstructionHandler = ProcessingInstructionHandler
    p.CharacterDataHandler = CharacterDataHandler
    p.UnparsedEntityDeclHandler = UnparsedEntityDeclHandler
    p.EntityDeclHandler = EntityDeclHandler
    p.NotationDeclHandler = NotationDeclHandler
    p.StartNamespaceDeclHandler = None
    p.EndNamespaceDeclHandler = None
    p.CommentHandler = None
    p.StartCdataSectionHandler = StartCdataSectionHandler
    p.EndCdataSectionHandler = None
    p.DefaultHandler = DefaultHandler
    p.DefaultHandlerExpand = None
    p.NotStandaloneHandler = NotStandaloneHandler
    p.ExternalEntityRefHandler = ExternalEntityRefHandler

    try:
        p.ParseFile(f)
    except xml.parsers.expat.ExpatError as e:
        log.lineno = e.lineno - 1
        log.error(_("%s") % e)
