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

## \namespace xorn.xml_writer
## Writing XML documents.
#
# See the documentation of the class XMLWriter for more information.

STATE_IDLE, STATE_CHARACTER_DATA, STATE_START_TAG = xrange(3)

## Checks whether a string is a valid XML name.

def valid_name(name):
    if not isinstance(name, unicode):
        raise TypeError, "invalid argument type (must be unicode)"

    if not name:
        return False

    c = name[0]
    if c == '-' or \
            c == '.' or \
            (c >= '0' and c <= '9') or \
            c == 0xb7 or \
            (c >= 0x0300 and c <= 0x036f) or \
            (c >= 0x203f and c <= 0x2040):
        return False

    for c in name:
        if c != '-' and c != '.' and \
                not (c >= '0' and c <= '9') and c != ':' and \
                not (c >= 'A' and c <= 'Z') and c != '_' and \
                not (c >= 'a' and c <= 'z') and c != 0xb7 and \
                not (c >= u'\u00c0' and c <= u'\u00d6') and \
                not (c >= u'\u00d8' and c <= u'\u00f6') and \
                not (c >= u'\u00f8' and c <= u'\u02ff') and \
                not (c >= u'\u0300' and c <= u'\u036f') and \
                not (c >= u'\u0370' and c <= u'\u037d') and \
                not (c >= u'\u037f' and c <= u'\u1fff') and \
                not (c >= u'\u200c' and c <= u'\u200d') and \
                not (c >= u'\u203f' and c <= u'\u2040') and \
                not (c >= u'\u2070' and c <= u'\u218f') and \
                not (c >= u'\u2c00' and c <= u'\u2fef') and \
                not (c >= u'\u3001' and c <= u'\ud7ff') and \
                not (c >= u'\uf900' and c <= u'\ufdcf') and \
                not (c >= u'\ufdf0' and c <= u'\ufffd'):
            # and not (c >= 0x10000 and c <= 0xeffff)
            return False

    return True

## Escape XML metacharacters in a string.

def escape(data):
    if not isinstance(data, unicode):
        raise TypeError, "invalid argument type (must be unicode)"

    return data.replace('&', '&amp;') \
               .replace('<', '&lt;') \
               .replace('>', '&gt;') \
               .replace('"', '&quot;')

## Writing XML documents.
#
# This class is used for creating XML documents and writing them to
# some output function in a stream-like fashion.  It does not create
# an in-memory representation of the document.
#
# Example usage:
# \snippet xml_writer.py use XMLWriter
#
# This yields the following output:
# \code{.xml}
# <?xml version="1.0" encoding="UTF-8"?>
# <document attribute="value">
#   some text
# </document>
# \endcode
#
# \note The caller must only pass characters which are valid in XML,
#       i.e. none of 00-1f (except 09, 0a, and 0d), d800-dfff, fffe,
#       ffff, or 110000 and above.

class XMLWriter:
    ## Create an \c %XMLWriter instance.
    #
    # \a write must be a function accepting a single \c unicode object
    # as an argument which is called to write the actual data.
    #
    # You need to create a separate \c %XMLWriter instance for each
    # XML document.

    def __init__(self, write):
        self.write = write
        self.is_initialized = False
        self.has_root_element = False
        self.stack = []
        self.preserve_depth = None
        self.state = STATE_IDLE
        self.current_attrs = set()

    ## Return whether the document written so far is complete.

    def is_done(self):
        return self.has_root_element and not self.stack

    ## Callback for writing out data.
    #
    # When creating an instance, this is set to the \a write argument
    # to the \c %XMLWriter constructor.

    def write(self, data):
        raise NotImplementedError

    def _prepare_for_data(self):
        if not self.is_initialized:
            self.write('<?xml version="1.0" encoding="UTF-8"?>\n')
            self.is_initialized = True

        if self.state == STATE_START_TAG:
            self.write('>')
            if self.preserve_depth is None:
                self.write('\n')
            self.current_attrs.clear()
            self.state = STATE_IDLE

    ## Write an opening tag for a new element.
    #
    # \a name must be a valid XML element name.  If \a
    # preserve_whitespace is \c True, no extra formatting whitespace
    # will be inserted inside this element.

    def start_element(self, name, preserve_whitespace = False):
        name = unicode(name)
        if not valid_name(name):
            raise ValueError, "invalid element name '%s'" % name

        if not self.stack:
            if self.has_root_element:
                raise ValueError, "only one root element allowed"
            self.has_root_element = True

        self._prepare_for_data()
        if self.preserve_depth is None:
            if self.state == STATE_CHARACTER_DATA:
                self.write('\n')
            self.write('  ' * len(self.stack))
        self.write('<' + name)
        if preserve_whitespace and self.preserve_depth is None:
            self.preserve_depth = len(self.stack)
        self.stack.append(name)
        self.state = STATE_START_TAG

    ## Write a closing tag for the innermost element.

    def end_element(self):
        if self.state == STATE_START_TAG:
            self.write('/>')
        elif self.stack:
            if self.preserve_depth is None:
                if self.state == STATE_CHARACTER_DATA:
                    self.write('\n')
                self.write('  ' * (len(self.stack) - 1))
            self.write('</%s>' % self.stack[-1])
        else:
            raise ValueError, "can't end element at root level"

        self.state = STATE_IDLE

        del self.stack[-1]
        if self.preserve_depth == len(self.stack):
            self.preserve_depth = None
        if self.preserve_depth is None:
            self.write('\n')
        self.current_attrs.clear()

    ## Write an attribute for the innermost element.
    #
    # \a name must be a valid XML element name.  In \a value, the
    # characters \c '&', \c '<', \c '>', and \c '"' are replaced with
    # their entity representations.
    #
    # All attributes must be written before any character data, \c
    # CDATA sections, or child elements are written.

    def write_attribute(self, name, value):
        name = unicode(name)
        value = unicode(value)
        if self.state != STATE_START_TAG:
            raise ValueError, "can't write attributes right now"
        if not valid_name(name):
            raise ValueError, "invalid attribute name '%s'" % name
        if '\n' in value:
            raise ValueError, "line feed character in attribute value"
        # TODO: validate value

        if name in self.current_attrs:
            raise ValueError, "duplicate attribute name '%s'" % name
        self.current_attrs.add(name)

        self.write(' %s="%s"' % (name, escape(value)))

    ## Write character data.
    #
    # The characters \c '&', \c '<', \c '>', and \c '"' are replaced
    # with their entity representations.

    def write_character_data(self, data):
        data = unicode(data)
        # TODO: validate data
        if not self.stack:
            raise ValueError, \
                "can't write character data outside of root element"

        self._prepare_for_data()
        if self.state == STATE_IDLE and self.preserve_depth is None:
            self.write('  ' * len(self.stack))
        self.state = STATE_CHARACTER_DATA

        self.write(escape(data))

    ## Write a \c CDATA section.
    #
    # Special characters in this section are not escaped except for
    # the character sequence <tt>']]>'</tt>.

    def write_cdata_section(self, data):
        data = unicode(data)
        # TODO: validate data
        if not self.stack:
            raise ValueError, \
                "can't write CDATA section outside of root element"

        self._prepare_for_data()
        if self.state == STATE_IDLE and self.preserve_depth is None:
            self.write('  ' * len(self.stack))
        self.state = STATE_CHARACTER_DATA

        self.write('<![CDATA[')
        self.write(data.replace(']]>', ']]>]]&gt;<![CDATA['))
        self.write(']]>')
