# xorn.geda - Python library for manipulating gEDA files
# Copyright (C) 1998-2010 Ales Hvezda
# Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
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

## \namespace xorn.geda.attrib
## Attribute parsing and lookup.
#
# An attribute is a normal text object with a string that is delimited
# by an equals sign ("=").  The part before the equals sign is called
# \a name and the part of the string behind the equals sign is called
# \a value.  Attributes can either be floating (that is, not attached
# to any object) or attached to a net (including pins and buses) or
# component.
#
# \note If one of the parts is empty, the name ends with a space
#       character, or the value starts with a space character, the
#       text object is not recognized as an attribute.
#
# In addition to its attached attributes, a component inherits the
# attributes which live as toplevel un-attached attributes inside the
# \c prim_objs of its symbol.

import xorn.storage
import xorn.proxy

## Raised when trying to parse a text object that is not recognized as
## an attribute.

class MalformedAttributeError(Exception):
    pass

## Parse an attribute string of the form \c name=value into its name
## and value parts.
#
# \throws MalformedAttributeError if the passed string is not a
# syntactically correct attribute string
#
# \return A pair <tt>(name, value)</tt>.

def parse_string(string):
    try:
        ptr = string.index('=')
    except ValueError:
        raise MalformedAttributeError
    if ptr == 0 or ptr == len(string) - 1:
        raise MalformedAttributeError
    if string[ptr - 1] == ' ' or string[ptr + 1] == ' ':
        raise MalformedAttributeError

    return string[:ptr], string[ptr + 1:]

## Tell whether an object is a text object whose text constitutes a
## valid attribute.
#
# \throw KeyError if the object doesn't exist

def is_attribute(ob):
    data = ob.data()
    if not isinstance(data, xorn.storage.Text):
        return False  # not a text
    try:
        parse_string(data.text)
    except MalformedAttributeError:
        return False  # not a valid attribute

    return True

## Returns the toplevel objects of a component's symbol.
#
# \throw KeyError   if the object doesn't exist
# \throw ValueError if the object isn't a component

def inherited_objects(ob):
    data = ob.data()
    if not isinstance(data, xorn.storage.Component):
        raise ValueError
    return xorn.proxy.RevisionProxy(data.symbol.prim_objs).toplevel_objects()

## Return all floating attributes in the given revision.

def find_floating_attribs(rev):
    return [ob for ob in rev.toplevel_objects() if is_attribute(ob)]

## Return the attributes directly attached to a net or component.
#
# \throw KeyError if the object doesn't exist

def find_attached_attribs(ob):
    return [attrib for attrib in ob.attached_objects() if is_attribute(attrib)]

## Return the attributes inherited by a component via its symbol.
#
# \throw KeyError   if the object doesn't exist
# \throw ValueError if the object isn't a component

def find_inherited_attribs(ob):
    return [attrib for attrib in inherited_objects(ob) if is_attribute(attrib)]

## Return all attributes, attached and inherited, of the specified
## component.
#
# This function aggregates the attached and inherited attributes
# belonging to a given object.
#
# \throw KeyError   if the object doesn't exist
# \throw ValueError if the object isn't a component

def find_all_attribs(ob):
    return [attrib for attrib in ob.attached_objects() + inherited_objects(ob)
                              if is_attribute(attrib)]


## Search a list of objects for attributes with a certain name and
## return their values.
#
# \return List of strings with the values of the matching attributes.

def search(attribs, name):
    found = []
    for attrib in attribs:
        data = attrib.data()
        if not isinstance(data, xorn.storage.Text):
            continue
        try:
            attrib_name, attrib_value = parse_string(data.text)
        except MalformedAttributeError:
            continue
        if attrib_name == name:
            found += [attrib_value]
    return found

## Search the floating attributes in a revision for an attribute name
## and return matching values.
#
# \return List of strings with the values of the matching attributes.

def search_floating(rev, name):
    return search(rev.toplevel_objects(), name)

## Search attributes attached to a net or component for an attribute
## name and return matching values.
#
# \throw KeyError   if the object doesn't exist
#
# \return List of strings with the values of the matching attributes.

def search_attached(ob, name):
    return search(ob.attached_objects(), name)

## Search attributes inherited by a component for an attribute name
## and return matching values.
#
# \throw KeyError   if the object doesn't exist
# \throw ValueError if the object isn't a component
#
# \return List of strings with the values of the matching attributes.

def search_inherited(ob, name):
    return search(inherited_objects(ob), name)

## Search both attached and inherited attributes of a component for an
## attribute name and return matching values.
#
# For nets, use \ref search_attached instead.
#
# \throw KeyError   if the object doesn't exist
# \throw ValueError if the object isn't a component
#
# \return List of strings with the values of the matching attributes.

def search_all(ob, name):
    return search(ob.attached_objects() + inherited_objects(ob), name)


## Return all pins in a component with a particular attribute.
#
# \throw KeyError   if \a ob doesn't exist
# \throw ValueError if \a ob isn't a component
#
# \return A list of pins with the given attribute.

def find_pins_by_attribute(ob, name, value):
    found = []
    for pin in inherited_objects(ob):
        data = pin.data()
        if isinstance(data, xorn.storage.Net) and data.is_pin and \
                value in search_attached(pin, name):
            found += [pin]
    return found
