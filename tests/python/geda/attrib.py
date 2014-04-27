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

import xorn.proxy
import xorn.geda.attrib as GA

def throws(fun, *args):
    try:
        fun(*args)
    except Exception as e:
        return type(e)

rev = xorn.proxy.RevisionProxy(xorn.storage.Revision())
ob00 = rev.add_object(xorn.storage.Text(text = 'foo0bar0'))
ob01 = rev.add_object(xorn.storage.Text(text = 'foo0=bar0'))
ob02 = rev.add_object(xorn.storage.Text(text = 'foo0baz0'))
ob03 = rev.add_object(xorn.storage.Text(text = 'foo0=baz0'))
ob10 = rev.add_object(xorn.storage.Text(text = 'foo1bar1'))
ob11 = rev.add_object(xorn.storage.Text(text = 'foo1=bar1'))
ob12 = rev.add_object(xorn.storage.Text(text = 'foo1baz1'))
ob13 = rev.add_object(xorn.storage.Text(text = 'foo1=baz1'))
ob20 = rev.add_object(xorn.storage.Text(text = 'foo2bar2'))
ob21 = rev.add_object(xorn.storage.Text(text = 'foo2=bar2'))
ob22 = rev.add_object(xorn.storage.Text(text = 'foo2baz2'))
ob23 = rev.add_object(xorn.storage.Text(text = 'foo2=baz2'))

net = rev.add_object(xorn.storage.Net())
ob10.relocate(net, None)
ob11.relocate(net, None)
ob12.relocate(net, None)
ob13.relocate(net, None)

component = rev.add_object(xorn.storage.Component())
ob20.relocate(component, None)
ob21.relocate(component, None)
ob22.relocate(component, None)
ob23.relocate(component, None)

class Symbol:
    pass

component.symbol = Symbol()
component.symbol.prim_objs = xorn.storage.Revision()
srev = xorn.proxy.RevisionProxy(component.symbol.prim_objs)

ob30 = srev.add_object(xorn.storage.Text(text = 'foo3bar3'))
ob31 = srev.add_object(xorn.storage.Text(text = 'foo3=bar3'))
ob32 = srev.add_object(xorn.storage.Text(text = 'foo3baz3'))
ob33 = srev.add_object(xorn.storage.Text(text = 'foo3=baz3'))
ob40 = srev.add_object(xorn.storage.Text(text = 'foo4bar4'))
ob41 = srev.add_object(xorn.storage.Text(text = 'foo4=bar4'))
ob42 = srev.add_object(xorn.storage.Text(text = 'foo4baz4'))
ob43 = srev.add_object(xorn.storage.Text(text = 'foo4=baz4'))

pin = srev.add_object(xorn.storage.Net(is_pin = True))
ob40.relocate(pin, None)
ob41.relocate(pin, None)
ob42.relocate(pin, None)
ob43.relocate(pin, None)

nonexisting = rev.add_object(xorn.storage.Circle())
rev.delete_object(nonexisting)

# is_attribute

assert GA.is_attribute(ob00) == False
assert GA.is_attribute(ob01) == True
assert GA.is_attribute(ob02) == False
assert GA.is_attribute(ob03) == True
assert GA.is_attribute(net) == False
assert GA.is_attribute(component) == False
assert GA.is_attribute(pin) == False

assert throws(GA.is_attribute, nonexisting) == KeyError

# inherited_objects

assert GA.inherited_objects(component) == [ob30, ob31, ob32, ob33, pin]

assert throws(GA.inherited_objects, ob00) == ValueError
assert throws(GA.inherited_objects, ob01) == ValueError
assert throws(GA.inherited_objects, net) == ValueError
assert throws(GA.inherited_objects, pin) == ValueError
assert throws(GA.inherited_objects, nonexisting) == KeyError

# find_floating_attribs

assert GA.find_floating_attribs(rev) == [ob01, ob03]
assert GA.find_floating_attribs(srev) == [ob31, ob33]

# find_attached_attribs

assert GA.find_attached_attribs(ob00) == []
assert GA.find_attached_attribs(ob01) == []
assert GA.find_attached_attribs(net) == [ob11, ob13]
assert GA.find_attached_attribs(component) == [ob21, ob23]
assert GA.find_attached_attribs(pin) == [ob41, ob43]

assert throws(GA.find_attached_attribs, nonexisting) == KeyError

# find_inherited_attribs

assert GA.find_inherited_attribs(component) == [ob31, ob33]

assert throws(GA.find_inherited_attribs, ob00) == ValueError
assert throws(GA.find_inherited_attribs, ob01) == ValueError
assert throws(GA.find_inherited_attribs, net) == ValueError
assert throws(GA.find_inherited_attribs, pin) == ValueError
assert throws(GA.find_inherited_attribs, nonexisting) == KeyError

# find_all_attribs

assert GA.find_all_attribs(component) == [ob21, ob23, ob31, ob33]

assert throws(GA.find_all_attribs, ob00) == ValueError
assert throws(GA.find_all_attribs, ob01) == ValueError
assert throws(GA.find_all_attribs, net) == ValueError
assert throws(GA.find_all_attribs, pin) == ValueError
assert throws(GA.find_all_attribs, nonexisting) == KeyError

# search

assert GA.search([ob00, ob01, ob02, ob03,
                  ob10, ob11, ob12, ob13], 'foo0') == ['bar0', 'baz0']
assert GA.search([ob00, ob01, ob02, ob03,
                  ob10, ob11, ob12, ob13], 'foo1') == ['bar1', 'baz1']

# search_floating

assert GA.search_floating(rev, 'foo0') == ['bar0', 'baz0']
assert GA.search_floating(rev, 'foo1') == []
assert GA.search_floating(rev, 'foo2') == []
assert GA.search_floating(rev, 'foo3') == []
assert GA.search_floating(rev, 'foo4') == []

assert GA.search_floating(srev, 'foo0') == []
assert GA.search_floating(srev, 'foo1') == []
assert GA.search_floating(srev, 'foo2') == []
assert GA.search_floating(srev, 'foo3') == ['bar3', 'baz3']
assert GA.search_floating(srev, 'foo4') == []

# search_attached

assert GA.search_attached(ob00, 'foo0') == []
assert GA.search_attached(ob00, 'foo1') == []
assert GA.search_attached(ob00, 'foo2') == []
assert GA.search_attached(ob00, 'foo3') == []
assert GA.search_attached(ob00, 'foo4') == []

assert GA.search_attached(ob01, 'foo0') == []
assert GA.search_attached(ob01, 'foo1') == []
assert GA.search_attached(ob01, 'foo2') == []
assert GA.search_attached(ob01, 'foo3') == []
assert GA.search_attached(ob01, 'foo4') == []

assert GA.search_attached(net, 'foo0') == []
assert GA.search_attached(net, 'foo1') == ['bar1', 'baz1']
assert GA.search_attached(net, 'foo2') == []
assert GA.search_attached(net, 'foo3') == []
assert GA.search_attached(net, 'foo4') == []

assert GA.search_attached(component, 'foo0') == []
assert GA.search_attached(component, 'foo1') == []
assert GA.search_attached(component, 'foo2') == ['bar2', 'baz2']
assert GA.search_attached(component, 'foo3') == []
assert GA.search_attached(component, 'foo4') == []

assert GA.search_attached(pin, 'foo0') == []
assert GA.search_attached(pin, 'foo1') == []
assert GA.search_attached(pin, 'foo2') == []
assert GA.search_attached(pin, 'foo3') == []
assert GA.search_attached(pin, 'foo4') == ['bar4', 'baz4']

assert throws(GA.search_attached, nonexisting, 'foo0') == KeyError

# search_inherited

assert GA.search_inherited(component, 'foo0') == []
assert GA.search_inherited(component, 'foo1') == []
assert GA.search_inherited(component, 'foo2') == []
assert GA.search_inherited(component, 'foo3') == ['bar3', 'baz3']
assert GA.search_inherited(component, 'foo4') == []

assert throws(GA.search_inherited, ob00, 'foo0') == ValueError
assert throws(GA.search_inherited, ob01, 'foo0') == ValueError
assert throws(GA.search_inherited, net, 'foo0') == ValueError
assert throws(GA.search_inherited, pin, 'foo0') == ValueError
assert throws(GA.search_inherited, nonexisting, 'foo0') == KeyError

# search_all

assert GA.search_all(component, 'foo0') == []
assert GA.search_all(component, 'foo1') == []
assert GA.search_all(component, 'foo2') == ['bar2', 'baz2']
assert GA.search_all(component, 'foo3') == ['bar3', 'baz3']
assert GA.search_all(component, 'foo4') == []

assert throws(GA.search_all, ob00, 'foo0') == ValueError
assert throws(GA.search_all, ob01, 'foo0') == ValueError
assert throws(GA.search_all, net, 'foo0') == ValueError
assert throws(GA.search_all, pin, 'foo0') == ValueError
assert throws(GA.search_all, nonexisting, 'foo0') == KeyError

# find_pins_by_attribute

assert GA.find_pins_by_attribute(component, 'foo3', 'bar3') == []
assert GA.find_pins_by_attribute(component, 'foo3', 'baz3') == []
assert GA.find_pins_by_attribute(component, 'foo3', 'bar4') == []
assert GA.find_pins_by_attribute(component, 'foo3', 'baz4') == []
assert GA.find_pins_by_attribute(component, 'foo4', 'bar3') == []
assert GA.find_pins_by_attribute(component, 'foo4', 'baz3') == []
assert GA.find_pins_by_attribute(component, 'foo4', 'bar4') == [pin]
assert GA.find_pins_by_attribute(component, 'foo4', 'baz4') == [pin]

pin.is_pin = False
assert GA.find_pins_by_attribute(component, 'foo4', 'bar4') == []
assert GA.find_pins_by_attribute(component, 'foo4', 'baz4') == []

assert throws(GA.find_pins_by_attribute, ob00, 'foo3', 'bar3') == ValueError
assert throws(GA.find_pins_by_attribute, ob01, 'foo3', 'bar3') == ValueError
assert throws(GA.find_pins_by_attribute, net, 'foo3', 'bar3') == ValueError
assert throws(GA.find_pins_by_attribute, pin, 'foo3', 'bar3') == ValueError
assert throws(GA.find_pins_by_attribute,
              nonexisting, 'foo3', 'bar3') == KeyError
