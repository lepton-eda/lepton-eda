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

import xorn.storage
import xorn.proxy

def throws(fun, *args):
    try:
        fun(*args)
    except Exception as e:
        return type(e)

rev = xorn.storage.Revision()
rev1 = xorn.storage.Revision()
ob = rev.add_object(xorn.storage.Box())
ob1 = rev.add_object(xorn.storage.Box())
sel = xorn.storage.select_object(ob)

rp = xorn.proxy.RevisionProxy(rev)
rp1 = xorn.proxy.RevisionProxy(rev1)
op = xorn.proxy.ObjectProxy(rev, ob)
op1 = xorn.proxy.ObjectProxy(rev, ob1)
ap = xorn.proxy.AttributeProxy(rev, ob, 'line')
ap1 = xorn.proxy.AttributeProxy(rev, ob, 'invalid_attribute')

# comparison

assert xorn.proxy.RevisionProxy(rev) == rp
assert xorn.proxy.RevisionProxy(rev) != rp1
assert xorn.proxy.RevisionProxy(rev1) != rp
assert xorn.proxy.RevisionProxy(rev1) == rp1

assert hash(xorn.proxy.RevisionProxy(rev)) == hash(rp)
assert hash(xorn.proxy.RevisionProxy(rev)) != hash(rp1)
assert hash(xorn.proxy.RevisionProxy(rev1)) != hash(rp)
assert hash(xorn.proxy.RevisionProxy(rev1)) == hash(rp1)

assert xorn.proxy.ObjectProxy(rev, ob) == op
assert xorn.proxy.ObjectProxy(rev, ob) != op1
assert xorn.proxy.ObjectProxy(rev, ob1) != op
assert xorn.proxy.ObjectProxy(rev, ob1) == op1
assert xorn.proxy.ObjectProxy(rev1, ob) != op
assert xorn.proxy.ObjectProxy(rev1, ob) != op1
assert xorn.proxy.ObjectProxy(rev1, ob1) != op
assert xorn.proxy.ObjectProxy(rev1, ob1) != op1

assert hash(xorn.proxy.ObjectProxy(rev, ob)) == hash(op)
assert hash(xorn.proxy.ObjectProxy(rev, ob)) != hash(op1)
assert hash(xorn.proxy.ObjectProxy(rev, ob1)) != hash(op)
assert hash(xorn.proxy.ObjectProxy(rev, ob1)) == hash(op1)
assert hash(xorn.proxy.ObjectProxy(rev1, ob)) != hash(op)
assert hash(xorn.proxy.ObjectProxy(rev1, ob)) != hash(op1)
assert hash(xorn.proxy.ObjectProxy(rev1, ob1)) != hash(op)
assert hash(xorn.proxy.ObjectProxy(rev1, ob1)) != hash(op1)

# object attributes

assert op.color == 0
rev.set_object_data(ob, xorn.storage.Box(color = 5))
assert op.color == 5
op.color = 7
assert rev.get_object_data(ob).color == 7

ap2 = op.line
assert isinstance(ap2, xorn.proxy.AttributeProxy)
assert ap2.rev == rev
assert ap2.ob == ob
assert ap2.name == 'line'
del ap2

op.line = xorn.storage.LineAttr(width = 2.5)
assert rev.get_object_data(ob).line.width == 2.5

try:
    op.invalid_attribute
except AttributeError:
    pass
else:
    raise AssertionError

assert 'xorn.storage.Box' in repr(op.data())
assert 'xorn.storage.Box' not in repr(op)

# attribute attributes

assert ap.width == 2.5
rev.set_object_data(ob, xorn.storage.Box(
        line = xorn.storage.LineAttr(width = 4.5)))
assert ap.width == 4.5
ap.width = 6.5
assert rev.get_object_data(ob).line.width == 6.5

try:
    ap.invalid_attribute
except AttributeError:
    pass
else:
    raise AssertionError

assert 'xorn.storage.LineAttr' in repr(ap.data())
assert 'xorn.storage.LineAttr' not in repr(ap)

# revision methods

assert rp.toplevel_objects() == [op, op1]
assert rp1.toplevel_objects() == []

assert rp.all_objects() == [op, op1]
assert rp1.all_objects() == []

assert rp.selected_objects(sel) == [op]
assert rp1.selected_objects(sel) == []

op2 = rp1.add_object(xorn.storage.Box())
assert op2.rev == rev1
assert rev1.object_exists(op2.ob)

op3 = rp1.copy_object(op)
assert op3.rev == rev1
assert rev1.object_exists(op3.ob)

sel4 = rp1.copy_objects(rp, sel)
assert xorn.storage.selection_is_empty(rev, sel4) == True
assert xorn.storage.selection_is_empty(rev1, sel4) == False
op4, = rp1.selected_objects(sel4)

rp1.delete_object(op3)
assert not rev1.object_exists(op3.ob)
assert xorn.storage.selection_is_empty(rev1, sel4) == False

rp1.delete_objects(sel4)
assert xorn.storage.selection_is_empty(rev1, sel4) == True

assert rp.is_transient()
rp.finalize()
assert not rp.is_transient()

# object methods

assert op.exists()
assert op1.exists()
assert op2.exists()
assert not op3.exists()
assert not op4.exists()

assert isinstance(op.data(), xorn.storage.Box)
assert isinstance(op1.data(), xorn.storage.Box)
assert isinstance(op2.data(), xorn.storage.Box)
assert throws(op3.data) == KeyError
assert throws(op4.data) == KeyError

assert throws(op.set_data, xorn.storage.Circle()) == ValueError
assert throws(op1.set_data, xorn.storage.Circle()) == ValueError
op2.set_data(xorn.storage.Circle())
op3.set_data(xorn.storage.Circle())

assert isinstance(op.data(), xorn.storage.Box)
assert isinstance(op1.data(), xorn.storage.Box)
assert isinstance(op2.data(), xorn.storage.Circle)
assert isinstance(op3.data(), xorn.storage.Circle)
assert throws(op4.data) == KeyError

assert op.location() == (None, 0)
assert op1.location() == (None, 1)
assert op2.location() == (None, 0)
assert op3.location() == (None, 1)
assert throws(op4.location) == KeyError

assert op.attached_objects() == []
assert op1.attached_objects() == []
assert op2.attached_objects() == []
assert op3.attached_objects() == []
assert throws(op4.attached_objects) == KeyError

assert throws(op.relocate, None, None) == ValueError
assert throws(op1.relocate, None, None) == ValueError
assert throws(op2.relocate, None, None) == None
assert throws(op3.relocate, None, None) == None
assert throws(op4.relocate, None, None) == KeyError

assert op.is_selected(sel) == True
assert op1.is_selected(sel) == False
assert op2.is_selected(sel) == False
assert op3.is_selected(sel) == False
assert op4.is_selected(sel) == False

# attribute methods

assert ap.exists() == True
assert ap1.exists() == False

assert isinstance(ap.data(), xorn.storage.LineAttr)
assert throws(ap1.data) == AttributeError

# sanity checks

assert throws(xorn.proxy.RevisionProxy, None) == ValueError
assert throws(xorn.proxy.ObjectProxy, None, ob) == ValueError
assert throws(xorn.proxy.ObjectProxy, rev, None) == ValueError
assert throws(xorn.proxy.AttributeProxy, None, ob, 'line') == ValueError
assert throws(xorn.proxy.AttributeProxy, rev, None, 'line') == ValueError
assert throws(xorn.proxy.AttributeProxy, rev, ob, None) == ValueError
