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

def assert_this_net(rev, ob, color):
    objects = rev.get_objects()
    assert objects == [ob]
    data = rev.get_object_data(ob)
    assert type(data) == xorn.storage.Net
    assert data.color == color

def assert_this_net_with_text(rev, ob, color, text_color):
    objects = xorn.storage.get_objects_attached_to(rev, None)
    assert objects == [ob]
    data = rev.get_object_data(ob)
    assert type(data) == xorn.storage.Net
    assert data.color == color

    objects = xorn.storage.get_objects_attached_to(rev, ob)
    assert type(objects) == list
    assert len(objects) == 1
    data = rev.get_object_data(objects[0])
    assert type(data) == xorn.storage.Text
    assert data.color == text_color

def get_only_selected_object(rev, sel):
    objects = xorn.storage.get_selected_objects(rev, sel)
    assert type(objects) == list
    assert len(objects) == 1
    return objects[0]

def assert_four(rev, sel, net0_color, net1_color, text0_color, text1_color):
    objects = xorn.storage.get_objects_attached_to(rev, None)
    assert type(objects) == list
    assert len(objects) == 3

    if True:
        data = rev.get_object_data(objects[0])
        assert type(data) == xorn.storage.Net
        assert data.color == net0_color
        net0 = objects[0]

        data = rev.get_object_data(objects[1])
        assert type(data) == xorn.storage.Net
        assert data.color == net1_color
        net1 = objects[1]

        data = rev.get_object_data(objects[2])
        assert type(data) == xorn.storage.Text
        assert data.color == text0_color
        text0 = objects[2]

    assert xorn.storage.get_objects_attached_to(rev, net0) == []

    objects = xorn.storage.get_objects_attached_to(rev, net1)
    assert type(objects) == list
    assert len(objects) == 1
    data = rev.get_object_data(objects[0])
    assert type(data) == xorn.storage.Text
    assert data.color == text1_color
    text1 = objects[0]

    assert xorn.storage.get_objects_attached_to(rev, text0) == []
    assert xorn.storage.get_objects_attached_to(rev, text1) == []

    assert xorn.storage.object_is_selected(rev, sel, net0) == True
    assert xorn.storage.object_is_selected(rev, sel, net1) == True
    assert xorn.storage.object_is_selected(rev, sel, text0) == True
    assert xorn.storage.object_is_selected(rev, sel, text1) == False


src = xorn.storage.Revision()
assert src is not None

net_data = xorn.storage.Net()
net_data.color = 1
net0 = src.add_object(net_data)
net_data.color = 2
net1 = src.add_object(net_data)

text_data = xorn.storage.Text()
text_data.color = 3
text0 = src.add_object(text_data)
text_data.color = 4
text1 = src.add_object(text_data)

assert net0 is not None
assert net1 is not None
assert text0 is not None
assert text1 is not None

src.relocate_object(text1, net1, None)
src.finalize()

# text1 is attached to net1, text0 is not attached

dest = xorn.storage.Revision()
assert dest is not None
copy = dest.copy_object(src, net0)
assert_this_net(dest, copy, 1)

dest = xorn.storage.Revision()
assert dest is not None
copy = dest.copy_object(src, net1)
assert_this_net_with_text(dest, copy, 2, 4)

dest = xorn.storage.Revision()
assert dest is not None
sel = xorn.storage.select_object(net0)
copies = dest.copy_objects(src, sel)
assert_this_net(dest, get_only_selected_object(dest, copies), 1)

dest = xorn.storage.Revision()
assert dest is not None
sel = xorn.storage.select_object(net1)
copies = dest.copy_objects(src, sel)
assert_this_net_with_text(dest, get_only_selected_object(dest, copies), 2, 4)

dest = xorn.storage.Revision()
assert dest is not None
sel = xorn.storage.select_attached_to(src, None)
copies = dest.copy_objects(src, sel)
assert_four(dest, copies, 1, 2, 3, 4)
