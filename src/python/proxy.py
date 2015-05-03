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

## \namespace xorn.proxy
#  \brief High-level proxy classes for the storage backend.

import xorn.storage

## \brief High-level revision proxy class.

class RevisionProxy:
    def __init__(self, rev):
        if not isinstance(rev, xorn.storage.Revision):
            raise ValueError
        self.rev = rev

    def __eq__(self, other):
        return isinstance(other, RevisionProxy) and self.rev == other.rev

    def __ne__(self, other):
        return not isinstance(other, RevisionProxy) or self.rev != other.rev

    def __hash__(self):
        return hash(self.rev)

    def toplevel_objects(self):
        return [ObjectProxy(self.rev, ob)
                for ob in xorn.storage.get_objects_attached_to(self.rev, None)]

    def all_objects(self):
        return [ObjectProxy(self.rev, ob) for ob in self.rev.get_objects()]

    def selected_objects(self, sel):
        return [ObjectProxy(self.rev, ob)
                for ob in xorn.storage.get_selected_objects(self.rev, sel)]

    def add_object(self, data):
        return ObjectProxy(self.rev, self.rev.add_object(data))

    def copy_object(self, ob):
        return ObjectProxy(self.rev, self.rev.copy_object(ob.rev, ob.ob))

    def copy_objects(self, rev, sel):
        if isinstance(rev, RevisionProxy):
            rev = rev.rev
        return self.rev.copy_objects(rev, sel)

    def delete_object(self, ob):
        if isinstance(ob, ObjectProxy):
            ob = ob.ob
        self.rev.delete_object(ob)

    def delete_objects(self, sel):
        self.rev.delete_objects(sel)

    def is_transient(self):
        return self.rev.is_transient()

    def finalize(self):
        self.rev.finalize()

## \brief High-level object proxy class.

class ObjectProxy:
    def __init__(self, rev, ob):
        if not isinstance(rev, xorn.storage.Revision):
            raise ValueError
        if not isinstance(ob, xorn.storage.Object):
            raise ValueError
        self.rev = rev
        self.ob = ob

    def __eq__(self, other):
        return isinstance(other, ObjectProxy) and self.rev == other.rev \
                                              and self.ob == other.ob

    def __ne__(self, other):
        return not isinstance(other, ObjectProxy) or self.rev != other.rev \
                                                  or self.ob != other.ob

    def __hash__(self):
        return hash((self.rev, self.ob))

    def exists(self):
        return self.rev.object_exists(self.ob)

    def data(self):
        return self.rev.get_object_data(self.ob)

    def set_data(self, data):
        self.rev.set_object_data(self.ob, data)

    def location(self):
        attached_to, pos = self.rev.get_object_location(self.ob)
        if attached_to is None:
            return None, pos
        return ObjectProxy(self.rev, attached_to), pos

    def attached_objects(self):
        return [ObjectProxy(self.rev, ob) for ob in
                    xorn.storage.get_objects_attached_to(self.rev, self.ob)]

    def relocate(self, attach_to, insert_before):
        if isinstance(attach_to, ObjectProxy):
            attach_to = attach_to.ob
        if isinstance(insert_before, ObjectProxy):
            insert_before = insert_before.ob
        self.rev.relocate_object(self.ob, attach_to, insert_before)

    def is_selected(self, sel):
        return xorn.storage.object_is_selected(self.rev, sel, self.ob)

    def __getattr__(self, name):
        if name.startswith('_'):
            raise AttributeError
        if 'rev' not in self.__dict__ or 'ob' not in self.__dict__:
            raise AttributeError
        if name in ['line', 'fill']:
            return AttributeProxy(self.rev, self.ob, name)
        return self.data().__getattribute__(name)

    def __setattr__(self, name, value):
        if 'rev' not in self.__dict__ or 'ob' not in self.__dict__:
            self.__dict__[name] = value
            return
        data = self.data()
        try:
            data.__setattr__(name, value)
        except AttributeError:
            self.__dict__[name] = value
        else:
            self.rev.set_object_data(self.ob, data)

## \brief High-level line/fill attribute proxy class.

class AttributeProxy:
    def __init__(self, rev, ob, name):
        if not isinstance(rev, xorn.storage.Revision):
            raise ValueError
        if not isinstance(ob, xorn.storage.Object):
            raise ValueError
        if not isinstance(name, str):
            raise ValueError
        self.rev = rev
        self.ob = ob
        self.name = name

    def exists(self):
        try:
            ob_data = self.rev.get_object_data(self.ob)
        except KeyError:
            return False
        try:
            ob_data.__getattribute__(self.name)
        except AttributeError:
            return False
        return True

    def data(self):
        return self.rev.get_object_data(self.ob).__getattribute__(self.name)

    def __getattr__(self, name):
        if name.startswith('_'):
            raise AttributeError
        if 'rev' not in self.__dict__ or 'ob' not in self.__dict__ \
                or 'name' not in self.__dict__:
            raise AttributeError
        return self.data().__getattribute__(name)

    def __setattr__(self, name, value):
        if 'rev' not in self.__dict__ or 'ob' not in self.__dict__ \
                or 'name' not in self.__dict__:
            self.__dict__[name] = value
            return
        data0 = self.rev.get_object_data(self.ob)
        data1 = data0.__getattribute__(self.name)
        try:
            data1.__setattr__(name, value)
        except AttributeError:
            self.__dict__[name] = value
        else:
            self.rev.set_object_data(self.ob, data0)
