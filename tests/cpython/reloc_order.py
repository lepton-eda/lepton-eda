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

import xorn.storage, Setup

def relocate_and_assert(rev, ob, insert_before, result, objects):
    try:
        rev.relocate_object(ob, None, insert_before)
    except Exception as e:
        assert result == type(e)
    else:
        assert result is None

    assert rev.get_objects() == objects

rev0, rev1, rev2, rev3, ob0, ob1a, ob1b = Setup.setup()

rev4 = xorn.storage.Revision(rev2)

assert rev4.get_objects() == [ob0, ob1a, ob1b]

relocate_and_assert(rev4, ob0,  ob0,        None, [ob0, ob1a, ob1b])
relocate_and_assert(rev4, ob0,  ob1a,       None, [ob0, ob1a, ob1b])
relocate_and_assert(rev4, ob0,  ob1b,       None, [ob1a, ob0, ob1b])
relocate_and_assert(rev4, ob0,  None,       None, [ob1a, ob1b, ob0])
relocate_and_assert(rev4, ob1a, ob0,        None, [ob1b, ob1a, ob0])
relocate_and_assert(rev4, ob1a, ob1a,       None, [ob1b, ob1a, ob0])
relocate_and_assert(rev4, ob1a, ob1b,       None, [ob1a, ob1b, ob0])
relocate_and_assert(rev4, ob1a, None,       None, [ob1b, ob0, ob1a])
relocate_and_assert(rev4, ob1b, ob0,        None, [ob1b, ob0, ob1a])
relocate_and_assert(rev4, ob1b, ob1a,       None, [ob0, ob1b, ob1a])
relocate_and_assert(rev4, ob1b, ob1b,       None, [ob0, ob1b, ob1a])
relocate_and_assert(rev4, ob1b, None,       None, [ob0, ob1a, ob1b])
relocate_and_assert(rev4, None, ob0,   TypeError, [ob0, ob1a, ob1b])
relocate_and_assert(rev4, None, ob1a,  TypeError, [ob0, ob1a, ob1b])
relocate_and_assert(rev4, None, ob1b,  TypeError, [ob0, ob1a, ob1b])
relocate_and_assert(rev4, None, None,  TypeError, [ob0, ob1a, ob1b])

relocate_and_assert(rev4, ob0,  ob0,        None, [ob0, ob1a, ob1b])
relocate_and_assert(rev4, ob1a, ob1a,       None, [ob0, ob1a, ob1b])
relocate_and_assert(rev4, ob1b, ob1b,       None, [ob0, ob1a, ob1b])

rev4.delete_object(ob0)

assert rev4.get_objects() == [ob1a, ob1b]

relocate_and_assert(rev4, ob0,  ob0,    KeyError, [ob1a, ob1b])
relocate_and_assert(rev4, ob0,  ob1a,   KeyError, [ob1a, ob1b])
relocate_and_assert(rev4, ob0,  ob1b,   KeyError, [ob1a, ob1b])
relocate_and_assert(rev4, ob0,  None,   KeyError, [ob1a, ob1b])
relocate_and_assert(rev4, ob1a, ob0,    KeyError, [ob1a, ob1b])
relocate_and_assert(rev4, ob1a, ob1a,       None, [ob1a, ob1b])
relocate_and_assert(rev4, ob1a, ob1b,       None, [ob1a, ob1b])
relocate_and_assert(rev4, ob1a, None,       None, [ob1b, ob1a])
relocate_and_assert(rev4, ob1b, ob0,    KeyError, [ob1b, ob1a])
relocate_and_assert(rev4, ob1b, ob1a,       None, [ob1b, ob1a])
relocate_and_assert(rev4, ob1b, ob1b,       None, [ob1b, ob1a])
relocate_and_assert(rev4, ob1b, None,       None, [ob1a, ob1b])
relocate_and_assert(rev4, None, ob0,   TypeError, [ob1a, ob1b])
relocate_and_assert(rev4, None, ob1a,  TypeError, [ob1a, ob1b])
relocate_and_assert(rev4, None, ob1b,  TypeError, [ob1a, ob1b])
relocate_and_assert(rev4, None, None,  TypeError, [ob1a, ob1b])

rev4.finalize()

relocate_and_assert(rev4, ob1a, ob1a, ValueError, [ob1a, ob1b])
relocate_and_assert(rev4, ob1a, ob1b, ValueError, [ob1a, ob1b])
relocate_and_assert(rev4, ob1a, None, ValueError, [ob1a, ob1b])
relocate_and_assert(rev4, ob1b, ob1a, ValueError, [ob1a, ob1b])
relocate_and_assert(rev4, ob1b, ob1b, ValueError, [ob1a, ob1b])
relocate_and_assert(rev4, ob1b, None, ValueError, [ob1a, ob1b])
