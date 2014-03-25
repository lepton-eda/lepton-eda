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

import xorn.storage

_ = None

def check(rev, ob, attach_to, insert_before, result, objects):
    try:
        rev.relocate_object(ob, attach_to, insert_before)
    except Exception as e:
        assert type(e) == result
    else:
        assert result is None

    assert rev.get_objects() == objects

def common_checks(rev, N, a, b, ob0, ob1, ob2):
    # can't relocate None

    check(rev, _, _, _, TypeError, [ob0, ob1, ob2])
    check(rev, _, _, N, TypeError, [ob0, ob1, ob2])
    check(rev, _, _, a, TypeError, [ob0, ob1, ob2])
    check(rev, _, _, b, TypeError, [ob0, ob1, ob2])

    check(rev, _, N, _, TypeError, [ob0, ob1, ob2])
    check(rev, _, N, N, TypeError, [ob0, ob1, ob2])
    check(rev, _, N, a, TypeError, [ob0, ob1, ob2])
    check(rev, _, N, b, TypeError, [ob0, ob1, ob2])

    check(rev, _, a, _, TypeError, [ob0, ob1, ob2])
    check(rev, _, a, N, TypeError, [ob0, ob1, ob2])
    check(rev, _, a, a, TypeError, [ob0, ob1, ob2])
    check(rev, _, a, b, TypeError, [ob0, ob1, ob2])

    check(rev, _, b, _, TypeError, [ob0, ob1, ob2])
    check(rev, _, b, N, TypeError, [ob0, ob1, ob2])
    check(rev, _, b, a, TypeError, [ob0, ob1, ob2])
    check(rev, _, b, b, TypeError, [ob0, ob1, ob2])

    # can't embed N to itself

    check(rev, N, N, _, ValueError, [ob0, ob1, ob2])
    check(rev, N, N, N, ValueError, [ob0, ob1, ob2])
    check(rev, N, N, a, ValueError, [ob0, ob1, ob2])
    check(rev, N, N, b, ValueError, [ob0, ob1, ob2])

    # can't embed N to a

    check(rev, N, a, _, ValueError, [ob0, ob1, ob2])
    check(rev, N, a, N, ValueError, [ob0, ob1, ob2])
    check(rev, N, a, a, ValueError, [ob0, ob1, ob2])
    check(rev, N, a, b, ValueError, [ob0, ob1, ob2])

    # can't embed N to b

    check(rev, N, b, _, ValueError, [ob0, ob1, ob2])
    check(rev, N, b, N, ValueError, [ob0, ob1, ob2])
    check(rev, N, b, a, ValueError, [ob0, ob1, ob2])
    check(rev, N, b, b, ValueError, [ob0, ob1, ob2])

    # can't embed a to itself

    check(rev, a, a, _, ValueError, [ob0, ob1, ob2])
    check(rev, a, a, N, ValueError, [ob0, ob1, ob2])
    check(rev, a, a, a, ValueError, [ob0, ob1, ob2])
    check(rev, a, a, b, ValueError, [ob0, ob1, ob2])

    # can't embed a to b

    check(rev, a, b, _, ValueError, [ob0, ob1, ob2])
    check(rev, a, b, N, ValueError, [ob0, ob1, ob2])
    check(rev, a, b, a, ValueError, [ob0, ob1, ob2])
    check(rev, a, b, b, ValueError, [ob0, ob1, ob2])

    # can't embed b to a

    check(rev, b, a, _, ValueError, [ob0, ob1, ob2])
    check(rev, b, a, N, ValueError, [ob0, ob1, ob2])
    check(rev, b, a, a, ValueError, [ob0, ob1, ob2])
    check(rev, b, a, b, ValueError, [ob0, ob1, ob2])

    # can't embed b to itself

    check(rev, b, b, _, ValueError, [ob0, ob1, ob2])
    check(rev, b, b, N, ValueError, [ob0, ob1, ob2])
    check(rev, b, b, a, ValueError, [ob0, ob1, ob2])
    check(rev, b, b, b, ValueError, [ob0, ob1, ob2])

    # can't embed something to N before N

    check(rev, a, N, N, ValueError, [ob0, ob1, ob2])
    check(rev, b, N, N, ValueError, [ob0, ob1, ob2])

def check_delete(rev, ob, remaining):
    r = xorn.storage.Revision(rev)
    assert r is not None
    r.delete_object(ob)
    assert r.get_objects() == remaining

def check0(rev, N, a, b):
    # N() a b
    common_checks(rev, N, a, b, N, a, b)

    check(rev, N, _, _, None,       [a, b, N])
    check(rev, N, _, b, None,       [a, N, b])
    check(rev, N, _, a, None,       [N, a, b])

    check(rev, N, _, N, None,       [N, a, b])
    check(rev, a, _, a, None,       [N, a, b])
    check(rev, a, N, a, ValueError, [N, a, b])
    check(rev, b, _, b, None,       [N, a, b])
    check(rev, b, N, b, ValueError, [N, a, b])

    check_delete(rev, N, [a, b])

def check1(rev, N, a, b):
    # a N() b
    common_checks(rev, N, a, b, a, N, b)

    check(rev, N, _, _, None,       [a, b, N])
    check(rev, N, _, a, None,       [N, a, b])
    check(rev, N, _, b, None,       [a, N, b])

    check(rev, N, _, N, None,       [a, N, b])
    check(rev, a, _, a, None,       [a, N, b])
    check(rev, a, N, a, ValueError, [a, N, b])
    check(rev, b, _, b, None,       [a, N, b])
    check(rev, b, N, b, ValueError, [a, N, b])

    check_delete(rev, N, [a, b])

def check2(rev, N, a, b):
    # a b N()
    common_checks(rev, N, a, b, a, b, N)

    check(rev, N, _, b, None,       [a, N, b])
    check(rev, N, _, a, None,       [N, a, b])
    check(rev, N, _, _, None,       [a, b, N])

    check(rev, N, _, N, None,       [a, b, N])
    check(rev, a, _, a, None,       [a, b, N])
    check(rev, a, N, a, ValueError, [a, b, N])
    check(rev, b, _, b, None,       [a, b, N])
    check(rev, b, N, b, ValueError, [a, b, N])

    check_delete(rev, N, [a, b])

def check3(rev, N, a, b):
    # N(a) b
    common_checks(rev, N, a, b, N, a, b)

    check(rev, N, _, _, None,       [b, N, a])
    check(rev, N, _, a, ValueError, [b, N, a])
    check(rev, N, _, b, None,       [N, a, b])

    check(rev, N, _, N, None,       [N, a, b])
    check(rev, a, _, a, ValueError, [N, a, b])
    check(rev, a, N, a, None,       [N, a, b])
    check(rev, b, _, b, None,       [N, a, b])
    check(rev, b, N, b, ValueError, [N, a, b])

    check_delete(rev, N, [b])

def check4(rev, N, a, b):
    # a N(b)
    common_checks(rev, N, a, b, a, N, b)

    check(rev, N, _, a, None,       [N, b, a])
    check(rev, N, _, b, ValueError, [N, b, a])
    check(rev, N, _, _, None,       [a, N, b])

    check(rev, N, _, N, None,       [a, N, b])
    check(rev, a, _, a, None,       [a, N, b])
    check(rev, a, N, a, ValueError, [a, N, b])
    check(rev, b, _, b, ValueError, [a, N, b])
    check(rev, b, N, b, None,       [a, N, b])

    check_delete(rev, N, [a])

def check5(rev, N, a, b):
    # N(a b)
    common_checks(rev, N, a, b, N, a, b)

    check(rev, N, _, _, None,       [N, a, b])
    check(rev, N, _, a, ValueError, [N, a, b])
    check(rev, N, _, b, ValueError, [N, a, b])

    check(rev, N, _, N, None,       [N, a, b])
    check(rev, a, _, a, ValueError, [N, a, b])
    check(rev, a, N, a, None,       [N, a, b])
    check(rev, b, _, b, ValueError, [N, a, b])
    check(rev, b, N, b, None,       [N, a, b])

    check_delete(rev, N, [])

def do_it(rev, ob, attach_to, insert_before, result, objects, fun, fN, fa, fb):
    check(rev, ob, attach_to, insert_before, result, objects)
    fun(rev, fN, fa, fb)

    check(rev, ob, attach_to, insert_before, result, objects)
    fun(rev, fN, fa, fb)


rev = xorn.storage.Revision()
assert rev is not None

N = rev.add_object(xorn.storage.Net())
a = rev.add_object(xorn.storage.Text())
b = rev.add_object(xorn.storage.Text())

assert N is not None
assert a is not None
assert b is not None

common_checks(rev, N, a, b, N, a, b)

# can move objects

do_it(rev, N, _, _, None,       [a, b, N], check2, N, a, b)
do_it(rev, N, _, a, None,       [N, a, b], check0, N, a, b)
do_it(rev, N, _, b, None,       [a, N, b], check1, N, a, b)

do_it(rev, a, _, _, None,       [N, b, a], check0, N, b, a)
do_it(rev, a, _, N, None,       [a, N, b], check1, N, a, b)
do_it(rev, a, _, b, None,       [N, a, b], check0, N, a, b)

do_it(rev, b, _, N, None,       [b, N, a], check1, N, b, a)
do_it(rev, b, _, a, None,       [N, b, a], check0, N, b, a)
do_it(rev, b, _, _, None,       [N, a, b], check0, N, a, b)

# can embed a to N, but not before b

do_it(rev, a, N, _, None,       [N, a, b], check3, N, a, b)
do_it(rev, a, N, b, ValueError, [N, a, b], check3, N, a, b)

do_it(rev, b, _, N, None,       [b, N, a], check4, N, b, a)
do_it(rev, b, _, a, ValueError, [b, N, a], check4, N, b, a)
do_it(rev, b, _, _, None,       [N, a, b], check3, N, a, b)

do_it(rev, a, _, b, None,       [N, a, b], check0, N, a, b)

# can embed b to N, but not before a

do_it(rev, b, N, _, None,       [N, b, a], check3, N, b, a)
do_it(rev, b, N, a, ValueError, [N, b, a], check3, N, b, a)

do_it(rev, a, _, N, None,       [a, N, b], check4, N, a, b)
do_it(rev, a, _, b, ValueError, [a, N, b], check4, N, a, b)
do_it(rev, a, _, _, None,       [N, b, a], check3, N, b, a)

do_it(rev, b, _, _, None,       [N, a, b], check0, N, a, b)

# can embed both

do_it(rev, a, N, _, None,       [N, a, b], check3, N, a, b)
do_it(rev, b, N, _, None,       [N, a, b], check5, N, a, b)

do_it(rev, a, N, _, None,       [N, b, a], check5, N, b, a)
do_it(rev, a, N, b, None,       [N, a, b], check5, N, a, b)
do_it(rev, b, N, a, None,       [N, b, a], check5, N, b, a)
do_it(rev, b, N, _, None,       [N, a, b], check5, N, a, b)

do_it(rev, a, _, _, None,       [N, b, a], check3, N, b, a)
do_it(rev, b, _, _, None,       [N, a, b], check0, N, a, b)

rev.finalize()

common_checks(rev, N, a, b, N, a, b)

check(rev, N, _, _, ValueError, [N, a, b])
check(rev, N, _, a, ValueError, [N, a, b])
check(rev, N, _, b, ValueError, [N, a, b])

check(rev, a, _, _, ValueError, [N, a, b])
check(rev, a, _, N, ValueError, [N, a, b])
check(rev, a, _, b, ValueError, [N, a, b])
check(rev, a, N, _, ValueError, [N, a, b])

check(rev, b, _, _, ValueError, [N, a, b])
check(rev, b, _, N, ValueError, [N, a, b])
check(rev, b, _, a, ValueError, [N, a, b])
check(rev, b, N, _, ValueError, [N, a, b])

rev1 = xorn.storage.Revision(rev)
assert rev1 is not None

# can't attach text to line

rev1.set_object_data(N, xorn.storage.Line())
do_it(rev1, a, N, _, ValueError, [N, a, b], check0, N, a, b)

# can attach text to component

rev1.set_object_data(N, xorn.storage.Component())
do_it(rev1, a, N, _, None, [N, a, b], check3, N, a, b)
