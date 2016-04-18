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

import tempfile
import xorn.guile

def throws(fun, *args, **kwds):
    try:
        fun(*args, **kwds)
    except Exception as e:
        return type(e)

# converting objects to and from Guile

assert throws(xorn.guile.define, 'foo', object()) == xorn.guile.GuileError
assert throws(xorn.guile.define, 'foo', 0) == None

assert throws(xorn.guile.lookup, 'foo') == None
assert throws(xorn.guile.lookup, 'bar') == xorn.guile.GuileError

assert throws(xorn.guile.eval_string, '(') == xorn.guile.GuileError
assert throws(xorn.guile.eval_string, 'foo') == None
assert throws(xorn.guile.eval_string, 'bar') == xorn.guile.GuileError

for value in [None, False, True, 1, 1., 'foo', u'fo\xf6', (), (0, 1, 'foo')]:
    xorn.guile.define('foo', value)

    for fn in [xorn.guile.lookup, xorn.guile.eval_string]:
        returned = fn('foo')
        assert returned == value
        if isinstance(value, str):
            assert type(returned) == unicode
        else:
            assert type(returned) == type(value)

assert throws(xorn.guile.eval_string, "'(0 . ())") == None
assert throws(xorn.guile.eval_string, "'(0 . 1)") == xorn.guile.GuileError

# calling Guile functions from Python

assert throws(xorn.guile.Procedure) == TypeError
p = xorn.guile.lookup('-')
assert type(p) == xorn.guile.Procedure
assert repr(p) == '<Guile procedure - (#:optional _ _ . _)>'
assert p(13, 8) == 5
assert throws(p) == xorn.guile.GuileError
assert throws(p, foo = None) == ValueError
assert p != xorn.guile.lookup('+')
assert p == xorn.guile.lookup('-')
assert p == xorn.guile.eval_string('-')

xorn.guile.define('foo', p)
assert xorn.guile.eval_string('(simple-format #f "~S" foo)') \
    == '#<procedure - (#:optional _ _ . _)>'

# calling Python functions from Guile, argument count

state = u'foo'
def update_state(value):
    global state
    result = state
    state = value
    return result
xorn.guile.define('update-state!', update_state)

returned = xorn.guile.eval_string('(update-state! "bar")')
assert returned == u'foo'
assert type(returned) == unicode
returned = xorn.guile.eval_string('(update-state! "bar")')
assert returned == u'bar'
assert type(returned) == unicode

assert throws(
    xorn.guile.eval_string, '(update-state!)') == xorn.guile.GuileError
assert throws(
    xorn.guile.eval_string, '(update-state! 0)') == None
assert throws(
    xorn.guile.eval_string, '(update-state! 0 1)') == xorn.guile.GuileError

assert xorn.guile.lookup('update-state!') == update_state

# passing Guile exceptions to Python

def assert_zero(value):
    assert value == 0
xorn.guile.define('assert-zero', assert_zero)

assert throws(
    xorn.guile.eval_string, '(assert-zero 0)') == None
assert throws(
    xorn.guile.eval_string, '(assert-zero 1)') == xorn.guile.GuileError

# passing Python exceptions to Guile

def raise_exception():
    raise Exception
xorn.guile.define('raise-exception', raise_exception)

assert xorn.guile.eval_string('''
  (catch 'python-exception
         raise-exception
         (lambda (key . args)
           (cons (symbol->string key) args)))
''') == ('python-exception', 'exceptions.Exception', False)

# running Guile script files

f = tempfile.NamedTemporaryFile(dir = '.', suffix = '.scm')
f.write('''\
(set! foo (1+ foo))
(* foo 2)
''')
f.flush()
xorn.guile.define('foo', 0)
assert xorn.guile.eval_string('foo') == 0
assert xorn.guile.load(str(f.name)) == 2
assert xorn.guile.eval_string('foo') == 1
assert xorn.guile.load(unicode(f.name)) == 4
assert xorn.guile.eval_string('foo') == 2
f.close()
