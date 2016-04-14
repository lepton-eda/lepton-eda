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

## \file xorn/guile.py
## Placeholder file for xorn.guile documentation.
#
# This file DOES NOT contain the actual source code of the xorn.guile
# module.  It contains documented stubs of the code from which the
# Doxygen documentation is generated.  For the actual definition of
# the module, see the Python extension in \c src/cpython/guile/.

## \namespace xorn.guile
## Embedding a Guile interpreter.
#
# This module allows embedding a Guile interpreter into a Python
# application.  It translates Python objects transparently into Guile
# objects and vice versa.  In order to make a Python function
# available to Guile code, just bind it to a variable name:
#
# \snippet guile.py guile

## Raised on Guile-related errors.

class GuileError(Exception):
    pass

## Guile procedure.
#
# This type can't be directly instantiated.

class Procedure(object):
    ## x.__call__(...) <==> x(...)
    def __call__(...):
        pass

    ## x.__repr__() <==> repr(x)
    def __repr__(...):
        pass

## Return the variable bound to a symbol.
#
# Signals an error if there is no such binding or the symbol is not
# bound to a variable.

def lookup(name):
    pass

## Create a top level variable.
#
# If the named variable already exists, just changes its value.
#
# \throws GuileError if \a value can't be represented as a Guile
#                    object

def define(name, value):
    pass

## Load a file and evaluate its contents in the top-level environment.
#
# \a name must either be a full pathname or be a pathname relative to
# the current directory.  If the Guile variable \c %%load-hook is
# defined, the procedure to which it is bound will be called before
# any code is loaded.
#
# \sa [Guile documentation for %%load-hook](https://www.gnu.org/
#software/guile/manual/html_node/Loading.html#index-_0025load_002dhook)

def load(name):
    pass

## Parse a string as Scheme and evaluate the expressions it contains,
## in order, returning the last expression.

def eval_string(string):
    pass
