# xorn.geda.netlist - gEDA Netlist Extraction and Generation
# Copyright (C) 1998-2010 Ales Hvezda
# Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
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

# Copyright (C) 2011 Peter Brett <peter@peter-b.co.uk>

## \file util_getopt.py
## gnetlist `-O' command-line option parsing for backends.
#
# This module provides the function \ref backend_getopt to assist
# backends which wish to provide command-line gnetlist options via the
# `-O' argument.  `backend_getopt' accepts a grammar and the set of
# `-O' arguments, and extracts the options.
#
# The name of an `-O' option is assumed to be all characters in the
# argument up to the first `=' or ` '.

import collections, sys

## Raised by \ref backend_getopt if it finds a problem with the arguments.

class OptionError(Exception):
    pass

NO_ARGUMENT, REQUIRED_ARGUMENT, OPTIONAL_ARGUMENT = xrange(3)

Option = collections.namedtuple(
    'Option', ['required', 'argument', 'predicate'])

## Parse command-line `-O' arguments against a given backend option grammar.
#
# The \a grammar argument is expected to be a dictionary of this form:
#
#   { 'option-name': Option(False, NO_ARGUMENT, None), ... }
#
# For each option, the following properties are given:
#
#   required (bool)
#     If \a required is \c True, the option is required.  \ref
#     backend_getopt will raise an error if it is not found in the
#     arguments.
#
#   value (int)
#     If \a value is REQUIRED_ARGUMENT, the option requires a value;
#     if it is NO_ARGUMENT, it does not; and if it is
#     OPTIONAL_ARGUMENT, the option may appear in the arguments with
#     or without a value.
#
#   predicate (function)
#     If the option accepts a value (i.e. you specified
#     REQUIRED_ARGUMENT or OPTIONAL_ARGUMENT for this option), then
#     \ref backend_getopt will apply \a predicate to the value and
#     raise an error if it returns \c False.  \a predicate should be a
#     procedure which accepts a string and returns a boolean value.
#
# \returns a dictionary whose keys are the names of the backend
#          options specified by the user and whose values are the
#          option values, or \c True where the option was given but
#          has no value
#
# \throws OptionError if it finds a problem with the arguments

def backend_getopt(args, grammar, raise_exception = False):
    def error(msg):
        if raise_exception:
            raise OptionError, msg
        sys.stderr.write(msg + "\n")
        sys.exit(1)

    options = {}

    # First pass: process options
    for arg in args:
        # Find the first index of a ' ' or '=' in the argument
        try:
            i = next(i for i, ch in enumerate(arg) if ch in ' =')
        except StopIteration:
            # none found
            name, value = arg, None
        else:
            if i == 0:
                error("Invalid backend option syntax '%s'" % arg)
            name, value = arg[:i], arg[i + 1:]

        try:
            spec = grammar[name]
        except KeyError:
            # Is this a valid argument?
            error("Unrecognized backend option '%s'" % name)

        # Validate the given `-O' argument name and value against the grammar.

        # Check that a value was provided, if one was required, or vice versa.
        if spec.argument == NO_ARGUMENT and value is not None:
            error("Backend option '%s' doesn't allow an argument" % name)
        if spec.argument == REQUIRED_ARGUMENT and value is None:
            error("Backend option '%s' requires an argument" % name)

        # If a value-verification predicate was provided, use it to verify
        # the value.
        if spec.predicate is not None and value is not None \
               and not spec.predicate(value):
            error("Invalid argument '%s' to backend option '%s'"
                  % (value, name))

        # If a value was provided, return it, otherwise return True.
        if value is None:
            options[name] = True
        else:
            options[name] = value

    # Second pass: ensure required options have been provided
    for name in grammar:
        if grammar[name].required and not name in options:
            error("Backend option '%s' must be specified" % name)

    return options
