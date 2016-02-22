# xorn.geda.netlist - gEDA Netlist Extraction and Generation
# Copyright (C) 1998-2010 Ales Hvezda
# Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
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

## \namespace xorn.geda.netlist.guile
## Scheme API for retrieving netlist information.
#
# This module mirrors the gnetlist Scheme API which exposes the
# results of a netlister run to a netlist backend written in Scheme.
# For each gnetlist API function, this module contains an equivalent
# Python function and makes it available by creating a top-level
# Scheme binding.
#
# The gnetlist API included a function \c gnetlist:get-renamed-nets
# which returned a list of "renamed nets" and could be used to unit
# test the internals of the netlister.  Due to the way nets are
# handled, the concept of a "renamed net" isn't applicable and this
# function isn't supported any more.
#
# Some functions take a dummy argument "level" which must be a string
# but is otherwise ignored.

import sys
from gettext import gettext as _
import xorn.guile
import xorn.geda.attrib

## Global netlist object.
# Set this to the netlist to be inspected.
the_netlist = None

## Global backend argument list.
# Set this to the actual list of arguments.
the_backend_arguments = None

## Global backend verbosity flag.
# Set this to \c 1 if a \c `-v' option has been passed, to \c -1 if a
# \c `-q' option has been passed, to \c 0 otherwise.
the_verbosity = None

## Whether to return net names appropriate for SPICE backends.
# Set this to \c True or \c False depending on whether the backend
# expects the API functions to return SPICE net names.
# \see spicified_net_name
the_spice_mode = None

## Helper function for checking the type of an API function argument.
#
# When constructing an error message, \a fun and \a i are used to
# indicate the API function and argument index, respectively.
#
# \throws TypeError if \a arg is not an instance of \a t or of a
#                   subclass thereof

def check_argument_type(fun, i, arg, t):
    if not isinstance(arg, t):
        raise TypeError, '"%s" argument %d must be %s, not %s' % (
            fun, i, t.__name__, arg.__class__.__name__)

## Return the net name visible through the API.
#
# If the name of the invoked backend starts with \c "spice", \c
# gnetlist omits the \c "unnamed_net" net name prefix.  Here, however,
# the generated netlist is independent of the backend, so the API
# needs to fix up the net name before passing it to the backend.
#
# If \ref the_spice_mode isn't true when evaluated in a boolean
# context, this function just returns \a net.name.

def spicified_net_name(net):
    if the_spice_mode and net.unnamed_counter is not None:
        return str(net.unnamed_counter)
    else:
        return net.name

# ============================ netlist functions =============================

## Return a list of all package refdes's in the design.
#
# For historical reasons, this function returns its results in
# reversed order.

def get_packages(level):
    check_argument_type('gnetlist:get-packages', 1, level, basestring)
    return [package.refdes for package in reversed(the_netlist.packages)]

## Return a list of all package refdes's in the design; might return
## duplicates.
#
# \note Don't use this function.  Use \ref get_packages instead.
#
# Walks through the list of components and returns a list of each
# component's refdes.  Power symbols don't have a refdes and are
# skipped.
#
# For historical reasons, this function returns its results in
# reversed order.

def get_non_unique_packages(level):
    check_argument_type('gnetlist:get-non-unique-packages', 1,
                        level, basestring)
    return [component.refdes
            for component in reversed(the_netlist.components)
            # filter out power symbols
            if component.refdes is not None]

## Return a list of pin numbers for a given package.
#
# If \a refdes doesn't refer to an existing package, an empty list is
# returned.
#
# For historical reasons, this function returns its results in
# reversed order.

def get_pins(refdes):
    check_argument_type('gnetlist:get-pins', 1, refdes, basestring)
    try:
        package = the_netlist.packages_by_refdes[refdes]
    except KeyError:
        return []

    return [pin.number for pin in reversed(package.pins)]

## Return a list of all net names in the design; might return duplicates.
#
# \note Don't use this function.  Use \ref get_all_unique_nets instead.
#
# Walks through the list of components (skipping power symbols) and
# through the list of individual pins on each component (skipping
# unconnected pins), and returns a list of the names of all found
# nets.
#
# Due to the way hierarchy is handled in the new netlisting code,
# subsheet symbols and ports aren't part of the finished netlist.
# Therefore, when using hierarchical schematics, this function doesn't
# return some of the net name duplications the old function did.
#
# For historical reasons, this function returns its results in
# reversed order.

def get_all_nets(level):
    check_argument_type('gnetlist:get-all-nets', 1, level, basestring)

    # g_get_all_nets doesn't seem to filter out power symbols, but for
    # a flat hierarchy, the corresponding net names are omitted from
    # the returned list.  Strange...

    return [spicified_net_name(cpin.local_net.net)
            for component in reversed(the_netlist.components)
            for cpin in reversed(component.cpins)
            # filter out power symbols
            if component.refdes is not None
            # filter out stub nets for unconnected pins
            and not cpin.local_net.net.is_unconnected_pin]

## Return a list of all net names in the design.
#
# Stub nets for unconnected pins are not included.
#
# For historical reasons, this function returns its results in
# reversed order.

def get_all_unique_nets(level):
    check_argument_type('gnetlist:get-all-unique-nets', 1, level, basestring)
    return [spicified_net_name(net) for net in reversed(the_netlist.nets)]

## Return a list of pins connected to the a given net.
#
# For each pin, a pair <tt>(refdes, pinnumber)</tt> is returned.
#
# If \a wanted_net_name doesn't refer to an existing net, an empty
# list is returned.
#
# For historical reasons, this function returns its results in
# reversed order.

def get_all_connections(wanted_net_name):
    check_argument_type('gnetlist:get-all-connections', 1,
                        wanted_net_name, basestring)
    try:
        net = the_netlist.nets_by_name[wanted_net_name]
    except KeyError:
        return []

    return [(pin.package.refdes, pin.number)
            for pin in reversed(net.connections)]

## Return the net to which a given pin is connected and a list of pins
## graphically connected to it.
#
# The result is a pair
#   <tt>(netname, [(refdes, pin), (refdes, pin), ...]</tt>).
#
# If \a wanted_refdes and \a wanted_pin don't refer to an existing
# pin, the pair <tt>('ERROR_INVALID_PIN', None)</tt> is returned.
#
# \note This function is different to all other API functions in that
#       it does not consider the actual logical net but only pins
#       which are graphically connected to the given pin.  Unless you
#       are analyzing the graphical properties of a netlist, you
#       should only use the netname part of its return value.
#
# Reflecting the behavior of \c gnetlist:get-nets, this function
# doesn't return the pin itself if it isn't connected to anything.
#
# For historical reasons, this function returns the list in
# reversed order.

def get_nets(wanted_refdes, wanted_pin):
    check_argument_type('gnetlist:get-nets', 1, wanted_refdes, basestring)
    check_argument_type('gnetlist:get-nets', 2, wanted_pin, basestring)

    try:
        package = the_netlist.packages_by_refdes[wanted_refdes]
        pin = package.pins_by_number[wanted_pin]
    except KeyError:
        sys.stderr.write(_("Invalid refdes ('%s') and pin ('%s') passed "
                           "to get_nets\n") % (wanted_refdes, wanted_pin))
        return 'ERROR_INVALID_PIN', None

    if pin.net.is_unconnected_pin:
        return spicified_net_name(pin.net), []

    # The behavior of this gnetlist API function is ***BUGGED***!
    # It lists only the pins graphically connected to the given pin
    # (the "local net" in xorn.netlist terminology).
    #
    # To get the correct behavior, uncomment the following lines:
    # return pin.net.name, [(pin_.package.refdes, pin_.number)
    #                       for pin_ in pin.net.connections]

    return spicified_net_name(pin.net), [
        (cpin_.component.refdes, cpin_.blueprint.number)
        for cpin in pin.cpins
        for cpin_ in cpin.local_net.cpins
        # filter out power pins
        if cpin_.component.refdes is not None]

## Return a list of pairs <tt>(pinnumber, netname)</tt> for a given
## package.
#
## Each pair contains the name of a pin of that package and the name
## of the net connected to that pin.
#
# If \a refdes doesn't refer to an existing package, an empty list is
# returned.
#
# This function *does not* return its results in reversed order.

def get_pins_nets(wanted_refdes):
    check_argument_type('gnetlist:get-pins-nets', 1, wanted_refdes, basestring)
    try:
        package = the_netlist.packages_by_refdes[wanted_refdes]
    except KeyError:
        return []

    return [(pin.number, spicified_net_name(pin.net)) for pin in package.pins]

# ----------------------------------------------------------------------------

## Get attribute value(s) from a package with given refdes.
#
# This function returns the values of a specific attribute type
# attached to the symbol instances with the given refdes.
#
# Every first attribute value found is added to the return list.
# \c False is added if the instance has no such attribute.
#
# \note The order of the values in the return list is the order of
#       symbol instances within gnetlist (the first element is the
#       value associated with the first symbol instance).
#
# This function *does not* return its results in reversed order.
#
# \param [in] refdes         package reference
# \param [in] wanted_attrib  attribute name
#
# \returns a list of attribute values as strings and \c False

def get_all_package_attributes(refdes, wanted_attrib):
    check_argument_type('gnetlist:get-all-package-attributes', 1,
                        refdes, basestring)
    check_argument_type('gnetlist:get-all-package-attributes', 2,
                        wanted_attrib, basestring)
    try:
        package = the_netlist.packages_by_refdes[refdes]
    except KeyError:
        return []

    # search for refdes instances and through the entire list
    l = []
    for component in package.components:
        try:
            value = component.blueprint.get_attribute(wanted_attrib)
        except KeyError:
            l.append(False)
        else:
            l.append(value)
    return l

## Return value of attribute, otherwise string \c "not found".

def get_toplevel_attribute(wanted_attrib):
    check_argument_type('gnetlist:get-toplevel-attribute', 1,
                        wanted_attrib, basestring)
    return the_netlist.get_toplevel_attribute(wanted_attrib, 'not found')

## Take a refdes and pinseq number and return wanted_attribute
## associated with that pinseq pin and component.

def get_attribute_by_pinseq(refdes, pinseq, wanted_attrib):
    check_argument_type('gnetlist:get-attribute-by-pinseq', 1,
                        refdes, basestring)
    check_argument_type('gnetlist:get-attribute-by-pinseq', 2,
                        pinseq, basestring)
    check_argument_type('gnetlist:get-attribute-by-pinseq', 3,
                        wanted_attrib, basestring)

    try:
        pinseq = int(pinseq)
    except ValueError:
        return 'unknown'

    try:
        package = the_netlist.packages_by_refdes[refdes]
        pin = package.get_pin_by_pinseq(pinseq)
        return pin.get_attribute(wanted_attrib)
    except KeyError:
        return 'unknown'

## Take a pin number and return the appropriate attribute on that pin.
#
# \a pin is the value associated with the pinnumber= attribute and refdes.

def get_attribute_by_pinnumber(refdes, pin, wanted_attrib):
    check_argument_type('gnetlist:get-attribute-by-pinnumber', 1,
                        refdes, basestring)
    check_argument_type('gnetlist:get-attribute-by-pinnumber', 2,
                        pin, basestring)
    check_argument_type('gnetlist:get-attribute-by-pinnumber', 3,
                        wanted_attrib, basestring)

    try:
        package = the_netlist.packages_by_refdes[refdes]
        pin = package.pins_by_number[pin]
        return pin.get_attribute(wanted_attrib)
    except KeyError:
        return 'unknown'

# For historical reasons, this function returns its results in
# reversed order.

def vams_get_package_attributes(refdes):
    check_argument_type('gnetlist:vams-get-package-attributes', 1,
                        refdes, basestring)

    try:
        package = the_netlist.packages_by_refdes[refdes]
    except KeyError:
        return []
    else:
        # search outside the symbol (attached attributes only)
        return list(reversed(package.get_attribute_names(False)))

# ----------------------------------------------------------------------------

## Given a net name, an attribute, and a wanted attribute, return all
## the given attribute of all the graphical objects connected to that net.
#
# For historical reasons, this function returns its results in
# reversed order.

def graphical_objs_in_net_with_attrib_get_attrib(
        wanted_net_name, has_attrib, wanted_attrib):
    check_argument_type(
        'gnetlist:graphical-objs-in-net-with-attrib-get-attrib', 1,
        wanted_net_name, basestring)
    check_argument_type(
        'gnetlist:graphical-objs-in-net-with-attrib-get-attrib', 2,
        has_attrib, basestring)
    check_argument_type(
        'gnetlist:graphical-objs-in-net-with-attrib-get-attrib', 3,
        wanted_attrib, basestring)

    try:
        has_attrib_name, has_attrib_value = \
            xorn.geda.attrib.parse_string(has_attrib)
    except xorn.geda.attrib.MalformedAttributeError:
        return []

    try:
        net = the_netlist.nets_by_name[wanted_net_name]
    except KeyError:
        return []

    return list(reversed(net.graphical_objs_with_attrib_get_attrib(
        has_attrib_name, has_attrib_value, wanted_attrib)))

# ============================= SDB -- 9.1.2003 ==============================

## Obtain a list of `-O' backend arguments.
#
# Returns a list of arguments passed to the gnetlist backend via the
# \c `-O' gnetlist command-line option.

def get_backend_arguments():
    return the_backend_arguments

## Get input files from command line.
#
# This function returns a list of the files named on the command line.
#
# \returns a list of filenames as strings

def get_input_files():
    return [sheet.blueprint.filename
            for sheet in the_netlist.toplevel_sheets]

## Indicate the verbosity level for messages.
#
# If the \c "-q" gnetlist command-line option was specified, returns \c -1.
# If the \c "-v" gnetlist command-line option was specified, returns \c 1.
# Otherwise, returns \c 0.

def get_verbosity():
    return the_verbosity

# ============================================================================

xorn.guile.define('quit', lambda: sys.exit(0))
xorn.guile.define('exit', lambda: sys.exit(0))

for name, value in {
        # netlist functions
        'get-packages': get_packages,
        'get-non-unique-packages': get_non_unique_packages,
        'get-pins': get_pins,
        'get-all-nets': get_all_nets,
        'get-all-unique-nets': get_all_unique_nets,
        'get-all-connections': get_all_connections,
        'get-nets-internal': get_nets,
        'get-pins-nets-internal': get_pins_nets,

        'get-all-package-attributes': get_all_package_attributes,
        'get-toplevel-attribute': get_toplevel_attribute,
        'get-attribute-by-pinseq': get_attribute_by_pinseq,
        'get-attribute-by-pinnumber': get_attribute_by_pinnumber,
        'vams-get-package-attributes': vams_get_package_attributes,

        'graphical-objs-in-net-with-attrib-get-attrib':
            graphical_objs_in_net_with_attrib_get_attrib,

        # SDB -- 9.1.2003
        'get-backend-arguments': get_backend_arguments,
        'get-input-files': get_input_files,
        'get-verbosity': get_verbosity
    }.iteritems():
    xorn.guile.define('gnetlist:' + name, value)

xorn.guile.eval_string('''
(define (gnetlist:get-pins-nets refdes)
  (map (lambda (x) (cons (car x) (cadr x)))
       (gnetlist:get-pins-nets-internal refdes)))

(define (gnetlist:get-nets refdes pin)
  (let ((x (gnetlist:get-nets-internal refdes pin)))
    (cons (car x) (cadr x))))
''')
