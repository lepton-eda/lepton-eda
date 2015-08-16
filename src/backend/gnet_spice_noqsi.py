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

# gnetlist back end for SPICE
# Copyright (C) 2012, 2013 John P. Doty

import re, sys

# forward and reverse refdes maps
munges = {}
refdes_reserved = set()

# Expand a string in context of a particular package.
#
# Splits string into alternating fragments of whitespace and ink and
# passes each through check_and_expand_field.
#
# This is how we convert symbol data and connections into SPICE cards.

MAYBE_WHITESPACE_REGEXP = re.compile('\s+|\S+')

def expand_string(netlist, package, s):
    fields = []
    pos = 0
    while pos < len(s):
        m = MAYBE_WHITESPACE_REGEXP.match(s, pos)
        fields.append(check_and_expand_field(netlist, package, m.group(0)))
        pos = m.end()
    return ''.join(fields)

# gnetlist associates net with pinnumber, but traditionally SPICE
# backends for gnetlist have keyed on pinseq. This function implements that.

def get_net_by_pinseq(package, n):
    try:
        pin = package.get_pin_by_pinseq(n)
    except KeyError:
        package.error("pinseq=%s not found. "
                      "This may indicate an erroneously duplicated refdes." % n)
        return ''

    return get_net(package, pin.number)

# Get the net attached to a particular pin.

def get_net(package, pin):
    if pin not in package.pins_by_number:
        package.error("pinnumber=%s not found" % pin)
        return ''

    net = package.pins_by_number[pin].net
    if net.unnamed_counter is not None:
        return '%d' % net.unnamed_counter
    return net.name

# Get the munged version of refdes

def get_munged(prefix, refdes):
    try:
        return munges[prefix, refdes]
    except KeyError:
        # Make unique munged version.
        # Append X until we have a unique refdes.
        candidate = prefix + refdes
        while candidate in refdes_reserved:
            candidate += 'X'

        refdes_reserved.add(candidate)
        munges[prefix, refdes] = candidate
        return candidate

# Expand value. Empty string if it doesn't exist, and no default given.
# Deal with the fact that missing attributes may either be "unknown"
# or "not found" :(

def expand_value(package, name, default):
    # Select either package attribute or toplevel attribute
    if package is not None:
        return package.get_attribute(name, default)
    else:
        return netlist.get_toplevel_attribute(name, default)

# Check field for magic, and expand accordingly if necessary.
# We only expand a given field once, on the first magic character discovered.

def check_and_expand_field(netlist, package, field):
    # Find magic character for field expansion.
    try:
        i = next(i for i, c in enumerate(field) if c in '?#=@%')
    except StopIteration:
        return field

    # Split field at the magic character
    left, key, right = field[:i], field[i], field[i + 1:]

    if key == '?':
        # Expand refdes, munging if asked. Note that an empty prefix
        # matches any string, here, so The Right Thing (nothing) happens.
        if package.refdes.lower().startswith(left.lower()):
            return package.refdes + right
        else:
            return get_munged(left, package.refdes) + right
    elif key == '#':
        # Get name of net connected to pin.
        # The only issue is whether "left" specifies an alternate refdes.
        if not left:
            return get_net(package, right)
        else:
            return get_net(netlist.packages_by_refdes[left], right)
    elif key == '=':
        # Expand attribute. Result is name=value.
        # Empty string if it doesn't exist, and no default given.
        value = expand_value(package, left, right)
        if not value:
            return ''
        else:
            return left + '=' + value
    elif key == '@':
        return expand_value(package, left, right)
    elif key == '%':
        # Expand "%" variables.
        # For now, reconstruct input for unrecognized ones.
        # Should be error instead?

        if right == 'pinseq':
            # get all net connections in pinseq order
            # Check for slotting, error if present.
            try:
                numslots = int(package.get_attribute('numslots', 'unknown'))
            except ValueError:
                pass
            else:
                if numslots > 0:
                    package.error("This package uses slotting. "
                                  "You must list its connections by "
                                  "pinnumber, not pinseq.")
                    return left

            return left + ' '.join(get_net_by_pinseq(package, n + 1)
                                   for n in range(len(package.pins)))
        if right == 'io':
            # get nets attached to SPICE io pins
            return left + ' '.join(get_net(package, '1')
                                   for package in spice_io_pins)
        if right == 'up':
            # List input/output symbols in lexical order
            return left + ' '.join(get_net(package, '1')
                                   for package in io_pins)
        if right == 'down':
            # list pins sorted by pinlabel
            pins_by_label = package.pins
            pins_by_label.sort(
                key = lambda pin:
                    (pin.get_attribute('pinlabel', 'unknown'), pin))
            # get all net connections in lexical pinlabel order
            return left + ' '.join(get_net(package, pin.number)
                                   for pin in pins_by_label)

        return field

# ============================================================================

# Essentially, this back end works by collecting lists of output "cards"
# and then generating output.

# List of files to include
files = []

# This variable will hold the .subckt card if given.
subcircuit = False

# List of cards in the circuit or subcircuit.
# Note that a string here might actually represent a group
# of cards, using embedded newline characters.
cards = []

# Build sort key for package: a pair (number, refdes)

def number_package(package):
    # convert refdes tu number, ignoring non-numeric chars
    value = ''.join(c for c in package.refdes if c in '0123456789')
    try:
        return int(value), package.refdes
    except ValueError:
        return None, package.refdes

# Include "cards" from appropriate toplevel attributes.

def process_toplevel(netlist, attr):
    t = netlist.get_toplevel_attribute(attr, None)
    if t is not None:
        collect_card(netlist, expand_string(netlist, None, t))

# Put the "card" in the right place

def collect_card(netlist, card):
    global subcircuit
    if card.lower().startswith('.subckt'):
        # record a subcircuit card, error if more than one
        if subcircuit:
            netlist.error("More than one .subckt card generated!")
        else:
            subcircuit = card
    else:
        cards.append(card)

def run(f, netlist):
    # Collect input/output symbols and SPICE subcircuit IO pin symbols
    io_pins = []
    spice_io_pins = []
    for package in netlist.packages:
        device = package.get_attribute('device', 'unknown')
        if device in ['INPUT', 'OUTPUT', 'IO']:
            io_pins.append(package)
        if device == 'spice-IO':
            spice_io_pins.append(package)
    io_pins.sort(key = lambda package: package.refdes)
    # Sort list of packages by the numeric part of their refdes
    spice_io_pins.sort(key = number_package)

    # Write a header. Critical because SPICE may treat the first line
    # as a comment, even if it's not!
    f.write('* %s\n' % ' '.join(sys.argv))
    f.write('* SPICE file generated by spice-noqsi version 20130710\n'
            '* Send requests or bug reports to jpd@noqsi.com\n')

    for package in netlist.packages:
        # prevent munging from accidentally duplicating an existing
        # refdes by reserving all existing refdeses.
        refdes_reserved.add(package.refdes)

    # Collect file= attribute values.
    # Unlike previous SPICE back ends for gnetlist, this one allows
    # any symbol to request a file to be included.

    for package in netlist.packages:
        filename = package.get_attribute('file', None)
        if filename is not None:
            if filename in files:
                files.remove(filename)
            files.append(filename)

    process_toplevel(netlist, 'spice-prolog')

    for package in netlist.packages:
        # To process a part, we must find a suitable prototype,
        # fill in that prototype with instance data, and then figure
        # out if this is an ordinary "card" or a .SUBCKT

        proto = package.get_attribute('spice-prototype', None)

        if proto is None:
            # If no spice-prototype attribute, get prototype according to
            # the device attribute, or use the default for the "unknown"
            # device.
            try:
                proto = prototypes[package.get_attribute('device', 'unknown')]
            except KeyError:
                proto = prototypes['unknown']

        collect_card(netlist, expand_string(netlist, package, proto))

    process_toplevel(netlist, 'spice-epilog')

    if subcircuit:
        f.write('%s\n' % subcircuit)
    for f_ in files:
        f.write('.INCLUDE %s\n' % f_)
    for s in cards:
        f.write('%s\n' % s)
    if subcircuit:
        f.write('.ENDS\n')

# Default prototypes by device
# Note that the "unknown" prototype applies to all unlisted devices.

# Standard prototypes. Most of these are intended to be similar to the
# hard-wired behavior of spice-sdb.

prototypes = {
    'unknown': '? %pinseq value@ model-name@ spice-args@',
    'AOP-Standard': 'X? %pinseq model-name@',
    'BATTERY': 'V? #1 #2 spice-args@',
    'SPICE-cccs': 'F? #1 #2 V? value@\n'
                  'V? #3 #4 DC 0',
    'SPICE-ccvs': 'H? #1 #2 V? value@\n'
                  'V? #3 #4 DC 0',
    'directive': 'value@',
    'include': '*',                     # just a place to hang file=
    'options': '.OPTIONS value@',
    'CURRENT_SOURCE': 'I? %pinseq value@',
    'K': 'K? inductors@ value@',
    'SPICE-nullor': 'N? %pinseq value@1E6',
    'SPICE-NPN': 'Q? %pinseq model-name@ spice-args@ ic= temp=',
    'PNP_TRANSISTOR': 'Q? %pinseq model-name@ spice-args@ ic= temp=',
    'NPN_TRANSISTOR': 'Q? %pinseq model-name@ spice-args@ ic= temp=',
    'spice-subcircuit-LL': '.SUBCKT model-name@ %io',
    'spice-IO': '*',
    'SPICE-VC-switch': 'S? %pinseq model-name@ value@',
    'T-line': 'T? %pinseq value@',
    'vac': 'V? %pinseq value@',
    'SPICE-vccs': 'G? %pinseq value@',
    'SPICE-vcvs': 'E? %pinseq value@',
    'VOLTAGE_SOURCE': 'V? %pinseq value@',
    'vexp': 'V? %pinseq value@',
    'vpulse': 'V? %pinseq value@',
    'vpwl': 'V? %pinseq value@',
    'vsin': 'V? %pinseq value@',
    'VOLTAGE_SOURCE': 'V? %pinseq value@',
    'INPUT': '*',
    'OUTPUT': '*',
    'CAPACITOR': 'C? %pinseq value@ model-name@ spice-args@ l= w= area= ic=',
    'DIODE': 'D? %pinseq model-name@ spice-args@ area= ic= temp=',
    'NMOS_TRANSISTOR': 'M? %pinseq model-name@ spice-args@ '
                       'l= w= as= ad= pd= ps= nrd= nrs= temp= ic= m=',
    'PMOS_TRANSISTOR': 'M? %pinseq model-name@ spice-args@ '
                       'l= w= as= ad= pd= ps= nrd= nrs= temp= ic= m=',
    'RESISTOR': 'R? %pinseq value@ model-name@ spice-args@ w= l= area= temp=',
    'DUAL_OPAMP': 'X1? #3 #2 #8 #4 #1 model-name@\n'
                  'X2? #5 #6 #8 #4 #7 model-name@',
    'QUAD_OPAMP': 'X1? #3 #2 #11 #4 #1 model-name@\n'
                  'X2? #5 #6 #11 #4 #7 model-name@\n'
                  'X3? #10 #9 #11 #4 #8 model-name@\n'
                  'X4? #12 #13 #11 #4 #14 model-name@',
    'model': '* refdes@',
}
