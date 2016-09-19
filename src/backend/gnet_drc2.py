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

# --------------------------------------------------------------------------
#
# DRC backend written by Carlos Nieves Onega
#
# 2015-06-23: Translated to Python by Roland Lutz.
# 2010-12-11: Fix stack overflows with large designs.
# 2010-10-02: Applied patch from Karl Hammar. Do drc-matrix lower triangular
#             and let get-drc-matrixelement swap row/column if row < column.
# 2006-04-22: Display the pins when reporting a net with only one connection.
# 2006-04-08: Added support for DRC directives (DontCheckPintypes and
#             NoConnection), so the DRC doesn't depend on the net name
#             anymore.
#             Changed the drc connection matrix. Now an unknown pin doesn't
#             generate an error, and it can drive a net.
#             Added report for pins without the 'pintype' attribute.
# 2006-04-05: Fixed parenthesis mismatch in function drc2:check-slots.
#             Thanks to David Logan for reporting the bug.
# 2006-03-02: Don't check pintypes of net "NoConnection".
#             Thanks to Holger Oehm for the bug report and providing
#             a patch.
# 2006-02-28: Added netname in the output message when checking pintype
#             connections. Thanks to Holger Oehm for providing the patch.
# 2006-01-15: Changed error message to explain it a little bit.
# 2006-01-07: Added missing 'passive' in the pintype-full-names list, and
#             changed the pintype error/warning message to something more
#             self-explaining.
# 2005-02-11: Output to stdout if the output filename is "-".
# 2005-02-08: Use a parameter instead of the quiet mode of gnetlist so
#             gnetlist doesn't return a non-zero value when there are only
#             warnings. This parameter is 'ignore-warnings-in-return-value'.
# 2005-02-06: Make gnetlist return a non-zero value when errors or warnings
#             are found. If there is only warnings, the non-zero return value
#             can be disabled using the "quiet mode" option of gnetlist.
# 2005-02-06: Fixed bug when packages list is empty.
# 2005-01-23: Added check for duplicated references.
# 2003-10-24: Added numslots and slot attributes check.
# 2003-06-17: Added configuration support and slots check.
# 2003-06-05: Now checking for unconnected pins look into the DRC matrix if
#             it should issue an error, warning, or do nothing.
#             If the drc-matrix is defined before the execution of the backend,
#             then it's not overwritten. It allows backend configuration.
# 2003-06-04: Added check for unconnected pins and fix one small error
#             (index limit error).
# 2003-06-03: First release

# Parameters
# ----------
# Parameters should be passed to the backed using -O option in gnetlist's
# command line.
#
#   * ignore-warnings-in-return-value:
#         By default, this backend makes gnetlist return a non-zero
#         value when warnings or errors are found.  This is useful for
#         Makefiles.  Using this option, gnetlist will return a zero
#         value if there are only DRC warnings.
#
# Output
# ------
# By default, the backend outputs to the filename specified in the
# command line, or to stdout if the output filename is "-".
#
# Configuration
# -------------
# Some test can be disabled defining some variables.  Following is a
# list with a pair of check and variable.  If the variable is set to
# True, then that check is not performed.
#
#       Check                                    Variable [= Value]
# ----------------------------------------------------------------------------
# Not numbered parts.                     dont_check_non_numbered_parts
# Duplicated part references  (Note 1)    dont_check_duplicated_references
# Nets with only one connection.          dont_check_one_connection_nets
# Type of pins connected to each net.     dont_check_pintypes_of_nets
# Net not driven.                         dont_check_not_driven_nets
# Unconnected pins                        dont_check_unconnected_pins
# Values of slot and numslots attribs.    dont_check_slots
# Slot is used more than one time.        dont_check_duplicated_slots
# Reports unused slots                    dont_check_unused_slots
#     Don't report anything               action_unused_slots = 'c'
#     Report them as a warning            action_unused_slots = 'w'
#     Report them as an error             action_unused_slots = 'e'
#
# Note 1: DRC checks are case sensitive by default.  If you want them
# to be case insensitive, then you only have to set the variable
# 'case_insensitive' to True.
#
# Example:
#   dont_check_non_numbered_parts = True
#   dont_check_duplicated_references = True
#   dont_check_one_connection_nets = True
#   dont_report_unknown_pintypes = True
#   dont_check_pintypes_of_nets = True
#   dont_check_not_driven_nets = True
#   dont_check_unconnected_pins = True
#   dont_check_duplicated_slots = True
#   dont_check_unused_slots = True
#   action_unused_slots = 'w'
#   case_insensitive = True
#
# The check for not driven nets only is performed when checking the
# type of the pins connected to each net.
# There is a list which specifies which type of pin can drive a net.
# It's called pintype_can_drive.  It's a list, with 0 or 1 integer
# elements.  The order is specified below and is very important, since
# each position in the list matches one type of pin.  This list can be
# specified before running this backend, otherwise, the backend will
# use the default values.
#
# Example:
#   pintype_can_drive = [0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0]
#
# There are two checks that are configurable by a DRC connection
# matrix: check for unconnected pins and check for the type of pins
# connected to each net.
# Each element of the DRC matrix matches one connection between two
# pins (the "row" pin and the "column" pin).  The order is specified
# below and is very important, since each position in the list matches
# one type of pin.
# The DRC matrix can be specified before running this backend.
# Otherwise, the backend will use the default values.
#
# Example (default matrix):
#   drc-matrix = [      # Order is important !
#       'c',            # unknown
#       'cc',           # in
#       'cce',          # out
#       'ccwc',         # io
#       'ccewe',        # oc
#       'ccewce',       # oe
#       'ccccccc',      # pas
#       'cceweece',     # tp
#       'cceccccec',    # tri
#       'cccccccccc',   # clk
#       'cceweeceeec',  # pwr
#       'eeeeeeeeeeee'  # unconnected
#   ]

import sys
from util_getopt import *

# ============================== Configuration ===============================

dont_check_not_driven_nets = False
dont_check_non_numbered_parts = False
dont_check_duplicated_references = False
dont_check_connected_noconnects = False
dont_check_one_connection_nets = False
dont_report_unknown_pintypes = False
dont_check_pintypes_of_nets = False
dont_check_unconnected_pins = False
dont_check_slots = False
dont_check_duplicated_slots = False
dont_check_unused_slots = False

action_unused_slots = 'w'
case_insensitive = False

# define if a specified pin can drive a net
#                   unk in  out io  oc  oe  pas tp  tri clk pwr undef
pintype_can_drive = [1,  0,  1,  1,  1,  1,  1,  1,  1,  0,  1,  0]

# DRC matrix
# 'e': error    'w': warning   'c': correct
# Order is important!
drc_matrix = [
    # unknown in out io oc oe pas tp tri clk pwr unconnected
    'c',            # unknown
    'cc',           # in
    'cce',          # out
    'ccwc',         # io
    'ccewe',        # oc
    'ccewce',       # oe
    'ccccccc',      # pas
    'cceweece',     # tp
    'cceccccec',    # tri
    'cccccccccc',   # clk
    'cceweeceeec',  # pwr
    'eeeeeeeeeeee', # unconnected
]

# =========================== Internal definitions ===========================

# Pintype definitions.  Overwrite previous definitions, because the
# backend depends on them.

PINTYPE_COUNT = 12

(PINTYPE_UNKNOWN,
 PINTYPE_IN,
 PINTYPE_OUT,
 PINTYPE_IO,
 PINTYPE_OC,
 PINTYPE_OE,
 PINTYPE_PAS,
 PINTYPE_TP,
 PINTYPE_TRI,
 PINTYPE_CLK,
 PINTYPE_PWR,
 PINTYPE_UNDEFINED) = xrange(PINTYPE_COUNT)

pintype_names = [
    'unknown', 'in', 'out', 'io', 'oc', 'oe', 'pas', 'tp', 'tri', 'clk', 'pwr',
    'unconnected'
]
descriptive_pintype_names = [
    'unknown', 'input', 'output', 'input/output', 'open collector',
    'open emitter', 'passive', 'totem-pole', 'tristate', 'clock', 'power',
    'unconnected'
]

# Number of errors and warnings found
errors_number = 0
warnings_number = 0

def error(f, msg):
    global errors_number
    f.write("ERROR: %s\n" % msg)
    errors_number += 1

def warning(f, msg):
    global warnings_number
    f.write("WARNING: %s\n" % msg)
    warnings_number += 1

def ignore(f, msg):
    pass

def get_drc_matrix_element(row, column):
    if row < column:
        row, column = column, row
    try:
        return {
            'c': ignore,
            'w': warning,
            'e': error
        }[drc_matrix[row][column]]
    except KeyError:
        sys.stderr.write("INTERNAL ERROR: DRC matrix has unknown value "
                         "on position %s,%s\n" % (row, column))
        sys.exit(3)

# ======================== Symbol checking functions =========================

# Check for symbols not numbered.

def check_non_numbered_items(f, netlist, packages):
    for package in packages:
        if '?' in package.refdes:
            error(f, "Reference not numbered: %s" % package.refdes)

# Check for duplicated slots.
#
# Check if a slot of a package is used more than one time.
# Checks all packages in the design.

def check_duplicated_slots(f, netlist):
    for package in reversed(netlist.packages):
        slots = set()
        for slot in package.get_slots():
            if slot in slots:
                error(f, "duplicated slot %d of uref %s"
                         % (slot, package.refdes))
                break
            slots.add(slot)

# Checks for slots not used.

def check_unused_slots(f, netlist):
    if action_unused_slots == 'c':
        return

    for package in reversed(netlist.packages):
        try:
            numslots = int(package.get_attribute('numslots', 'unknown'))
        except ValueError:
            continue

        slots_list = package.get_unique_slots()

        for slot_number in xrange(numslots):
            if slot_number + 1 in slots_list:
                continue

            if action_unused_slots == 'e':
                error(f, "Unused slot %d of uref %s"
                         % (slot_number + 1, package.refdes))
            else:
                warning(f, "Unused slot %d of uref %s"
                           % (slot_number + 1, package.refdes))

# Check slot number is greater or equal than numslots for all packages.

def check_slots(f, netlist):
    for package in reversed(netlist.packages):
        numslots_string = package.get_attribute('numslots', 'unknown')
        slots = package.get_all_attributes('slot')
        if not slots or slots[0] is None:
            slot_string = 'unknown'
        else:
            slot_string = slots[0]

        if slot_string.lower() == 'unknown':
            # If slot attribute is not defined.
            if numslots_string.lower() == 'unknown' or \
               numslots_string == '0':
                # No slot neither numslots (or set to zero) attributes defined.
                # This is correct.
                #f.write("No slotted reference: %s\n" % package.refdes)
                continue
            slot = None
        else:
            # Slot attribute defined.
            # If it's a number, then check slots.
            # If it's not, then report an error.
            try:
                slot = int(slot_string)
            except ValueError:
                # Slot attribute is not a number.
                error(f, "Reference %s: "
                         "Incorrect value of slot attribute (%s)."
                         % (package.refdes, slot_string))
                continue

        try:
            numslots = int(numslots_string)
        except ValueError:
            error(f, "Reference %s: "
                     "Incorrect value of numslots attribute (%s)."
                     % (package.refdes, numslots_string))
            continue

        if slot is None:
            # If no slot attribute, but numslots is defined and not zero.
            # If numslots is a number, then slot should be defined.
            error(f, "Multislotted reference %s "
                     "has no slot attribute defined." % package.refdes)
            continue

        for this_slot in package.get_unique_slots():
            if this_slot > numslots or this_slot < 1:
                # If slot is not between 1 and numslots,
                # then report an error.
                error(f, "Reference %s: Slot out of range (%d)."
                         % (package.refdes, this_slot))

# Check duplicated references of the given list.
#
# If the number of ocurrences of a reference in the schematic doesn't
# match the number of unique slots used by that part, then that
# reference is used more than one time in the schematic.

def check_duplicated_references(f, netlist, packages):
    for package in reversed(netlist.packages):
        count = 0
        if case_insensitive:
            for component in netlist.components:
                if component.refdes.lower() == package.refdes.lower():
                    count += 1
        else:
            for component in netlist.components:
                if component.refdes == package.refdes:
                    count += 1

        if count > len(package.get_unique_slots()):
            error(f, "Duplicated reference %s." % package.refdes)

# ========================== Net checking functions ==========================

# Check for NoConnection nets with more than one pin connected.

def check_connected_noconnects(f, netlist, all_nets):
    for net in all_nets:
        directives = net.graphical_objs_with_attrib_get_attrib(
            'device', 'DRC_Directive', 'value')

        # Only check nets with a NoConnection directive
        if 'NoConnection' not in directives:
            continue

        if len(net.connections) > 1:
            error(f, "Net '%s' has connections, "
                     "but has the NoConnection DRC directive: %s."
                     % (net.name, display_pins_of_type(netlist, 'all', net)))

# Check for nets with less than two pins connected.

def check_single_nets(f, netlist, all_nets):
    for net in all_nets:
        directives = net.graphical_objs_with_attrib_get_attrib(
            'device', 'DRC_Directive', 'value')

        # If one of the directives is NoConnection,
        # then it shouldn't be checked.
        if 'NoConnection' in directives:
            continue

        if len(net.connections) == 0:
            error(f, "Net '%s' has no connections." % net.name)
        if len(net.connections) == 1:
            error(f, "Net '%s' is connected to only one pin: %s."
                  % (net.name, display_pins_of_type(netlist, 'all', net)))

# Return a list with the pintypes of the pins connected to a net.

def get_pintypes_of_net_connections(netlist, net):
    return [pin.get_attribute('pintype', 'unknown')
            for pin in reversed(net.connections)]

# Count pintypes of a net.

def count_pintypes_of_net(f, net):
    output_list = PINTYPE_COUNT * [0]
    for type in net:
        try:
            output_list[pintype_names.index(type.lower())] += 1
        except ValueError:
            f.write("INTERNAL ERROR: unknown pin type : %s\n" % type)
    return output_list

# Display pins of a specified type connected to a net.
#
# type: pin type index, or the string "all" to display all the pins.

def display_pins_of_type(netlist, type, net):
    return ''.join(
        '%s:%s ' % (pin.package.refdes, pin.number)
        for pin in reversed(net.connections)
        if type == 'all' or pin.get_attribute('pintype', 'unknown').lower()
                                == pintype_names[type].lower())

# Check connection between two pintypes.
#
# type1, type2: pin type indices

def check_connection_of_two_pintypes(
        f, netlist, type1, type2, net):
    proc = get_drc_matrix_element(type1, type2)
    proc(f, "Pin(s) with pintype '%s': %s\n"
            "\tare connected by net '%s'\n"
            "\tto pin(s) with pintype '%s': %s" % (
                descriptive_pintype_names[type1],
                display_pins_of_type(netlist, type1, net),
                net.name,
                descriptive_pintype_names[type2],
                display_pins_of_type(netlist, type2, net)))

# Check pintypes of the pins connected to a single net.
#
# pintype_count: vector with the number of pins connected to a single net,
#                by pintype.

def check_pintypes_of_single_net(
        f, netlist, net, pintypes, pintype_count):
    for type1 in xrange(PINTYPE_COUNT - 1):
        type1_count = pintype_count[type1]
        if type1_count == 0:
            continue

        for type2 in xrange(type1, PINTYPE_COUNT - 1):
            type2_count = pintype_count[type2]
            if type2_count == 0:
                continue

            if (type1 == type2 and type1_count > 1) or \
               (type1 != type2 and type1_count > 0 and type2_count > 0):
                check_connection_of_two_pintypes(
                    f, netlist, type1, type2, net)

# Check if a net has a pintype which can drive the net.
#
# pintype_count: vector with the number of pins connected to a single net,
#                by pintype.

def check_if_net_is_driven(pintype_count):
    for position in xrange(PINTYPE_COUNT - 1):
        if pintype_count[position] > 0 and pintype_can_drive[position] == 1:
            return True

    return False

# Check pintype of the pins connected to every net in the design.

def check_pintypes_of_nets(f, netlist, all_nets):
    for net in all_nets:
        pintypes = get_pintypes_of_net_connections(netlist, net)
        pintype_count = count_pintypes_of_net(f, pintypes)
        directives = net.graphical_objs_with_attrib_get_attrib(
            'device', 'DRC_Directive', 'value')

        # If some directives are defined, then it shouldn't be checked.
        if 'DontCheckPintypes' not in directives:
            check_pintypes_of_single_net(
                f, netlist, net, pintypes, pintype_count)

        if not dont_check_not_driven_nets and \
           'DontCheckIfDriven' not in directives and \
           'NoConnection' not in directives and \
           not check_if_net_is_driven(pintype_count):
            error(f, "Net %s is not driven." % net.name)

# Check unconnected pins.

def check_unconnected_pins(f, netlist, packages):
    for package in packages:
        for pin in package.pins:
            if not pin.net.is_unconnected_pin:
                continue

            try:
                position = pintype_names.index(
                    pin.get_attribute('pintype', 'unknown').lower())
            except ValueError:
                position = PINTYPE_COUNT

            proc = get_drc_matrix_element(PINTYPE_UNDEFINED, position)
            proc(f, "Unconnected pin %s:%s" % (package.refdes, pin.number))

# Report pins without the 'pintype' attribute (pintype=unknown).

def report_unknown_pintypes(f, netlist, nets):
    nets = list(nets)

    # count unknown pintypes
    count = 0
    for net in nets:
        pintypes = get_pintypes_of_net_connections(netlist, net)
        pintype_count = count_pintypes_of_net(f, pintypes)
        count += pintype_count[PINTYPE_UNKNOWN]

    # display unknown pintypes
    if count > 0:
        f.write("NOTE: Found pins without the 'pintype' attribute: ")
        f.write(''.join(
            display_pins_of_type(netlist, PINTYPE_UNKNOWN, net)
            for net in nets))
        #message("\n")

# ========================== Highest level function ==========================

def run(f, netlist, args):
    options = backend_getopt(args, {
        'ignore-warnings-in-return-value': Option(False, NO_ARGUMENT, None)
    })

    if not isinstance(pintype_can_drive, list) or \
       len(pintype_can_drive) != PINTYPE_COUNT or \
       not next((False for x in pintype_can_drive
                 if not isinstance(x, int) or x not in [0, 1]), True):
        sys.stderr.write("INTERNAL ERROR: List of pins which can drive a net "
                         "bad specified.\n")
        sys.exit(3)

    # Perform DRC-matrix sanity checks.
    # See if all elements of the matrix are chars

    for row in xrange(PINTYPE_COUNT):
        for column in xrange(row + 1):
            c = drc_matrix[row][column]
            if not isinstance(c, basestring) or len(c) != 1:
                sys.stderr.write("INTERNAL ERROR: DRC matrix has unknown "
                                 "value on position %s,%s\n" % (row, column))
                sys.exit(3)

    if action_unused_slots not in 'wce':
        sys.stderr.write("INTERNAL ERROR: Action when unused slots are found "
                         "has a wrong value.\n")
        sys.exit(3)

    # Check non-numbered symbols
    if not dont_check_non_numbered_parts:
        f.write("Checking non-numbered parts...")
        f.write("\n")
        check_non_numbered_items(f, netlist, reversed(netlist.packages))
        f.write("\n")

    # Check for duplicated references
    if not dont_check_duplicated_references:
        f.write("Checking duplicated references...")
        f.write("\n")
        check_duplicated_references(f, netlist, reversed(netlist.packages))
        f.write("\n")

    # Check for NoConnection nets with more than one pin connected.
    if not dont_check_connected_noconnects:
        f.write("Checking NoConnection nets for connections...")
        f.write("\n")
        check_connected_noconnects(f, netlist, reversed(netlist.nets))
        f.write("\n")

    # Check nets with only one connection
    if not dont_check_one_connection_nets:
        f.write("Checking nets with only one connection...")
        f.write("\n")
        check_single_nets(f, netlist, reversed(netlist.nets))
        f.write("\n")

    # Check "unknown" pintypes
    if not dont_report_unknown_pintypes:
        f.write("Checking pins without the 'pintype' attribute...")
        f.write("\n")
        report_unknown_pintypes(f, netlist, reversed(netlist.nets))
        f.write("\n")

    # Check pintypes of the pins connected to every net
    if not dont_check_pintypes_of_nets:
        f.write("Checking type of pins connected to a net...")
        f.write("\n")
        check_pintypes_of_nets(f, netlist, reversed(netlist.nets))
        f.write("\n")

    # Check unconnected pins
    if not dont_check_unconnected_pins:
        f.write("Checking unconnected pins...")
        f.write("\n")
        if netlist.packages:
            check_unconnected_pins(f, netlist, reversed(netlist.packages))
        f.write("\n")

    # Check slots
    if not dont_check_slots:
        f.write("Checking slots...")
        f.write("\n")
        check_slots(f, netlist)
        f.write("\n")

    # Check for duplicated slots
    if not dont_check_duplicated_slots:
        f.write("Checking duplicated slots...")
        f.write("\n")
        check_duplicated_slots(f, netlist)
        f.write("\n")

    # Check for unused slots
    if not dont_check_unused_slots:
        f.write("Checking unused slots...")
        f.write("\n")
        check_unused_slots(f, netlist)
        f.write("\n")

    # Display total number of warnings
    if warnings_number > 0:
        f.write("Found %s warnings.\n" % warnings_number)
    else:
        f.write("No warnings found. \n")

    # Display total number of errors
    if errors_number > 0:
        f.write("Found %s errors.\n" % errors_number)
    else:
        f.write("No errors found. \n")

    # Make gnetlist return an error if there are DRC errors.
    # If there are only warnings and it's in quiet mode, then
    # do not return an error.
    if f is not sys.stdout and errors_number > 0:
        sys.stderr.write("DRC errors found. See output file.\n")
    elif warnings_number > 0 and \
            'ignore-warnings-in-return-value' not in options:
        sys.stderr.write("DRC warnings found. See output file.\n")
