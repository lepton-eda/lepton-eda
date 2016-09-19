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
# SPICE netlist backend written by S. Gieltjes
#
# further modified by W. Kazubski to use scaling parameters for
# devices other than MOSFETS
#
# Started with gnet-spice1.scm by W. Kazubski.  Radically
# hacked by SDB to support advanced spice netlist generation.
# Project started 2003-03-05 -- SDB.
#
# Details and documentation at http://www.brorson.com/gEDA/SPICE/
#
# Change log:
# 2003-03-05 -- Started hacking.  SDB.
# 2003-03-17 -- 2nd version.  Hacked to allow for .SUBCKT files to model
#               ics.  Changed write-ic.  Added get-file-type.  Added
#               write-subcircuit.  SDB.
# 2003-03-31 -- 3rd version.  Hacked to enable creating .SUBCKT schematics
#               for hierarchical circuit modeling.
# 2003-08-29 -- 4th version.  Include patches from Ken Healy to sort
#               netlist, code by SDB to use gnetlist command line args in
#               Scheme fcns, as well as from Theo Deckers to fix strange
#               problem with '.SUBCKT quoting.
# 2003-09-09 -- 5th version.  Rearranged code for more organization (I was
#               beginning to get lost. . . .).  Incorporated changes to
#               handle external SPICE files more intelligently.  Changed
#               spew to be configurable by setting -v from the command line.
#               Placed new fcn debug-spew into gnetlist.scm.
#               Added -I command line flag.
# 2003-10-14 -- Bugfixes: Added empty-string? and hacked get-file-type to
#               handle case where a model file has an empty line before
#               .SUBCKT or .MODEL.
#               Also modified write-net-names-on-component to gracefully
#               handle case where not every pin has a pinseq attribute.
#               Now only outputs pins with valid pinseq attribute.
# 2003-12-25 -- Bugfix: Unswizzled emission of pins from user-defined
#               .subckts.  (Now correctly uses pinseq to define emission
#               order of pins.)  Also added ability to emit attributes for
#               semiconductors (e.g. area, off, ic, etc.)  Added in response
#               to user requests.
# 2003-12-29 -- * Two small enhancements requested by Peter Kaiser.
#               * Minor bugfix.
#               * Second minor bugfix.
#               * Change res & cap to incorporate modelname & "area=" attrib.
# 2004-03-24 -- Bugfixes made to JFET stuff during Feb.  Change released now.
# 2004-08-22 -- Added command line as first line of file.
# 2004-08-29 -- Changed sense source naming in controlled sources because
#               the old convention was confusing ngspice.
# 2004-10-09 -- Added patches for voltage controlled switches from
#               Peter Kaiser.
# 2005-03-16 -- Fixed CCCS bug (typo in Vsense) noticed by David Logan
# 2005-05-16 -- Modified behavior of .INCLUDE directive.  Now by default it
#               just spits out the string instead of putting the contents of
#               the file into the SPICE netlist.  You can force insertion of
#               the file using the -e flag.
# 2005-06-12 -- Changed order of writing out netlist and .model/.subckt
#               cards to facilitate use of numparam with ngspice.  Change
#               supplied by Dominique Michel.
# 2005-09-11 -- Incorporated patch from Paul Bunyk to enable netlisting of
#               Josephson junctions and "K" mutual inductances.  Also
#               enabled netlisting of "COIL" devices as inductors.
# 2005-12-27 -- Fix bug discovered by John Doty: spice-IO pins with refdes
#               greater than P9 were sorted incorrectly (as strings).  Now
#               they are sorted as numbers.
# 2006-03-10 -- Added "m" attribute to PMOS and NMOS per request of
#               Peter Kaiser.
# 2006-04-11 -- Changed the .END and .ENDS cards to lowercase.
#               This fixes bug 1442912. Carlos Nieves Onega.
# 2007-02-10 -- Various bugfixes.  Also incorporated slotted part netlist
#               patch from Jeff Mallatt.  SDB.
# 2007-04-28 -- Fixed slotted part stuff so that it uses pinseq to emit
#               pins.  SDB
# 2008-01-09 -- Fix slotted part handling to work without a modified pinseq.
#               pcjc2
# 2011-01-03 -- Combine write-ic and write-subcircuit with a fix to the
#               unbound type variable.  Fully document a check for the
#               special "?" value explaining why it fails silently.  Clean
#               up write-net-names-on-component to make it a bit more
#               flexible.
#               Combine write-probe-item and write-net-names-on-component.
#               Add a range utility function.  CC
# 2011-01-13 -- Add four lines of code (and some comments) that allow
#               formaitting strings to be used for netlisting NGspice device
#               models.  CC
# 2011-06-12 -- Updated the Problematci name=? symbols to name=unknown and
#               removed the FIXME check for them.  This should be a step
#               closer to place holder consistancy.  CC
# 2015-05-14 -- Translated to Python.  Roland Lutz
#
#******************************************************************************
#
#  Organization of gnet-spice-sdb.scm file:
#  --  Functions for program housekeeping, handling of calling flags,
#      file manipulation.
#  --  Functions for handling nets & devices and creating SPICE cards.
#  --  High-level functions which control program flow.  Note that the
#      program entry point lives at the very bottom of this file.
#
#  Unfortunately, no organization is present beneath this top level. . . .
#
#******************************************************************************

import os.path, sys
from util_getopt import *

# Common functions for the `spice' and `spice-sdb' backends
import spice_common

enable_debug = False

# Spew debug messages if enable_debug is set, otherwise do nothing.

def debug_spew(debug_string):
    if enable_debug:
        sys.stderr.write(debug_string)

# Custom get-uref function to append ".${SLOT}" where a component has
# a "slot=${SLOT}" attribute attached.
#
# NOTE: Original test for appending the ".<SLOT>" was this:
#   (let ((numslots netlist.get-package-attribute(package, "numslots"))
#        (slot-count (length netlist.get-unique-slots(package)))
#     (if (or (string=? numslots "unknown") (string=? numslots "0"))

def get_uref(netlist, object):
    real_uref = netlist.get_uref(object)
    x = get_attrib_value_by_attrib_name(object, 'slot')
    if x is None:
        return real_uref
    else:
        return real_uref + '.' + x[0]

############ Program housekeeping, handling calling flags, etc. ############

# Loops through the model-file list, and for each file name discovered
# in the list, it processes the file by invoking handle-spice-file.

def loop_through_files(f, file_info_list):
    for list_element in file_info_list:
        model_name = list_element[0]
        file_name = list_element[1]
        file_type = list_element[2]
        handle_spice_file(f, file_name)

# Loops through the model-file list looking for triplet corresponding
# to model-name.  If found, it returns the corresponding list.  If not
# found, returns False.

def get_file_info_list_item(model_name, file_info_list):
    for list_element in file_info_list:
        list_elt_model_name = list_element[0]
        list_elt_file_name = list_element[1]
        list_elt_file_type = list_element[2]

        if list_elt_model_name == model_name:
            # found model_name.  Return list_element.
            return list_element

    return None

# This wraps insert_text_file.
#
# It looks to see if the -I flag was set at the command line.  If so,
# it just writes a .INCLUDE card with the file name.  If not, it calls
# insert_text_file to stick the file's contents into the SPICE netlist.

def handle_spice_file(f, file_name):
    debug_spew("Handling spice model file %s\n" % file_name)
    if 'include_mode' in calling_flags:
        # -I found: just print out .INCLUDE card
        f.write('.INCLUDE %s\n' % file_name)
    else:
        # -I not found: invoke insert_text_file
        insert_text_file(f, file_name)

# Given a filename, open the file, get the contents, and dump them
# into the spice file.
#
# This function is usually used to include spice models contained in
# files into the netlist.  Note that it doesn't check the correctness
# of the spice code in the file -- you're on your own!

def insert_text_file(f, model_filename):
    if not os.path.isfile(model_filename):
        sys.stderr.write("ERROR: File '%s' not found.\n" % model_filename)
        sys.exit(3)

    model_file = open(model_filename)
    try:
        f.write('*vvvvvvvv  Included SPICE model from %s vvvvvvvv\n'
                % model_filename)

        for model_line in model_file:
            f.write(model_line)
    finally:
        model_file.close()

    f.write('*^^^^^^^^  End of included SPICE model from %s ^^^^^^^^\n'
            % model_filename)
    f.write('*\n')

# Figure out if this schematic is a .SUBCKT lower level.  This is
# determined if there is a spice-subcircuit-LL device instantiated
# somewhere on the schematic.  If it is a .SUBCKT, return ".SUBCKT
# model-name".

def get_schematic_type(netlist):
    for package in reversed(netlist.packages):
        if package.get_attribute('device', None) == 'spice-subcircuit-LL':
            # look for subcircuit label
            return '.SUBCKT ' + package.get_attribute('model-name', 'unknown')

    # no spice-subcircuit-LL found
    return 'normal schematic'

# Extract the modelname from the .SUBCKT modelname line.
# Just grab the chars from char 8 to the end of the string.

def get_subcircuit_modelname(schematic_type):
    return schematic_type[8:]

# This iterates through the schematic and compiles a list of all
# spice-IO pins found.  This is used when writing out a .SUBCKT lower
# level netlist.

def get_spice_IO_pins(netlist):
    spice_io_package_list = []
    for package in reversed(netlist.packages):
        # look for subcircuit label
        if package.get_attribute('device', None) == 'spice-IO':
            # we have found a spice-IO pin.
            spice_io_package_list.append(package)

    # end iteration & return list
    spice_io_package_list.reverse()
    return spice_io_package_list

# This takes the list of io-pin-packages and sorts it in order of
# refdes.  Repaired on 2005-12-27 to correctly sort pin numbers > 9.

def sort_spice_IO_pins(package_list):
    return sort(package_list, key = lambda x: int(x.refdes[1:]))

# Given a list of spice-IO packages (refdeses), this function returns
# the list of nets attached to the IOs.

def get_IO_nets(package_list):
    net_list = []
    for package in package_list:
        # get the net attached to pin 1
        net_list.append(package.pins_by_number['1'].net.name)

    # end iteration & return net_list
    net_list.reverse()
    return net_list

# Given a filename, open the file, get the first line, and see if it
# is a .MODEL or .SUBCKT file.  Returns either ".MODEL" or ".SUBCKT"
# or "OTHER".  The function opens the model file, and closes it when
# it is done.

def get_file_type(model_filename):
    if not os.path.isfile(model_filename):
        sys.stderr.write("ERROR: File '%s' not found.\n" % model_filename)
        sys.exit(3)

    model_file = open(model_filename)
    try:
        for file_line in model_file:
            if file_line.startswith('.'):
                debug_spew("In get_file_type, first_char = .\n")
                if file_line[:7].lower() == '.subckt':
                    # found .subckt as first line.
                    return '.SUBCKT'
                if file_line[:6].lower() == '.model':
                    # found .model as first line.
                    return '.MODEL'
                # first . spice card is neither .model nor .subckt
                return 'OTHER'

        # Arrived at end of line without finding .MODEL or .SUBCKT.
        return 'OTHER'
    finally:
        model_file.close()

# Write prefix if first char of refdes is improper, e.g. if MOSFET is
# named T1 then becomes MT1 in SPICE.

def write_prefix(f, package, prefix):
    different_prefix = package.refdes[0] != prefix
    nomunge = 'nomunge_mode' in calling_flags

    debug_spew("Checking prefix.  Package prefix =%s\n" % package.refdes[0])
    debug_spew("                  correct prefix =%s\n" % prefix)
    debug_spew("   nomunge mode = %s\n  different_prefix=%s\n"
               % (nomunge, different_prefix))
    if different_prefix and not nomunge:
        f.write(prefix)

# Sort procedure to order refdes's alphabetically but keep A? packages
# at the end of list so SPICE simulation directives operate correctly.
#
# This fcn written by Ken Healy to enable SPICE netlisting for Gnucap,
# which wants A refdes cards (i.e. SPICE directives) to appear last in
# the SPICE netlist.  Slightly modified and incorporated into main
# spice-sdb release by SDB on 2003-09-01.  To output the netlist in
# sorted order, use the -s switch when invoking gnetlist from the
# command line.
#
# Example: gnetlist -s -g spice-sdb -o output.spice schematic.sch
#
# The default behavior (i.e. if -s is not specified) is to do no
# sorting.

def packsort(x, y):
    xdes = x.refdes[0].lower()
    ydes = y.refdes[0].lower()
    xnum = x.refdes[1:].lower()
    ynum = y.refdes[1:].lower()

    if xdes == ydes:
        if xnum < ynum:
            return -1
        if xnum > ynum:
            return 1
        return 0
    if xdes == 'a':  # and ydes != 'a'
        return 1
    if ydes == 'a':  # and xdes != 'a'
        return -1
    if xdes < ydes:
        return -1
    if xdes > ydes:
        return 1
    return 0


################ Dealing with nets, devices, & SPICE cards. ################

# Write-transistor-diode: writes out component followed by model or
# model file associated with the component.
# This function does the following:
#   1.  Writes out the correct refdes prefix (if specified and necessary).
#   2.  Writes out the refdes and nets
#   3.  Looks for "model-name" attribute. Writes it out if it exists.
#   4.  If there is no "model-name" attribute, it writes out the "value"
#       attribute.  If there is no "value" attribute, it writes out "unknown"
#       and returns, causing the spice simulator to puke when the netlist
#       is run.  This is important
#       'cause the spice simulator needs to have some indication of what
#       model to look for.
#   5.  Outputs optional attributes attached to device, if any.  Feature
#       added by SDB on 2003-12-25.
#   6.  Outputs a new line
#   7.  Looks for a the "model" attribute.  If it exists, it it writes out
#       a .MODEL line like this:  .MODEL model-name type (model)

def write_transistor_diode(f, package, prefix, type, attrib_list):
    # First do local assignments
    model_name = package.get_attribute('model-name', None)
    model = package.get_attribute('model', None)
    value = package.get_attribute('value', None)
    area = package.get_attribute('area', None)
    off = package.get_attribute('off', None)
    model_file = package.get_attribute('file', None)

    # Write out the refdes prefix, if specified and necessary.
    if prefix:
        write_prefix(f, package, prefix)

    # Next we write out the refdes and nets.
    write_component_no_value(f, package)

    # next look for "model-name" attribute.  Write it out if it exists.
    # otherwise look for "value" attribute.
    if model_name is not None:
        # display model-name if known
        f.write(model_name + ' ')
    elif value is not None:
        # otherwise display value
        f.write(value + ' ')
    else:
        f.write('unknown ')

    # Next write out attributes if they exist
    # First attribute is area.  It is written as a simple string
    if area is not None:
        f.write(area + ' ')

    # Next attribute is off.    It is written as a simple string
    if off is not None:
        f.write(off + ' ')

    # Write out remaining attributes
    spice_common.write_list_of_attributes(f, package, attrib_list)

    # Now write out newline in preparation for writing out model.
    f.write('\n')

    # Now write out any model which is pointed to by the part.
    # one line model and model name exist
    if model is not None and model_name is not None:
        debug_spew("found model and model-name for %s\n" % package.refdes)
        f.write('.MODEL %s %s (%s)\n' % (model_name, type, model))
    # one line model and component value exist
    elif model_name is not None and value is not None:
        debug_spew("found model and value for %s\n" % package.refdes)
        f.write('.MODEL %s %s (%s)\n' % (model_name, type, value))
    # model file and model name exist
    elif model_file is not None and model_name is not None:
        debug_spew("found file and model-name for %s\n" % package.refdes)
        debug_spew("I'll deal with the file later . . .\n")
    # model file and component value exist
    elif model_file is not None and value is not None:
        debug_spew("found file and value for %s\n" % package.refdes)
        debug_spew("I'll deal with the file later . . .\n")

# This writes out a valid diode refdes & then calls the function which
# writes the rest of the line.

def write_diode(f, package):
    debug_spew("Found diode.  Refdes = %s\n" % package.refdes)
    write_transistor_diode(f, package, 'D', 'D', ['ic', 'temp'])

# This writes out a valid ic or subcircuit line.
# The algorithm is as follows:
#  1.  Figure out what type of model goes with this part from
#      file-info-list.  If it isn't listed, look for a MODEL attribute.
#      If MODEL attribute is attached, write out SPICE card, and then
#      write out .MODEL on next line.
#      If no MODEL attribute is attached, just write out what little
#      we know.  Then return
#  2.  If the model-name is in the file-info-list, get the associated
#      file-type.  Compare it against the component's refdes.  If model-type
#      is .MODEL or .SUBCKT and refdes doesn't begin with a U or X
#      respectively, prepend the correct prefix to the refdes.
#  3.  Print out the rest of the line.

def write_ic(f, package, file_info_list):
    # First do local assignments
    first_char = package.refdes[0]  # extract first char of refdes
    value = package.get_attribute('value', 'unknown')
    # If model-name is empty, we use value attribute instead.
    model_name = package.get_attribute('model-name', value)
    model = package.get_attribute('model', None)
    type = package.get_attribute('type', None)

    if first_char == 'U':
        debug_spew("Found ic.  Refdes = %s\n" % package.refdes)
    elif first_char == 'X':
        debug_spew("Found subcircuit.  Refdes = %s\n" % package.refdes)

    # Now get item from file_info_list using model_name as key
    list_item = get_file_info_list_item(model_name, file_info_list)

    # check to see if list_item is null.
    if list_item is None:
        # Evidently, we didn't discover any files holding this model.
        # Instead we look for model attribute
        if model is not None:
            # model attribute exists -- write out card and model.
            debug_spew("Model info not found in model file list, "
                       "but model attribute exists.  "
                       "Write out spice card and .model line..\n")
            write_component_no_value(f, package)
            f.write(model_name + '\n')
            f.write('.MODEL %s ' % model_name)
            if type is not None:
                f.write(type + ' ')
            # If no type then just skip it.
            f.write('(%s)\n' % model)
        else:
            # no model attribute either.  Just write out card.
            debug_spew("Model info not found in model file list.  No "
                       "model attribute either.  Just write what we know.\n")
            write_component_no_value(f, package)
            f.write(model_name + '\n')

    else:
        # list_item is not null.  Therefore we process line depending
        # upon contents of list_item
        file_type = list_item[2]

        if file_type == '.MODEL':
            # ---- file holds a model ----
            debug_spew("Found .MODEL with model-file and model-name for %s\n"
                       % package.refdes)
            write_prefix(f, package, 'U')
            # this prepends an "U" to the refdes if needed, since we
            # have a .model
            write_component_no_value(f, package)
            f.write(model_name + '\n')
            debug_spew("We'll handle the file contents later . . .\n")

        elif file_type == '.SUBCKT':
            # ---- file holds a subcircuit ----
            debug_spew("Found .SUBCKT with model-file and model-name for %s\n"
                       % package.refdes)
            write_prefix(f, package, 'X')
            # this prepends an "X" to the refdes if needed, since we
            # have a .subckt
            write_component_no_value(f, package)
            f.write(model_name + '\n')
            debug_spew("We'll handle the file contents later . . .\n")

# This writes out a valid transistor refdes & then calls the function
# which writes the rest of the line.

def write_npn_bipolar_transistor(f, package):
    debug_spew("Found npn bipolar transistor.  Refdes = %s\n" % package.refdes)
    write_transistor_diode(f, package, 'Q', 'NPN', ['ic', 'temp'])

def write_pnp_bipolar_transistor(f, package):
    debug_spew("Found pnp bipolar transistor.  Refdes = %s\n" % package.refdes)
    write_transistor_diode(f, package, 'Q', 'PNP', ['ic', 'temp'])

# Write n-channel JFET transistor.

def write_nfet_transistor(f, package):
    debug_spew("Found n-channel JFET.  Refdes = %s\n" % package.refdes)
    write_transistor_diode(f, package, 'J', 'NJF', ['ic', 'temp'])

# Write p-channel JFET transistor.

def write_pfet_transistor(f, package):
    debug_spew("Found p-channel JFET.  Refdes = %s\n" % package.refdes)
    write_transistor_diode(f, package, 'J', 'PJF', ['ic', 'temp'])

def write_pmos_transistor(f, package):
    debug_spew("Found PMOS transistor.  Refdes = %s\n" % package.refdes)
    write_transistor_diode(f, package, 'M', 'PMOS', [
        'l', 'w', 'as', 'ad', 'pd', 'ps', 'nrd', 'nrs', 'temp', 'ic', 'm'])

def write_nmos_transistor(f, package):
    debug_spew("Found NMOS transistor.  Refdes = %s\n" % package.refdes)
    write_transistor_diode(f, package, 'M', 'NMOS', [
        'l', 'w', 'as', 'ad', 'pd', 'ps', 'nrd', 'nrs', 'temp', 'ic', 'm'])

def write_subckt_pmos_transistor(f, package):
    debug_spew("Found PMOS subcircuit transistor.  "
               "Refdes = %s\n" % package.refdes)
    write_transistor_diode(f, package, 'X', 'PMOS', [
        'l', 'w', 'as', 'ad', 'pd', 'ps', 'nrd', 'nrs', 'temp', 'ic', 'm'])

def write_subckt_nmos_transistor(f, package):
    debug_spew("Found NMOS subcircuit transistor.  "
               "Refdes = %s\n" % package.refdes)
    write_transistor_diode(f, package, 'X', 'NMOS', [
        'l', 'w', 'as', 'ad', 'pd', 'ps', 'nrd', 'nrs', 'temp', 'ic', 'm'])

# ************  Fix this!!!!!!!!!!  **************
def write_mesfet_transistor(f, package):
    write_transistor_diode(f, package, 'Z', 'MESFET', [])
    # XXXXXX Fix this!!!

# Write voltage controlled switch.

def write_vc_switch(f, package):
    debug_spew("Found voltage controlled switch.  "
               "Refdes = %s\n" % package.refdes)
    write_transistor_diode(f, package, 'S', 'SW', [' '])

def write_resistor(f, package):
    debug_spew("Found resistor.  Refdes = %s\n" % package.refdes)

    # first write out refdes and attached nets
    write_component_no_value(f, package)

    # next write out mandatory resistor value if it exists.
    value = package.get_attribute('value', None)
    if value is not None:
        f.write(value + ' ')

    # next write our model name if it exists
    model_name = package.get_attribute('model-name', None)
    if model_name is not None:
        f.write(model_name + ' ')

    # write the attributes (if any) separately
    # I include non-standard "area" attrib here per popular demand
    # in the list of attributes which can be attached to a resistor.
    spice_common.write_list_of_attributes(
        f, package, ['area', 'l', 'w', 'temp'])
    # add additional space. . . .
    f.write(' ')

    # finally output a new line
    f.write('\n')

def write_capacitor(f, package):
    debug_spew("Found capacitor.  Refdes = %s\n" % package.refdes)

    # first write out refdes and attached nets
    write_component_no_value(f, package)

    # next write capacitor value, if any.  Note that if the
    # component value is not assigned nothing will be written out.
    value = package.get_attribute('value', None)
    if value is not None:
        f.write(value + ' ')

    # next write capacitor model name, if any.  This is applicable to
    # semiconductor caps used in chip design.
    model_name = package.get_attribute('model-name', None)
    if model_name is not None:
        f.write(model_name + ' ')

    # Next write out attributes if they exist.  Use
    # a list of attributes which can be attached to a capacitor.
    # I include non-standard "area" attrib here per request of Peter Kaiser.
    spice_common.write_list_of_attributes(
        f, package, ['area', 'l', 'w', 'ic'])
    # write the off attribute separately
    # add additional space. . . .
    f.write(' ')

    f.write('\n')

def write_inductor(f, package):
    debug_spew("Found inductor.  Refdes = %s\n" % package.refdes)

    # first write out refdes and attached nets
    write_component_no_value(f, package)

    # next write inductor value, if any.  Note that if the
    # component value is not assigned, then it will write "unknown"
    f.write(package.get_attribute('value', 'unknown'))

    spice_common.write_list_of_attributes(
        f, package, ['l', 'w', 'ic'])
    # write the off attribute separately
    # add additional space. . . .
    f.write(' ')

    f.write('\n')

# The behavior of the voltage source is held in the "value" attribute.

def write_independent_voltage_source(f, package):
    debug_spew("Found independent voltage source.  "
               "Refdes = %s\n" % package.refdes)

    # first write out refdes and attached nets
    write_component_no_value(f, package)

    # next write voltage value, if any.  Note that if the
    # voltage value is not assigned, then it will write "unknown"
    f.write(package.get_attribute('value', 'unknown'))

    f.write('\n')

# The behavior of the current source is held in the "value" attribute.

def write_independent_current_source(f, package):
    debug_spew("Found independent current source.  "
               "Refdes = %s\n" % package.refdes)

    # first write out refdes and attached nets
    write_component_no_value(f, package)

    # next write current value, if any.  Note that if the
    # current value is not assigned, then it will write "unknown"
    f.write(package.get_attribute('value', 'unknown'))

    f.write('\n')

# Write Josephson junction in wrspice format.  Paul Bunyk, Sep 2, 2005

def write_josephson_junction(f, package):
    debug_spew("Found Josephson junction.  Refdes = %s\n" % package.refdes)

    # first write out refdes and attached nets
    write_component_no_value(f, package)

    # next, add a dummy node for JJ phase. Unlike in Xic netlister, give it
    # a reasonable name, not a number, e.g., refdes.
    f.write(package.refdes + ' ')

    # next write JJ model name, if any.
    model_name = package.get_attribute('model-name', None)
    if model_name is not None:
        f.write(model_name + ' ')

    # Next write out attribtes if they exist.  Use
    # a list of attributes which can be attached to a junction.
    spice_common.write_list_of_attributes(f, package, ['area'])
    # write the off attribute separately
    # add additional space. . . .
    f.write(' ')

    f.write('\n')

# Write mutual inductance (actually K).  Paul Bunyk, Sep 2, 2005

def write_coupling_coefficient(f, package):
    debug_spew("Found mutual inductance.  Refdes = %s\n" % package.refdes)

    # first write out refdes and attached nets (none)
    write_component_no_value(f, package)

    # next two inductor names and value
    inductors = package.get_attribute('inductors', None)
    value = package.get_attribute('value', None)
    if inductors is not None:
        f.write(inductors + ' ')
    if value is not None:
        f.write(value + ' ')

    f.write('\n')

# Write a voltage probe.

def write_probe(f, package):
    # fetch only one attr we care about, so far
    value = package.get_attribute('value', 'TRAN')

    debug_spew("Found Probe item, refdes = %s\n" % package.refdes)

    f.write('* Probe device %s on nets ' % packager.refdes)
    write_net_names_on_component(f, package)
    f.write('\n')
    f.write('.print %s +' % value)
    # make format string
    fmt = ' '.join(['V(%s)'] * len(package.pins))
    write_net_names_on_component(f, package, fmt)
    f.write('\n')

# Given a refdes, and optionally a format string, this writes out the
# nets attached to the component's pins.  If it's not called with a
# format string it looks for one in the net-format attribute,
# otherwise it writes out the pins unformatted.  This is used to write
# out non-slotted parts.

def write_net_names_on_component(f, package, format = None):
    netnames = []
    for i in range(1, len(package.pins) + 1):
        # Get net name for pinseq i and add it to netnames,
        # unless it is "ERROR_INVALID_PIN".

        # -------  Super debug stuff  --------
        if False:
            debug_spew("  In write_net_names_on_component. . . . \n")
            debug_spew("     pin-name = %s\n" % str(i))
            debug_spew("     pinnumber = %s\n" %
                package.get_pin_by_pinseq(i).get_attribute('pinnumber', 'unknown'))
            debug_spew("     pinseq = %s" %
                package.get_pin_by_pinseq(i).get_attribute('pinseq', 'unknown'))
            if str(i) != package.get_pin_by_pinseq(i).get_attribute('pinseq', 'unknown'):
                debug_spew(" <== INCONSISTENT!\n")
            else:
                debug_spew("\n")
            debug_spew("     netname = %s\n",
                spice_common.get_net_name(package,
                    package.get_pin_by_pinseq(i).get_attribute('pinnumber', 'unknown')))
        # -------------------------------------

        try:
            pin = package.get_pin_by_pinseq(i)
        except KeyError:
            debug_spew("For %s, found pin with no pinseq attribute.  "
                       "Ignoring. . . .\n" % package.refdes)
        else:
            netnames.append(spice_common.get_net_name(package, pin.number))

    # Format agument take priority, otherwise use attribute
    if format is None:
        format = package.get_attribute('net-format', None)

    if format is None:
        # write out nets.
        f.write(''.join(netname + ' ' for netname in netnames))
    else:
        # write out nets with format string
        f.write(format % netnames)

# Write the refdes and the net names connected to pins on this
# component.  No return, and no component value is written, or extra
# attribs.  Those are handled later.

def write_component_no_value(f, package):
    f.write(package.refdes + ' ')
    # write component refdes
    write_net_names_on_component(f, package)

# Given a refdes, returns the device attribute "value" as string.
# Used when "value" is an optional attribute.  Returns "unknown" if
# not available.

def component_optional_value(package):
    value = package.get_attribute('value', None)
    if value is None:
        return ''
    return value + ' '

# Given a refdes, returns the device attribute "model" as string.

def component_model(package):
    # This returns either a string or a function!?
    model = package.get_attribute('model', None)
    if model is None:
        return spice_common.component_value  # TODO: looks wrong
    return model

# Include SPICE statements from a SPICE directive block.

def write_directive(f, package):
    # Collect variables used in creating spice code
    value = package.get_attribute('value', None)
    file = package.get_attribute('file', None)

    debug_spew("Found SPICE directive box.  Refdes = %s\n" % package.refdes)

    # First look to see if there is a value.
    if value is not None:
        f.write(value + '\n')
        debug_spew("Appending value = \"%s\" to output file.\n" % value)
    # since there is no value, look for file.
    elif file is not None:
        insert_text_file(f, file)
        # Note that we don't wait until the end here.  Is that OK?
        debug_spew("Inserting contents of file = %s into output file.\n" % file)

# Include a file using an .INCLUDE directive
# Changed on 2005-06-12: to embed the contents of the file,
# you must call gnetlist with the -e flag set.

def write_include(f, package):
    file = package.get_attribute('file', None)
    debug_spew("Found SPICE include box.  Refdes = %s\n" % package.refdes)

    if file is None:
        debug_spew("silently skip \"unknown\" file.\n")
        return

    if 'embedd_mode' in calling_flags:
        # -e found: invoke insert_text_file
        insert_text_file(f, file)
        debug_spew("embedding contents of file %s into netlist.\n" % file)
    else:
        # -e not found: just print out .INCLUDE card
        f.write('.INCLUDE %s\n' % file)
        debug_spew("placing .include directive string into netlist.\n")

# Include an option using an .OPTIONS directive.

def write_options(f, package):
    debug_spew("Found .OPTIONS box.  Refdes = %s\n" % package.refdes)
    f.write('.OPTIONS %s\n' % spice_common.component_value(package))

# Include a spice model (instantiated as a model box on the schematic).
# Two types of model can be included:
#  1.  An embedded model, which is a one- or multi-line string held in
#      the attribute "model".
#      In this case, the following attributes are mandatory:
#      --  model (i.e. list of parameter=value strings)
#      --  model-name
#      --  type
#      In this case, the function creates and formats the correct
#      spice model line(s).
#  2.  A model held in a file whose name is held in the attribute "file"
#      In this case, the following attribute are mandatory:
#      --  file (i.e. list of parameter=value strings)
#      In this case, the function just opens the file and dumps the
#      contents into the netlist.

def write_model(f, package):
    # Collect variables used in creating spice code
    model_name = package.get_attribute('model-name', None)
    model_file = package.get_attribute('file', None)
    model = package.get_attribute('model', None)

    debug_spew("Found .MODEL box.  Refdes = %s\n" % package.refdes)

    # Now, depending upon what combination of model, model_file, and
    # model_name exist (as described above) write out lines into spice
    # netlist.
    if model is not None and model_name is not None:
        # one model and model name exist
        debug_spew("found model and model_name for %s\n" % package.refdes)
        f.write('.MODEL %s %s (%s)\n' % (
            model_name, package.get_attribute('type', 'unknown'), model))
    elif model_file is not None:
        # model file exists
        debug_spew("found model_file for %s\n" % package.refdes)
        # insert_text_file(f, model_file)
        # don't write it out -- it's handled after the second pass.

#  This writes out the default component (i.e. the "device" attribute
#  was not recognized).  This function does the following:
#
#  1.  Gets the refdes (package).
#  2.  Checks the refdes against a short list of possible values.
#      Depending upon the refdes, it does the following thing:
#      A? -- Invokes write-ic. This provides the opportunity for a
#            code model which may include a .model line.
#      D? -- Invokes write-diode
#      Q? -- Invokes write-transistor-diode. (The "type" attribute is
#            <unknown> in this case so that the spice simulator will
#            barf if the user has been careless.)
#      M? -- Same as Q
#      U? -- Invokes write-ic. This provides the opportunity for a
#            component model to be instantiated.
#      X? -- Invokes write-ic.  This provides the opportunity for a
#            component subcircuit to be instantiated.
#      V? -- Invokes write-independent-voltage-source
#      I? -- Invokes write-independent-current-source
#      Otherwise, it just outputs the refdes, the attached nets, and
#      the value of the "value" attribute.

def write_default_component(f, package, file_info_list):
    # extract first char of refdes.
    first_char = package.refdes[0]

    if first_char == 'A':
        write_ic(f, package, file_info_list)
    elif first_char == 'D':
        write_diode(f, package)
    elif first_char == 'Q' or first_char == 'M':
        write_transistor_diode(f, package, False, '<unknown>', [])
    elif first_char == 'U':
        write_ic(f, package, file_info_list)
    elif first_char == 'V':
        write_independent_voltage_source(f, package)
    elif first_char == 'I':
        write_independent_current_source(f, package)
    elif first_char == 'X':
        write_ic(f, package, file_info_list)
    else:
        package.warn("unknown component")
        write_component_no_value(f, package)
        # write component value, if components have a label "value=#"
        # what if a component has no value label, currently unknown is written
        f.write(spice_common.component_value(package))
        f.write('\n')


################# High-level functions for program control #################

# This function is passed a list of refdesses (ls).  It uses each
# refdes to get the corresponding "device" attribute.  Depending upon
# the device, it then invokes one or another of the spice line output
# fcns to output a line of the spice netlist.  I have enlarged the
# number of devices it recognizes -- SDB.
# Write the refdes, to the pin# connected net and component value and
# optional extra attributes.  Check if the component is a special
# spice component.

def write_netlist(f, file_info_list, ls):
    for package in ls:
        device = package.get_attribute('device', 'unknown')

        # Super debug stuff -- outputs line describing device being processed.
        debug_spew("--- checking package = %s\n" % package.refdes)
        debug_spew("    device = %s\n" % device)

        if device == 'none':
            pass  # do nothing for graphical symbols.
        elif device == 'spice-subcircuit-LL':
            pass  # do nothing for subcircuit declaration.
        elif device == 'spice-IO':
            pass # do nothing for SPICE IO pins.
        elif device == 'SPICE-ccvs':
            spice_common.write_ccvs(f, package)
        elif device == 'SPICE-cccs':
            spice_common.write_cccs(f, package)
        elif device == 'SPICE-vcvs':
            spice_common.write_vcvs(f, package)
        elif device == 'SPICE-vccs':
            spice_common.write_vccs(f, package)
        elif device == 'SPICE-nullor':
            spice_common.write_nullor(f, package)
        elif device == 'DIODE':
            write_diode(f, package)
        elif device == 'PMOS_TRANSISTOR':
            write_pmos_transistor(f, package)
        elif device == 'NMOS_TRANSISTOR':
            write_nmos_transistor(f, package)
        elif device == 'PNP_TRANSISTOR':
            write_pnp_bipolar_transistor(f, package)
        elif device == 'SPICE-PNP':
            write_pnp_bipolar_transistor(f, package)
        elif device == 'NPN_TRANSISTOR':
            write_npn_bipolar_transistor(f, package)
        elif device == 'SPICE-NPN':
            write_npn_bipolar_transistor(f, package)
        elif device == 'PFET_TRANSISTOR':
            write_pfet_transistor(f, package)
        elif device == 'NFET_TRANSISTOR':
            write_nfet_transistor(f, package)
        elif device == 'MESFET_TRANSISTOR':
            write_mesfet_transistor(f, package)
        elif device == 'SPICE-VC-switch':
            write_vc_switch(f, package)
        elif device == 'RESISTOR':
            write_resistor(f, package)
        elif device == 'CAPACITOR':
            write_capacitor(f, package)
        elif device == 'POLARIZED_CAPACITOR':
            write_capacitor(f, package)  # change someday
        elif device == 'INDUCTOR':
            write_inductor(f, package)
        elif device == 'COIL':
            # Added to enable netlisting of coil-*.sym
            write_inductor(f, package)
        elif device == 'VOLTAGE_SOURCE':
            write_independent_voltage_source(f, package)
            # change someday
        elif device == 'CURRENT_SOURCE':
            write_independent_current_source(f, package)
            # change someday
        elif device == 'JOSEPHSON_JUNCTION':
            write_josephson_junction(f, package)
        elif device == 'K':
            write_coupling_coefficient(f, package)
        elif device == 'model':
            write_model(f, package)
        elif device == 'options':
            write_options(f, package)
        elif device == 'directive':
            write_directive(f, package)
        elif device == 'include':
            write_include(f, package)
        elif device == 'TESTPOINT':
            write_probe(f, package)
        elif device == 'SUBCKT_PMOS':
            write_subckt_pmos_transistor(f, package)
        elif device == 'SUBCKT_NMOS':
            write_subckt_nmos_transistor(f, package)
        else:
            write_default_component(f, package, file_info_list)

# This runs through the list of packages (refdesses), and for each
# gets the attributes.  If there is a "FILE" attribute, it gets the
# file info & uses it to build the file-info-list.  When done, it
# returns the file_info_list.

def create_file_info_list(netlist):
    file_info_list = []
    for package in reversed(netlist.packages):
        model = package.get_attribute('model-name', 'unknown')
        model_file = package.get_attribute('file', None)

        # Now run a series of checks to see if we should stick this
        # file into the file_info_list

        # Check to see if "file" attribute is non-empty
        if model_file is None:
            continue

        debug_spew("found file attribute for %s.  File name = %s\n"
                   % (package.refdes, model_file))

        # Now check to see if file is in file_info_list
        if is_in_file_info_list(model_file, file_info_list):
            #  File is already in list.  Print debug spew if desired.
            debug_spew("File has already been seen and entered into "
                       "known model file list.\n")
            continue

        # File is new.  Open file, find out what type it is, and push
        # info into file_info_list
        file_type = get_file_type(model_file)
        debug_spew("File is new.  New file type is %s \n" % file_type)

        # Check to see if file_type is known.
        if file_type == 'OTHER':
            # File type is not a model type.  Don't stick it in list.
            # Print debug spew if desired.
            debug_spew("File type is OTHER, and therefore will not be "
                       "entered in known model file list.\n")
            continue

        # file_type is OK.  Return file_info_list with new triplet attached.
        debug_spew("Inserting %s into list of known model files.\n"
                   % model_file)
        file_info_list.append((model, model_file, file_type))

    # end of packages processed.  Return file_info_list
    file_info_list.reverse()
    return file_info_list

# Helper function.  Returns True if file is already in file_info_list,
# otherwise False.  Assumes file_info_list of form:
#   [(model1, file1, file_type1), (model2, file2, file_type2), ...]

def is_in_file_info_list(model_file, file_info_list):
    for list_element in file_info_list:
        list_file_name = list_element[1]
        if list_file_name == model_file:
            return True  # item found

    return False


# Write out spice netlist header

def write_top_header(f):
    f.write('*********************************************************\n')
    f.write('* Spice file generated by gnetlist                      *\n')
    f.write('* spice-sdb version 4.28.2007 by SDB --                 *\n')
    f.write('* provides advanced spice netlisting capability.        *\n')
    f.write('* Documentation at http://www.brorson.com/gEDA/SPICE/   *\n')
    f.write('*********************************************************\n')

# Write out .SUBCKT netlist header

def write_subcircuit_header(f):
    f.write('*******************************\n')
    f.write('* Begin .SUBCKT model         *\n')
    f.write('* spice-sdb ver 4.28.2007     *\n')
    f.write('*******************************\n')

# Spice netlist generation.
# This is the entry point.
#
# Hacked on 2003-03-31 to enable writing out .SUBCKT models -- SDB.
# Hacked again in Sept 2003 to enable more intelligent embedding of
# external SPICE files into netlist -- SDB.
# The algorithm is as follows:
# 1.  Figure out if there is a .SUBCKT block on the schematic,
#     or if it is just a normal schematic.
#     If a .SUBCKT:
#     -- Write out subcircuit header (a comment identifying the netlister).
#     -- find all spice-IO pins.  Get a list of the packages.
#     -- put them in order (ordered by package refdes)
#     -- get the list of nets attached to the spice-IO pins.
#     -- write out .SUBCKT line
#     If a normal schematic:
#     -- Write out top header (a comment identifying the netlister).
# 2.  Loop through all components, looking for components with a "file"
#     attribute.  Every time a "file" attribute is found do this:
#     --  Open the file and find out what kind of file it is (.SUBCKT
#         or .MODEL).
#     --  Determine if the file has previously been processed.  If
#         not: stick the following info into the file-info list:
#         (model_name, file_name, file_type).  Otherwise just continue.
# 3.  Loop through all components again, and write out a SPICE card for each.
# 4.  Afterwards, for each item in the file-info list, open the file,
#     and write its contents into the netlist.
# 5.  If the schematic-type is .SUBCKT: write out .ENDS, otherwise
#     write out .END
# 6.  Close up the SPICE netlist file and return.

def run(f, netlist, args):
    # Parse backend options passed via the `-O' command-line option.
    global calling_flags
    calling_flags = backend_getopt(args, {
        'include_mode': Option(False, NO_ARGUMENT, None),
        'nomunge_mode': Option(False, NO_ARGUMENT, None),
        'sort_mode': Option(False, NO_ARGUMENT, None),
        'embedd_mode': Option(False, NO_ARGUMENT, None),
        'no_end_card': Option(False, NO_ARGUMENT, None)
    })

    # Redefine write_net_names_on_component
    spice_common.write_net_names_on_component = write_net_names_on_component

    # First find out if this is a .SUBCKT lower level,
    # or if it is a regular schematic.

    schematic_type = get_schematic_type(netlist)
    model_name = get_subcircuit_modelname(schematic_type)

    sys.stderr.write("Using SPICE backend by SDB -- Version of 2007-04-28\n")
    sys.stderr.write("schematic_type = %s\n" % schematic_type)

    if schematic_type != 'normal schematic':
        # we have found a .SUBCKT type schematic.
        io_pin_packages = get_spice_IO_pins(netlist)
        io_pin_packages_ordered = sort_spice_IO_pins(io_pin_packages)
        io_nets_list = get_IO_nets(io_pin_packages_ordered)
        debug_spew("found .SUBCKT type schematic\n")

        # now write out .SUBCKT header and .SUBCKT line
        write_subcircuit_header(f)
        f.write('%s %s \n' % (schematic_type,
                              ' '.join(reversed(io_nets_list))))
    else:
        # Otherwise it's a regular schematic.  Write out command line
        # followed by comments in file header.
        debug_spew("found normal type schematic\n")
        f.write('* %s\n' % ' '.join(sys.argv))
        write_top_header(f)

    # Now loop through all devices and process all "FILE" attributes.  Create
    # file_info_list.
    # Thanks to Carlos Nieves Onega for his e-mail to
    # geda-dev which is the genesis of this section.
    #
    debug_spew("Make first pass through design and "
               "create list of all model files referenced.\n")
    file_info_list = create_file_info_list(netlist)
    debug_spew("Done creating file_info_list.\n\n")

    # Moved this loop before the next one to get numparam to work with
    # ngspice, because numparam will at the subckt definition come
    # before the main netlist.  Change suggested by Dominique Michel;
    # implemented in code on 2005-06-12.
    #
    # Next loop through all items in file_info_list in the SPICE
    # netlist.  For each model_name, open up the corresponding file,
    # and call handle_spice_file to stick the corresponding stuff into
    # the output SPICE file.
    #
    debug_spew("Now process the items in model file list -- "
               "stick appropriate references to models in output SPICE file.\n")
    loop_through_files(f, file_info_list)
    debug_spew("Done processing items in model file list.\n")

    # Now write out netlist as before.  But don't write file contents out.
    # **** Modified by kh to sort list of packages so Spice
    # **** directives, etc. (A?) are output last, and in increasing
    # **** order.
    #
    debug_spew("Make second pass through design and "
               "write out a SPICE card for each component found.\n")
    f.write(
        '*==============  Begin SPICE netlist of main design ============\n')
    if 'sort_mode' in calling_flags:
        # sort on refdes
        write_netlist(f, file_info_list, sorted(
            netlist.packages, cmp = packsort))
    else:
        # don't sort.
        write_netlist(f, file_info_list, reversed(netlist.packages))
    debug_spew("Done writing SPICE cards . . .\n\n")

    # Now write out .END(S) of netlist, depending upon whether this
    # schematic is a "normal schematic" or a .SUBCKT.
    #
    if schematic_type != 'normal schematic':
        f.write('.ends %s\n' % model_name)
        f.write('*******************************\n')
    elif 'no_end_card' not in calling_flags:
        f.write('.end\n')

    debug_spew("\nOutput file is written.  We are done.\n")
