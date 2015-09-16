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

# Common functions for the SPICE netlist backends `spice' and `spice-sdb'.
# By S. Gieltjes and others.

# write netnames connected to pin_a and pin_b
#   (currently used by the controlled sources (e, g, f and h)

def write_two_pin_names(f, package, pin_a, pin_b):
    f.write('%s %s ' % (
        get_net_name(package, package.get_pin_by_pinseq(pin_a).number),
        get_net_name(package, package.get_pin_by_pinseq(pin_b).number)))

# Given a refdes returns the device associated nets(s) ordered by
# their pin number,
# what when not defined?
#      problem is slotted components e.g. ../examples/singlenet_1.sch

def write_net_names_on_component(f, package):
    for i in xrange(len(package.pins)):
        try:
            pin = package.get_pin_by_pinseq(i + 1)
        except KeyError:
            f.write("ERROR_INVALID_PIN ")
        else:
            f.write(get_net_name(package, pin.number) + " ")

# write a current controlled voltage source and implement the necessary
#   current measuring voltage source

def write_ccvs(f, package):
    f.write("* begin ccvs expansion, h<name>\n")
    # implement the controlled current source
    # the user should create the refdes label begining with a h
    f.write(package.refdes + " ")
    write_two_pin_names(f, package, 1, 2)
    f.write("Vsense_%s %s\n" % (package.refdes,
                                component_value(package)))
    # implement the current measuring voltage source
    f.write("Vsense_%s " % package.refdes)
    write_two_pin_names(f, package, 3, 4)
    f.write("dc 0\n")
    # now it is possible to leave the output voltage source unconnected
    # i.e. spice won't complain about unconnected nodes
    f.write("IOut_%s " % package.refdes)
    write_two_pin_names(f, package, 1, 2)
    f.write("dc 0\n")
    f.write("* end ccvs expansion\n")

# write a current controlled current source and implement the necessary
#   current measuring voltage source

def write_cccs(f, package):
    f.write("* begin cccs expansion, f<name>\n")
    # implement the controlled current source
    # the user should create the refdes label begining with a f
    f.write(package.refdes + " ")
    write_two_pin_names(f, package, 1, 2)
    f.write("Vsense_%s %s\n" % (package.refdes,
                                package.get_attribute('value', 'unknown')))
    # implement the current measuring voltage source
    f.write("Vsense_%s " % package.refdes)
    write_two_pin_names(f, package, 3, 4)
    f.write("dc 0\n")
    f.write("* end cccs expansion\n")

# write a voltage controlled current source and implement the necessary
#   voltage measuring current source

def write_vccs(f, package):
    f.write("* begin vccs expansion, g<name>\n")
    # implement the controlled current source
    # the user should create a refdes label beginning with a g
    f.write(package.refdes + " ")
    write_net_names_on_component(f, package)
    f.write(component_value(package) + "\n")
    # implement the voltage measuring current source
    # imagine yourself copying the voltage of a voltage source with an
    # internal impedance, spice starts complaining about unconnected
    # nets if this current source is not here.
    f.write("IMeasure_%s " % package.refdes)
    write_two_pin_names(f, package, 3, 4)
    f.write("dc 0\n")
    f.write("* end vccs expansion\n")

# write a voltage controlled voltage source and implement the necessary
#   voltage measuring current source

def write_vcvs(f, package):
    f.write("* begin vcvs expansion, e<name>\n")
    # implement the controlled voltage source
    # the user should create a refdes label beginning with an e
    f.write(package.refdes + " ")
    write_net_names_on_component(f, package)
    f.write(package.get_attribute('value', 'unknown') + "\n")
    # implement the voltage measuring current source
    # imagine yourself copying the voltage of a voltage source with an
    # internal impedance, spice starts complaining about unconnected
    # nets if this current source is not here.
    f.write("Isense_%s " % package.refdes)
    write_two_pin_names(f, package, 3, 4)
    f.write("dc 0\n")
    # with an output current source it is possible to leave the output
    # voltage source unconnected
    # i.e. spice won't complain about unconnected nodes
    f.write("IOut_%s " % package.refdes)
    write_two_pin_names(f, package, 1, 2)
    f.write("dc 0\n")
    f.write("* end vcvs expansion\n")

# Create a nullor, make sure it consists of a voltage controlled source

def write_nullor(f, package):
    value = package.get_attribute('value', 'unknown')
    f.write("* begin nullor expansion, e<name>\n")
    # implement the controlled voltage source
    f.write("E_%s " % package.refdes)
    write_net_names_on_component(f, package)
    if value == 'unknown':
        value = '1000Meg'
    f.write(value + "\n")
    # implement the voltage measuring current source
    # imagine yourself copying the voltage of a voltage source with an
    # internal impedance, spice starts complaining about unconnected
    # nets if this current source is not here.
    f.write("IMeasure_%s " % package.refdes)
    write_two_pin_names(f, package, 3, 4)
    f.write("dc 0\n")
    # with an output current source it is possible to leave the output
    # voltage source unconnected
    # i.e. spice won't complain about unconnected nodes
    f.write("IOut_%s " % package.refdes)
    write_two_pin_names(f, package, 1, 2)
    f.write("dc 0\n")
    f.write("* end of nullor expansion\n")

# write all listed and available attributes in the form of <variable>=<value>

def write_list_of_attributes(f, package, attrib_list):
    for attrib_name in attrib_list:
        attrib = package.get_attribute(attrib_name, 'unknown')
        # Is it possible to make no differentiation between upper and
        # lower case?  That relieves you of mixed case forms e.g. As,
        # AS, as..., they are the same attributes, spice3f5 is case
        # insensitive.  And other spice versions?
        if attrib != 'unknown':
            f.write(" %s=%s" % (attrib_name, attrib))

# Given a refdes, returns the device attribute "value" as string
# Used when "value" is a mandatory attribute.
# Returns "<no valid attribute . . .>" if not available.

def component_value(package):
    return package.get_attribute('value', '<No valid value attribute found>')

# gnet_spice replacement of package.get_net_name,
# a net labeled "GND" becomes 0

def get_net_name(package, pin_name):
    if pin_name not in package.pins_by_number:
        return 'ERROR_INVALID_PIN'

    net = package.pins_by_number[pin_name].net

    if net.name == 'GND':
        return '0'
    if net.unnamed_counter is not None:
        return '%d' % net.unnamed_counter
    return net.name
