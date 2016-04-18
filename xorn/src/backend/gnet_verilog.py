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

# Verilog netlist backend written by Mike Jarabek
# 14 July 2011 - VERY basic concatenation support added by Frank Thomson

import collections, re, sys

# some useful regexes for working with net names

id_regexp = '[a-zA-Z_][a-zA-Z_\d$]*'

# match on a verilog concatened net like: {netname[x:y],othernet}
# Will not expand a bus for replication so
concat_bus_reg = re.compile('^\s*\{.*\}')

# match on a verilog identifier like: netname[x:y]
bit_range_reg = re.compile('^(' + id_regexp + ')\s*'
                           '\[\s*(\d+)\s*:\s*(\d+)\s*\]')

# match on a verilog identifier like: netname[x]
single_bit_reg = re.compile('^(' + id_regexp + ')\s*'
                            '\[\s*(\d+)\s*\]')

# match on a verilog identifier like: netname
simple_id_reg = re.compile('^(' + id_regexp + ')$')

# return a list of nets whose pins have the desired attribute
# name/value pair

def get_matching_nets(netlist, attribute, value):
    return [package.pins[-1].net.name
            for package in reversed(netlist.packages)
            if package.get_attribute(attribute, 'unknown') == value]

## Structure which describes a net.
#
# netname            name of the wire
# N1                 first limit
# N2                 second limit
# Increasing_order   whether N2 > N1
# sure               whether we are sure about the order

Netinfo = collections.namedtuple('Netinfo', [
    'netname', 'n1', 'n2', 'increasing', 'sure', 'real'])

# Take a netname and parse it into a structure that describes the net.

def net_parse(netname):
    # check over each expression type, and build the appropriate result

    # is it a bit range?
    match = bit_range_reg.match(netname)
    if match is not None:
        n1 = int(match.group(2))
        n2 = int(match.group(3))
        return Netinfo(match.group(1), n1, n2, n2 > n1, True, netname)

    # just a single bit?
    match = single_bit_reg.match(netname)
    if match is not None:
        n1 = int(match.group(2))
        return Netinfo(match.group(1), n1, n1, False, False, netname)

    # or a net without anything
    match = simple_id_reg.match(netname)
    if match is not None:
        return Netinfo(match.group(1), 0, 0, False, False, netname)

    sys.stderr.write("Warning: `%s' is not likely "
                     "a valid Verilog identifier\n" % netname)
    return Netinfo(netname, 0, 0, False, False, netname)

# Return whether the passed name is something that might pass as a
# verilog identifier.

def is_identifier(netname):
    # check over each expression type, return result
    return bit_range_reg.match(netname) is not None or \
           concat_bus_reg.match(netname) is not None or \
           single_bit_reg.match(netname) is not None or \
           simple_id_reg.match(netname) is not None

# Display a verilog identifier that is escaped if needed.

def escaped_identifier(netname):
    if not is_identifier(netname):
        # need to escape
        return '\\%s ' % netname

    # just display the identifier
    return netname

# return just the netname part of a verilog identifier
def get_netname(netname):
    return net_parse(netname).netname

# Display wires from the design
#
# Display a net in a legal verilog format, based on the object passed

def display_wire(wire):
    # figure out if we need a bit range
    name = wire.netname
    n1 = wire.n1
    n2 = wire.n2

    if n1 != 0 or n2 != 0:
        # yes, print it
        return '[ %s : %s ] ' % (n1, n2) + escaped_identifier(name)

    # print the wire name
    return escaped_identifier(name)


# Highest level function
# Write Structural verilog representation of the schematic

def run(f, netlist):
    # Build a dictionary of net description objects by working over
    # the list of `unique' nets in the design, extracting names, and
    # bit ranges, if appropriate.

    nets = {}
    net_names = []
    for net in reversed(netlist.nets):
        # parse the netname, and see if it is already on the list
        parsed = net_parse(net.name)
        try:
            listed = nets[parsed.netname]
        except KeyError:
            # it is not, just add it
            nets[parsed.netname] = parsed
            net_names.append(parsed.netname)
            continue

        # it is, do some checks, and update the record
        consistant = listed.increasing == parsed.increasing

        if listed.sure and consistant:
            sure = True
        elif listed.sure and not parsed.sure \
                and parsed.n1 == 0 and parsed.n2 == 0:
            # this is a net without any expression, leave it
            continue
        elif listed.sure and not consistant:
            # order is inconsistent
            sys.stderr.write(
                "Warning: Net `%s' has a bit order that conflicts with the "
                         "original definition of `%s', ignoring `%s'\n"
                % (parsed.real, listed.real, parsed.real))
            continue
        elif not listed.sure and parsed.sure and consistant:
            sure = True
        elif not listed.sure and parsed.sure and not consistant:
            listed.n1, listed.n2 = listed.n2, listed.n1
            sure = True
        elif not listed.sure and not parsed.sure:
            sure = False
        else:
            assert False  # This should never happen!

        # Update the given bit range with data passed.  Take care of
        # ordering issues.
        if parsed.increasing:
            # originally increasing
            rn1 = min(parsed.n1, listed.n1)
            rn2 = max(parsed.n2, listed.n2)
        else:
            # originally decreasing
            rn1 = max(parsed.n1, listed.n1)
            rn2 = min(parsed.n2, listed.n2)

        # update record with the given parameters
        nets[parsed.netname] = Netinfo(
            listed.netname, rn1, rn2, parsed.increasing, sure, parsed.real)


    # write top level header
    f.write('/* structural Verilog generated by gnetlist */\n')
    f.write('/* WARNING: This is a generated file, edits */\n')
    f.write('/*        made here will be lost next time  */\n')
    f.write('/*        you run gnetlist!                 */\n')
    f.write('/* Id ..........$Id$ */\n')
    f.write('/* Source.......$Source$ */\n')
    f.write('/* Revision.....$Revision$ */\n')
    f.write('/* Author.......$Author$ */\n')
    f.write('\n')

    # return the top level block name for the module
    module_name = netlist.get_toplevel_attribute('module_name', 'not found')

    # build port lists
    #
    # Scan through the list of components, and pins from each one,
    # finding the pins that have PINTYPE == CHIPIN, CHIPOUT, CHIPTRI
    # (for inout).  Build three lists, one each for the inputs,
    # outputs and inouts.

    in_ports = get_matching_nets(netlist, 'device', 'IPAD')
    out_ports = get_matching_nets(netlist, 'device', 'OPAD')
    inout_ports = get_matching_nets(netlist, 'device', 'IOPAD')

    # write module declaration
    # output the meat of the module port section
    #
    # each line in the declaration is formatted like this:
    #
    #       PORTNAME , <newline>

    f.write('module %s (\n' % escaped_identifier(module_name))
    # loop over input/output pins, but take care of last comma
    for i, netname in enumerate(in_ports + out_ports + inout_ports):
        if i != 0:
            f.write(' ,\n')
        f.write('       ')
        f.write(get_netname(netname))
    f.write('\n')
    f.write('      );\n')
    f.write('\n')

    # output the module direction section
    f.write('/* Port directions begin here */\n')
    # do each input, output, and inout
    for netname in in_ports:
        f.write('input %s ;\n' % display_wire(nets[get_netname(netname)]))
    for netname in out_ports:
        f.write('output %s ;\n' % display_wire(nets[get_netname(netname)]))
    for netname in inout_ports:
        f.write('inout %s ;\n' % display_wire(nets[get_netname(netname)]))
    f.write('\n')
    f.write('\n')

    # Loop over the list of nets in the design, writing one by one
    f.write('/* Wires from the design */\n')
    # print a wire statement for each
    for wire in net_names:
        # if it does not appear to be a concatenated bus then print it
        if not concat_bus_reg.match(nets[wire].real):
            # net in the design
            f.write('wire %s ;\n' % display_wire(nets[wire]))
    f.write('\n')

    # Output any continuous assignment statements generated
    # by placing `high' and `low' components on the board
    f.write('/* continuous assignments */\n')
    # TODO: fixme, multiple bit widths!
    for wire in get_matching_nets(netlist, 'device', 'HIGH'):
        f.write("assign %s = 1'b1;\n" % escaped_identifier(wire))
    for wire in get_matching_nets(netlist, 'device', 'LOW'):
        f.write("assign %s = 1'b0;\n" % escaped_identifier(wire))
    f.write('\n')

    # Output a compoment instatantiation for each of the
    # components on the board
    #
    # use the format:
    #
    #  device-attribute refdes (
    #        .pinname ( net_name ),
    #        ...
    #    );

    f.write('/* Package instantiations */\n')
    # loop on packages
    for package in reversed(netlist.packages):
        device = package.get_attribute('device', 'unknown')
        # ignore specials
        if device in ['IOPAD', 'IPAD', 'OPAD', 'HIGH', 'LOW']:
            continue

        f.write('%s %s ( ' % (escaped_identifier(device),
                              escaped_identifier(package.refdes)))
        # if this module wants positional pins,
        # then output that format, otherwise
        # output normal named declaration
        positional = package.get_attribute(
            'VERILOG_PORTS', None) == 'POSITIONAL'

        # display connections
        # output a module connection for the package with named ports
        comma_pending = False

        if package.pins:
            f.write('\n')

        for pin in package.pins:
            if pin.net.is_unconnected_pin:
                continue

            # handle commas after the first pin
            if comma_pending:
                f.write(',\n')
            else:
                comma_pending = True

            # Display the individual net connections
            #  in this format if positional is true:
            #
            #    /* PINNAME */ NETNAME
            #
            #  otherwise emit:
            #
            #      .PINNAME ( NETNAME )

            if positional:
                # output a positional port instance
                # add in name for debugging
                f.write('  /* %s */ %s' % (pin.number, pin.net.name))
            else:
                # output a named port instance
                # Display the escaped version of the identifier
                f.write('    .%s ( %s )' % (
                    escaped_identifier(get_netname(pin.number)),
                    escaped_identifier(pin.net.name)))

        if package.pins:
            f.write('\n')

        f.write('    );\n')
        f.write('\n')

    # write bottom footer for file
    f.write('endmodule\n')
