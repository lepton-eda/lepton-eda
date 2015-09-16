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

# Various support functions shamelessly stolen from the verilog code
# and reshaped for vhdl.  Doing this now saves labour when the
# implementations starts to divert further.

# Get port list of top-level Entity
# THHE changed this to the urefs of the I/O-PAD symbols rather than
# the net names.  So the uref of the I/O port will become the port
# name in the VHDLport clause.

# THHE
#
# Since VHDL know about port directions, pins need a additional
# attribute.  The code assumes the attribute "type" (IN, OUT, INOUT)
# on each pin of a symbol.  In addition you can add the attribute
# "width" for a very simple definition of busses. (Not complete yet!)

def get_top_port_list(netlist):
    # construct list
    return (
        get_matching_urefs(netlist, 'device', 'IPAD'),
        get_matching_urefs(netlist, 'device', 'OPAD'),
        get_matching_urefs(netlist, 'device', 'IOPAD'))

def get_matching_urefs(netlist, attribute, value):
    return [(package.refdes, package.get_attribute('width', 'unknown'))
            for package in reversed(netlist.packages)
            if package.get_attribute(attribute, 'unknown') == value]


# Port Clause
#
# According to IEEE 1076-1993 1.1.1:
#
# entity_header :=
#  [ formal_generic_clause ]
#  [ formal_port_clause ]
#
# generic_clause :=
#    GENERIC ( generic_list ) ;
#
# port_clause :=
#    PORT ( port_list ) ;
#
# According to IEEE 1076-1993 1.1.1.2:
#
# port_list := port_interface_list
#
# According to IEEE 1076-1993 4.3.2.1:
#
# interface_list := interface_element { ; interface_element }
#
# interface_element := interface_declaration
#
# According to IEEE 1076-1993 4.3.2:
#
# interface_declaration :=
#    interface_constant_declaration
#  | interface_signal_declaration
#  | interface_variable_declaration
#  | interface_file_declaration
#
# interface_signal_declaration :=
#  [ SIGNAL ] identifier_list : [ mode ] subtype_indication [ BUS ]
#  [ := static_expression ]
#
# mode := IN | OUT | INOUT | BUFFER | LINKAGE
#
# Implementation note:
#    Since the port list must contain signals will only the interface
#    signal declaration of the interface declaration be valid. Further,
#    we may safely assume that the SIGNAL symbol will not be needed.
#    The identifier list is reduced to a signle name entry, mode is set
#    to in, out or inout due to which part of the port list it comes from.
#    The mode types supported are in, out and inout where as buffer and
#    linkage mode is not supported. The subtype indication is currently
#    hardwired to standard logic, but should be controlled by attribute.
#    There is currently no support for busses and thus is the BUS symbol
#    no being applied. Also, there is currently no static expression
#    support, this too may be conveyed using attributes.
#

# This little routine writes a single pin on the port clause.
# It assumes a list containing (portname, mode, type) such as
# (CLK in Std_Logic width).
#
# THHE If you added a attribute width=n to a pin or to a I/O-PAD, you get
#      portname : IN Std_Logic_Vector(width-1 downto 0)

def write_port(f, port):
    f.write('%s : %s %s' % (port[0], port[1], port[2]))
    if port[3] != 'unknown':
        f.write('_Vector(%d downto 0)' % (int(port[3]) - 1))

# This little routine will actually write the full port clause given a list
# of pins, such as ((CLK in Std_Logic) (D in Std_Logic) (Q out Std_Logic))

def write_port_list(f, port_list):
    if not port_list:
        return

    f.write('    PORT (\n')
    for i, port in enumerate(port_list):
        if i != 0:
            f.write(';\n')
        f.write('        ')
        write_port(f, port)
    f.write(');\n')

# This is the real thing. It will take a port-list arrangement.
#
# These lists will be transformed into a single list containing the full
# pin information. Currently is this done with hardwired to Std_Logic.

def write_port_clause(f, (in_ports, out_ports, inout_ports)):
    write_port_list(f,
        [(a, 'in', 'Std_Logic', b) for a, b in in_ports] +
        [(a, 'out', 'Std_Logic', b) for a, b in out_ports] +
        [(a, 'inout', 'Std_Logic', b) for a, b in inout_ports])

# Primary unit
#
# According to IEEE 1076-1993 11.1:
#
# primary_unit :=
#    entity_declaration
#  | configuration_declaration
#  | package_declaration
#
# Implementation note:
#    We assume that gEDA does not generate either a configuration or
#    package declaration. Thus, only a entity declaration will be generated.
#
# According to IEEE 1076-1993 1.1:
#
# entity_declaration :=
#    ENTITY identifier IS
#       entity_header
#       entity_declarative_part
#  [ BEGIN
#       entity_statement_part ]
#    END [ ENTITY ] [ entity_simple_name ] ;
#
# Implementation note:
#    We assume that no entity declarative part and no entity statement part
#    is to be produced. Further, it is good custom in VHDL-93 to append
#    both the entity keyword as well as the entity simple name to the
#    trailer, therefore this is done to keep VHDL compilers happy.
#
# According to IEEE 1076-1993 1.1.1:
#
# entity_header :=
#  [ formal_generic_clause ]
#  [ formal_port_clause ]
#
# Implementation note:
#    Initially we will assume that there is no generic clause but that there
#    is an port clause. We would very much like to have generic and the port
#    clause should be conditional (consider writting a test-bench).

def write_primary_unit(f, module_name, port_list):
    # Entity header
    f.write('-- Entity declaration\n')
    f.write('\n')
    f.write('ENTITY %s IS\n' % module_name)
    write_port_clause(f, port_list)
    # entity_declarative_part is assumed not to be used
    # entity_statement_part is assumed not to be used
    # Entity trailer
    f.write('END %s;\n' % module_name)
    f.write('\n')

#
# Secondary Unit Section
#

# Component Declaration
#
# According to IEEE 1076-1993 4.5:
#
# component_declaration :=
#    COMPONENT identifier [ IS ]
#     [ local_generic_clause ]
#     [ local_port_clause ]
#    END COMPONENT [ component_simple_name ] ;
#
# Implementation note:
#    The component declaration should match the entity declaration of the
#    same name as the component identifier indicates. Since we do not yeat
#    support the generic clause in the entity declaration we shall not
#    support it here either. We will however support the port clause.
#
#    In the same fassion as before we will use the conditional IS symbol
#    as well as replicating the identifier as component simple name just to
#    be in line with good VHDL-93 practice and keep compilers happy.

def write_component_declarations(f, netlist, device_list):
    for device in device_list:
        # Hmm... I just grabbed this if stuff... do I need it?
        # ignore specials
        if device in ['IOPAD', 'IPAD', 'OPAD', 'HIGH', 'LOW']:
            continue

        f.write('    COMPONENT %s\n' % device)

        # Find the first package which matches the devicename
        package = next(package for package in reversed(netlist.packages)
                       if package.get_attribute('device', 'unknown') == device)
        write_port_clause(f, get_device_port_list(package))
        f.write('    END COMPONENT ;\n')
        f.write('\n')

# THHE
# Build the port list from the symbols
#
# ... wouldn't it be better to feed get-pins,
#     get-attribute-by-pinnumber and co. with the device rather than
#     the component?  pin names and atributes are locked to the symbol
#     and not to the instance of the symbol in the sheet!

def get_device_port_list(package):
    # construct list
    return (
        get_device_matching_pins(package, reversed(package.pins), 'IN'),
        get_device_matching_pins(package, reversed(package.pins), 'OUT'),
        get_device_matching_pins(package, reversed(package.pins), 'INOUT'))

# THHE
# get a list of all pins of a given type

def get_device_matching_pins(package, pin_list, value):
    return [(pin.number, pin.get_attribute('width', 'unknown'))
            for pin in pin_list
            if pin.get_attribute('pintype', 'unknown') == value]

# THHE
# build a list of all unique devices in in the list

def get_unique_devices(device_list):
    l = []
    for device in reversed(device_list):
        if device not in l:
            l.append(device)
    return l

# THHE
# build a list of all unique devices in the schematic

def unique_devices(netlist):
    return get_unique_devices([package.get_attribute('device', 'unknown')
                               for package in reversed(netlist.packages)])


# Signal Declaration
#
# According to IEEE 1076-1993 4.3.1.2:
#
# signal_declaration :=
#    SIGNAL identifier_list : subtype_indication [ signal_kind ]
#    [ := expression ] ;
#
# signal_kind := REGISTER | BUS
#
# Implementation note:
#    Currently will the identifier list be reduced to a single entry.
#    There is no support for either register or bus type of signal kind.
#    Further, no default expression is being supported.
#    The subtype indication is hardwired to Std_Logic.

def write_signal_declarations(f, netlist):
    for signal in reversed(netlist.nets):
        f.write('    SIGNAL %s : Std_Logic;\n' % signal.name)

# Architecture Declarative Part
#
# According to IEEE 1076-1993 1.2.1:
#
# architecture_declarative_part :=
#  { block_declarative_item }
#
# block_declarative_item :=
#    subprogram_declaration
#  | subprogram_body
#  | type_declaration
#  | subtype_declaration
#  | constant_declaration
#  | signal_declaration
#  | shared_variable_declaration
#  | file_declaration
#  | alias_declaration
#  | component_declaration
#  | attribute_declaration
#  | attribute_specification
#  | configuration_specification
#  | disconnection_specification
#  | use_clause
#  | group_template_declaration
#  | group_declaration
#
# Implementation note:
#    There is currently no support for programs or procedural handling in
#    gEDA, thus will all declarations above involved in thus activites be
#    left unused. This applies to subprogram declaration, subprogram body,
#    shared variable declaration and file declaration.
#
#    Further, there is currently no support for type handling and therefore
#    will not the type declaration and subtype declaration be used.
#
#    The is currently no support for constants, aliases, configuration
#    and groups so the constant declaration, alias declaration, configuration
#    specification, group template declaration and group declaration will not
#    be used.
#
#    The attribute passing from a gEDA netlist into VHDL attributes must
#    wait, therefore will the attribute declaration and attribute
#    specification not be used.
#
#    The disconnection specification will not be used.
#
#    The use clause will not be used since we pass the responsibility to the
#    primary unit (where it is not yet supported).
#
#    The signal declation will be used to convey signals held within the
#    architecture.
#
#    The component declaration will be used to convey the declarations of
#    any external entity being used within the architecture.

def write_architecture_declarative_part(f, netlist):
    # Due to my taste will the component declarations go first
    # TODO: Broken until someday
    # THHE fixed today ;-)
    write_component_declarations(f, netlist, unique_devices(netlist))
    # Then comes the signal declatations
    write_signal_declarations(f, netlist)

# Architecture Statement Part
#
# According to IEEE 1076-1993 1.2.2:
#
# architecture_statement_part :=
#  { concurrent_statement }
#
# According to IEEE 1076-1993 9:
#
# concurrent_statement :=
#    block_statement
#  | process_statement
#  | concurrent_procedure_call_statement
#  | concurrent_assertion_statement
#  | concurrent_signal_assignment_statement
#  | component_instantiation_statement
#  | generate_statement
#
# Implementation note:
#    We currently does not support block statements, process statements,
#    concurrent procedure call statements, concurrent assertion statements,
#    concurrent signal assignment statements or generarte statements.
#
#    Thus, we only support component instantiation statements.
#
# According to IEEE 1076-1993 9.6:
#
# component_instantiation_statement :=
#    instantiation_label : instantiation_unit
#  [ generic_map_aspect ] [ port_map_aspect ] ;
#
# instantiated_unit :=
#    [ COMPONENT ] component_name
#  | ENTITY entity_name [ ( architecture_identifier ) ]
#  | CONFIGURATION configuration_name
#
# Implementation note:
#    Since we are not supporting the generic parameters we will thus not
#    suppport the generic map aspect. We will support the port map aspect.
#
#    Since we do not yeat support the component form we will not yet use
#    the component symbol based instantiated unit.
#
#    Since we do not yeat support configurations we will not support the
#    we will not support the configuration symbol based form.
#
#    This leaves us with the entity form, which we will support initially
#    using only the entity name. The architecture identifier could possibly
#    be supported by attribute value.

def write_architecture_statement_part(f, netlist):
    f.write('-- Architecture statement part\n')
    write_component_instantiation_statements(f, netlist)
    f.write('-- Signal assignment part\n')
    write_signal_assignment_statements(f, netlist)

# THHE
# write component instantiation for each component in the sheet

def write_component_instantiation_statements(f, netlist):
    for package in reversed(netlist.packages):
        device = package.get_attribute('device', 'unknown')
        if device in ['IOPAD', 'IPAD', 'OPAD', 'HIGH', 'LOW']:
            continue

        # label, entity name
        f.write('    %s : %s\n' % (package.refdes, device))
        # Generic map aspect should go in here
        # Port map aspect
        write_port_map(f, package)
        f.write(';\n')
        f.write('\n')

# THHE
# Write the signal assignment for the top-level ports
# Since I like to have the urefs as port names in the top
# level entity, I have to assign them to the correspinding nets as well

def write_signal_assignment_statements(f, netlist):
    for refdes, net in get_top_level_ports(netlist, 'IPAD'):
        f.write('%s <= %s;\n' % (net.name, refdes))

    for refdes, net in get_top_level_ports(netlist, 'OPAD'):
        f.write('%s <= %s;\n' % (refdes, net.name))

    for refdes, net in get_top_level_ports(netlist, 'IOPAD'):
        f.write('%s <= %s;\n' % (net.name, refdes))
        f.write('%s <= %s;\n' % (refdes, net.name))

# THHE
# get a list of the top-level ports (the urefs of the I/O-PADs)

def get_top_level_ports(netlist, pad_type):
    return [(package.refdes, package.pins[0].net)
            for package in reversed(netlist.packages)
            if package.get_attribute('device', 'unknown') == pad_type]

# Port map aspect
#
# According to IEEE 1076-1993 5.6.1.2:
#
# port_map_aspect := PORT MAP ( port_association_list )
#
# According to IEEE 1076-1993 4.3.2.2:
#
# association_list :=
#    association_element { , association_element }

def write_port_map(f, package):
    if not package.pins:
        return

    f.write('    PORT MAP (\n')
    for i, pin in enumerate(package.pins):
        if i != 0:
            f.write(',\n')
        f.write('        ')
        write_association_element(f, pin)
    f.write(')')

# Association element
#
# According to IEEE 1076-1993 4.3.2.2:
#
# association_element :=
#  [ formal_part => ] actual_part
#
# formal_part :=
#    formal_designator
#  | function_name ( formal_designator )
#  | type_mark ( formal_designator )
#
# formal_designator :=
#    generic_name
#  | port_name
#  | parameter_name
#
# actual_part :=
#    actual_designator
#  | function_name ( actual_designator )
#  | type_mark ( actual_designator )
#
# actual_designator :=
#    expression
#  | signal_name
#  | variable_name
#  | file_name
#  | OPEN
#
# Implementation note:
#    In the association element one may have a formal part or relly on
#    positional association. The later is doomed out as bad VHDL practice
#    and thus will the formal part allways be present.
#
#    The formal part will not support either the function name or type mark
#    based forms, thus only the formal designator form is supported.
#
#    Of the formal designator forms will generic name and port name be used
#    as appropriate (this currently means that only port name will be used).
#
#    The actual part will not support either the function name or type mark
#    based forms, thus only the actual designator form is supported.

def write_association_element(f, pin):
    f.write(pin.number)
    f.write(' => ')
    if pin.net.is_unconnected_pin:
        f.write('OPEN')
    else:
        f.write(pin.net.name)

# Secondary unit
#
# According to IEEE 1076-1993 11.1:
#
# secondary_unit :=
#    architecture_body
#  | package_body
#
# Implementation note:
#    Since we are not likely to create packages in gEDA in the near future
#    we will only support the architecture body.
#
# According to IEEE 1076-1993 1.2:
#
# architecture_body :=
#    ARCHITECTURE identifier OF entity_name IS
#       architecture_declarative_part
#    BEGIN
#       architecture_statement_part
#    END [ ARCHITECTURE ] [ architecture_simple_name ] ;
#
# Implementation note:
#    The identifier will identify one of many architectures for an entity.
#    Since we generate only an netlist architecture we will lock this to be
#    "netlist" for the time being. Just as with the entity declaration we
#    will use good VHDL-93 custom to add the architecture keyword as well
#    as the architecture simple name to the trailer to keep compilers happy.

def write_secondary_unit(f, netlist, module_name):
    f.write('-- Secondary unit\n')
    f.write('ARCHITECTURE netlist OF %s IS\n' % module_name)
    write_architecture_declarative_part(f, netlist)
    f.write('BEGIN\n')
    write_architecture_statement_part(f, netlist)
    f.write('END netlist;\n')

# Context clause
#
# According to IEEE 1076-1993 11.3:
#
# context_clause := { context_item }
# context_item := library_clause | use_clause
#
# Implementation note:
#    Both library and use clauses will be generated, eventually...
#    What is missing is the information from gEDA itself, i think.

def write_context_clause(f):
    f.write('-- Context clause\n')
    f.write('library IEEE;\n')
    f.write('use IEEE.Std_Logic_1164.all;\n')

# Top level function
# Write structural VHDL representation of the schematic

def run(f, netlist):
    f.write('-- Structural VHDL generated by gnetlist\n')

    write_context_clause(f)

    module_name = netlist.get_toplevel_attribute('module-name', 'not found')

    write_primary_unit(f, module_name, get_top_port_list(netlist))

    f.write('\n')

    write_secondary_unit(f, netlist, module_name)
