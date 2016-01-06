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

# VHDL-AMS netlist backend written by Eduard Moser and Martin Lehmann.
# Build on the VHDL backend from Magnus Danielson

# The following parameters are assigned from gschem.
# Only important for automatic-gnetlist-calls from gschem!

generate_mode = 1
top_package = None
top_attribs = []

# ===================== REALLY IMPORTANT HELP FUNCTIONS ======================

# returns a list, whitout the specified string.

def list_without_str_attrib(ls, str):
    return [l for l in ls if l != str]

# returns all not default-setted generics
# After our definitions, all attribs, which values not started with a
# '?' - character.

def all_used_generics(ls, package):
    return [l for l in ls
            if not package.get_attribute(l, 'unknown').startswith('?')]

# checks all pins of a net for consistence, under different points
# of view (pin-attributes).

def net_consistence(attribute, net):
    if not net.connections:
        return None

    x = net.connections[0].get_attribute(attribute, 'unknown')

    for pin in net.connections[1:]:
        if pin.get_attribute(attribute, 'unknown') != x:
            return None

    return x

def net_consistence_port_mode(net):
    for pin in reversed(net.connections):
        if pin.get_attribute('port_mode', None) == 'out':
            return True

    return False

# Returns all nets to which a given list of packages are connected.

def all_packages_nets(packages):
    return [pin.net.name
            for package in packages
            for pin in reversed(package.pins)]

# returns all nets in the schematic, which not
# directly connected to a port.

def all_necessary_nets(netlist):
    without_net_names = all_packages_nets(netlist.all_ports)
    return [net for net in reversed(netlist.nets)
            if net.name not in without_net_names]

# sort all port-components out

def all_necessary_packages(netlist):
    return [package for package in reversed(netlist.packages)
            if package not in netlist.all_ports]

# if pin connetected to a port (special component), then return port.
# else return the net, which the pin is connetcted to.

def port_test(netlist, netname):
    if netname in all_packages_nets(netlist.all_ports):
        return which_port(netname, netlist.all_ports)
    else:
        return netname

# returns the port, when is in port-list, which the pin is connected to

def which_port(netname, ports):
    for port in ports:
        if port.pins[-1].net.name == netname:
            return port

    return []


# ========================== ENTITY GENERATING PART ==========================

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


# writes some needed library insertions staticly
# not really clever, but a first solution

def write_context_clause(f):
    f.write('LIBRARY ieee,disciplines;\n')
    f.write('USE ieee.math_real.all;\n')
    f.write('USE ieee.math_real.all;\n')
    f.write('USE work.electrical_system.all;\n')
    f.write('USE work.all;\n')


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
#    is to be produced. Further, it is good custom in VAMS-93 to append
#    both the entity keyword as well as the entity simple name to the
#    trailer, therefore this is done to keep VAMS compilers happy.
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


# this routine managed the complete entity-declaration of a component
# or a schematic.

def write_primary_unit(f, entity, port_list, generic_list):
      write_context_clause(f)
      f.write('-- Entity declaration -- \n\n')
      f.write('ENTITY ')
      f.write(entity)
      f.write(' IS\n')
      write_generic_clause(f, generic_list)
      write_port_clause(f, port_list)
      f.write('END ENTITY ')
      f.write(entity)
      f.write('; \n\n')


# GENERIC & PORT Clause
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


# this next two functions are writing the generic-clause
# in the entity declaration
# generic_list is a list of all generics and
# its values, such like [('power', 12.2), ('velocity', 233.34)]

def write_generic_clause(f, generic_list):
    if not generic_list:
        return

    f.write('\t GENERIC (')
    f.write('\t')
    for i, generic in enumerate(generic_list):
        if i != 0:
            f.write(';\n\t\t\t')
        assert len(generic) == 2
        f.write('%s : REAL := %s' % generic)
    f.write(' );\n')


# this function writes the port-clause in the entity-declarartion

# This little routine writes a single pin on the port-clause.
# port_list is a list containing
#   (port_name, port_object, port_type, port_mode)
# such like
#   [('heat', 'quantity', 'thermal', 'in'),
#    ('base', 'terminal', 'electrical', 'unknown'), ...]

def write_port_clause(f, port_list):
    if not port_list:
        return

    f.write('\t PORT (\t\t')

    for i, port in enumerate(port_list):
        if i != 0:
            f.write(';\n\t\t\t')
        assert len(port) == 4
        f.write('%s \t%s \t: %s \t%s' % (
            port[1], port[0],
            port[3] if port[1] == 'quantity' else '', port[2]))

    f.write(' );\n')

# ======================= ARCHITECTURE GENERATING PART =======================

# Secondary Unit Section
#

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
#    The attribute passing from a gEDA netlist into VAMS attributes must
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


# toplevel-subfunction for architecture generation.

def write_secondary_unit(f, netlist, architecture, entity):
    f.write('-- Secondary unit\n\n')
    f.write('ARCHITECTURE %s OF %s IS\n' % (architecture, entity))
    write_architecture_declarative_part(f, netlist)
    f.write('BEGIN\n')
    write_architecture_statement_part(f, netlist)
    f.write('END ARCHITECTURE %s;\n' % architecture)


# at this time, it only calls the signal declarations

def write_architecture_declarative_part(f, netlist):
      # Due to my taste will the component declarations go first
      # XXX - Broken until someday
      # (vams:write_component_declarations packages)
      # Then comes the signal declatations
      write_signal_declarations(f, netlist)


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


# the really signal-declaration-writing function
# it's something more complex, because it's checking all signals
# for consistence. it only needs the output-port as parameter.

def write_signal_declarations(f, netlist):
    for net in all_necessary_nets(netlist):
        port_object = net_consistence('port_object', net)
        port_type = net_consistence('port_type', net)

        if port_object is not None and port_type is not None:
        # and port_object == 'quantity'
        # and port_mode(net_consistence_port_mode(net))
            f.write('\t%s %s \t:  %s;\n' % (port_object, net.name, port_type))
        else:
            f.write('-- error in subnet : %s\n' % net.name)


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
#    be in line with good VAMS-93 practice and keep compilers happy.

# writes the architecture body.

def write_architecture_statement_part(f, netlist):
      f.write('-- Architecture statement part')
      f.write('\n')

      for package in all_necessary_packages(netlist):
          device = package.get_attribute('device', 'unknown')
          architecture = package.get_attribute('architecture', None)

          if device not in ['IOPAD', 'IPAD', 'OPAD', 'HIGH', 'LOW']:
              f.write(' \n  ')

              # writes instance-label
              f.write(package.refdes)
              f.write(' : ENTITY ')

              # writes entity name, which should instanciated
              f.write(device)

              # write the architecture of an entity in brackets after
              # the entity, when necessary.
              if architecture is not None:
                  f.write('(')
                  if architecture.startswith('?'):
                      f.write(architecture[1:])
                  else:
                      f.write(architecture)
                  f.write(')')
              f.write('\n')

              # writes generic map
              write_generic_map(f, package)

              # writes port map
              write_port_map(f, netlist, package)

              f.write(';\n')


# Given a package, prints all generics attribute => values, without some
# special attribs, like refdes, source and architecture.
# Don't ask why .... it's not the right place to discuss this.

def write_generic_map(f, package):
    x = reversed(package.get_attribute_names(False))
    x = list_without_str_attrib(x, 'refdes')
    x = list_without_str_attrib(x, 'source')
    x = list_without_str_attrib(x, 'architecture')
    x = list_without_str_attrib(x, 'slot')
    new_ls = all_used_generics(x, package)
    if new_ls:
        f.write('\tGENERIC MAP (\n')
        write_component_attributes(f, package, new_ls)
        f.write(')\n')


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

# writes the port map of the component.

def write_port_map(f, netlist, package):
    if package.pins:
        f.write('\tPORT MAP (\t')
        for i, pin in enumerate(package.pins):
            if i != 0:
                f.write(',\n')
                f.write('\t\t\t')
            write_association_element(
                f, netlist, pin.number, pin.net)
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
#    positional association. The later is doomed out as bad VAMS practice
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


# the purpose of this function is very easy: write OPEN if pin
# unconnected and normal output if it connected.

def write_association_element(f, netlist, pinname, net):
    f.write(pinname)
    f.write(' => ')
    if net.is_unconnected_pin:
        f.write('OPEN')
    else:
        f.write(port_test(netlist, net.name))


# writes all generics of a component into the generic map.

def write_component_attributes(f, package, generic_list):
    helper = False
    for attrib in generic_list:
        value = package.get_attribute(attrib, None)
        if helper:
            if value is None:
                break
            f.write(', \n')
        else:
            if value is None:
                continue
        helper = True
        f.write('\t\t\t%s => %s' % (attrib, value))

# ============================ TOP LEVEL FUNCTION ============================

# returns a string, where are all whitespaces replaced to underlines

def change_all_whitespaces_to_underlines(s):
    while True:
        try:
            pos = s.index(' ')
        except ValueError:
            break

        if pos == len(s) - 1:
            s = s[:-1]
        else:
            s = s[:pos] + '_' + s[pos + 1:]

    return s

# generates a port list of the current schematic, or returns
# a empty list, if no port reachable.

def generate_port_list():
    if top_package is None:
        return []

    return [(pin.number, pin.get_attribute('port_object', 'unknown'),
                         pin.get_attribute('port_type', 'unknown'),
                         pin.get_attribute('port_mode', 'unknown'))
            for pin in reversed(top_package.pins)]

# generate generic list for generic clause
# [(generic, value), (generic, value), ...]

def generate_generic_list():
    gl = []
    for l in top_attribs:
        if l.startswith('refdes=') or \
           l.startswith('source=') or \
           l.startswith('architecture='):
            continue

        try:
            pos = l.rindex('=')
        except ValueError:
            gl.append(l)
            continue

        if l[pos + 1] == '?':
            pos += 1

        gl.append((l[:pos], l[pos + 1:]))

    return gl

# Write structural VAMS representation of the schematic.
#
# Evaluates things like generate_mode and top_attribs and starts the
# major subroutines.

def run(f, netlist):
    # Collects all ports from the netlist and stores them to
    # netlist.all_ports for future reference.

    # important for hierachical netlists. in our definition ports are
    # special components, which device-attributes a setted to 'PORT'.
    # The port-attributes are saved on toplevel of this special component.

    netlist.all_ports = [
        package for package in reversed(netlist.packages)
        if package.get_attribute('device', None) == 'PORT']

    # generate correctly architecture name
    architecture = change_all_whitespaces_to_underlines(
        netlist.get_toplevel_attribute('architecture', 'default_architecture'))

    # generate correctly entity name
    entity = change_all_whitespaces_to_underlines(
        netlist.get_toplevel_attribute('entity', 'default_entity'))

    # search all ports of a schematic. for entity generation only.
    port_list = generate_port_list()

    # search all generic of a schematic. for entity generatin only.
    generic_list = generate_generic_list()


    # generate_mode : 1 (default) -> generate a architecture (netlist) of a
    #                                schematic
    #                 2           -> is selected a component then generate
    #                                a entity of this, else generate
    #                                a toplevel entity. called from gschem
    #                                normally.

    if generate_mode == 1:
        # generate output_filename, like
        # (<entity>_arc.<output-file-extension>)
        #stem = entity.lower() + '_arc'

        f.write('-- Structural VAMS generated by gnetlist\n')
        write_secondary_unit(f, netlist, architecture, entity)
    elif generate_mode == 2:
        # if one component selected, then generate output_filename
        # (<device of selected component>.vhdl), else
        # <entity>.vhdl
        #if top_attribs:
        #    output_filename = top_package.get_attribute('device', 'unknown')
        #                                 .lower() + '.vhdl'
        #else:
        #    output_filename = entity.lower() + '.vhdl'

        # decide about the right parameters for entity-declaration
        if top_package is not None:
            write_primary_unit(
                f, top_package.get_attribute('device', 'unknown'),
                port_list, generic_list)
        else:
            write_primary_unit(f, entity, port_list, generic_list)
