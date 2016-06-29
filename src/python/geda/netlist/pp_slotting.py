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

## \namespace xorn.geda.netlist.pp_slotting
## Post-processing: Slotting mechanism.

from gettext import gettext as _
import xorn.geda.attrib

## Characters used to separate the pin labels in a \c "slotdef=" attribute.
SLOTDEF_ATTRIB_DELIMITERS = ',; '

## Split a string into non-empty parts separated by a set of delimiters.

def strtok(s, delim):
    start = 0
    while start < len(s):
        found = [i for i in (s.find(d, start) for d in delim) if i != -1]
        if found:
            end = min(found)
        else:
            end = len(s)
        if end != start:
            yield s[start:end]
        start = end + 1

## Get the active slot definition of a component.
#
# Reads the \c "slotdef=" and \c "slot=" attributes of the component
# and returns the appropriate slot definition for the slot, parsed
# into a list of pin labels.  If the component isn't slotted or the
# slotting attributes are invalid, returns \c None.

def get_slotdef(component_blueprint):
    # For this particular graphic object (component instantiation)
    # get the slot number as a string

    try:
        value = component_blueprint.get_attribute('slot')
    except KeyError:
        # Did not find slot= attribute.
        # This happens if there is no attached slot attribute, and
        # there is no default slot= attribute inside the symbol.
        # Assume slot=1.
        slot = 1
        slot_string = False
    else:
        try:
            slot = int(value)
        except ValueError:
            component_blueprint.error(
                _("non-numeric slot= attribute: %s") % value)
            slot = 0
        slot_string = True

    # OK, now that we have the slot number, use it to get the
    # corresponding slotdef=#:#,#,# string.
    slotdef = None
    for value in xorn.geda.attrib.search_all(
            component_blueprint.ob, 'slotdef'):
        if ':' not in value:
            # Didn't find proper slotdef=#:... put warning into log
            component_blueprint.error(
                _("improper slotdef= syntax: missing \":\" in \"%s\"") % value)
            return None

        if value.startswith('%d:' % slot):
            slotdef = value
            break

    if slotdef is None:
        if slot_string:
            # only an error if there's a slot string
            component_blueprint.error(
                _("did not find slotdef= attribute for slot %d") % slot)
        return None

    # skip over slotdef number
    # slotdef is in the form #:#,#,#
    # this code skips first #:
    cptr = slotdef[slotdef.index(':') + 1:]  # skip colon

    if not cptr:
        component_blueprint.error(_("improper slotdef= syntax: "
                                    "missing definition for slot %d") % slot)
        return None

    return list(strtok(cptr, SLOTDEF_ATTRIB_DELIMITERS))

def postproc_blueprints(netlist):
    for schematic in netlist.schematics:
        for component in schematic.components:
            # parse the slotdef= and slot= attributes of the component
            # and extract the effective list of pinnumbers for this slot
            component.slotdef = get_slotdef(component)

            # create a dictionary of pins by pinseq
            component.pins_by_pinseq = {}
            for pin in component.pins:
                try:
                    value = pin.get_attribute('pinseq')
                except KeyError:
                    continue
                try:
                    pinseq = int(value)
                except ValueError:
                    pin.error(_("non-numeric pinseq value: %s") % value)
                    continue
                if pinseq in component.pins_by_pinseq:
                    pin.error(_("duplicate pinseq \"%s\"") % pinseq)
                    continue
                component.pins_by_pinseq[pinseq] = pin
                if component.slotdef is not None \
                       and pinseq - 1 >= len(component.slotdef):
                    pin.warn(_("pin has pinseq=%d, but only %d pins are "
                               "covered by slot definition") % (
                                   pinseq, len(component.slotdef)))

            # for non-slotted components, just assign regular pinnumbers
            for pin in component.pins:
                pin.number = pin.get_attribute('pinnumber', None)

            # override these pinnumbers with the ones from the slot definition
            if component.slotdef is not None:
                for i, pinnumber in enumerate(component.slotdef):
                    try:
                        pin = component.pins_by_pinseq[i + 1]
                    except KeyError:
                        component.error(
                            _("slot definition covers %d pins, "
                              "but there is no pin with pinseq=%d") % (
                                  len(component.slotdef), i + 1))
                    else:
                        pin.number = pinnumber

            # create a dictionary of pins by pinnumber
            component.pins_by_number = {}
            for pin in component.pins:
                if pin.number is None:
                    pin.error(_("pinnumber missing"))
                elif pin.number in component.pins_by_number:
                    if component.composite_sources:
                        pin.warn(_("duplicate pinnumber \"%s\"") % pin.number)
                    else:
                        pin.error(_("duplicate pinnumber \"%s\"") % pin.number)
                else:
                    component.pins_by_number[pin.number] = pin
