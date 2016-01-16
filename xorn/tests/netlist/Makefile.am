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

TEST_EXTENSIONS = .out

AM_TESTS_ENVIRONMENT = \
	builddir='$(builddir)'; export builddir; \
	top_builddir='$(top_builddir)'; export top_builddir; \
	srcdir='$(srcdir)'; export srcdir; \
	top_srcdir='$(top_srcdir)'; export top_srcdir;
OUT_LOG_COMPILER = $(srcdir)/run-test

# required by backends: bom bom2 drc
attribs:
	$(AM_V_GEN){ echo refdes; echo value; echo device; } > $@
all-local: attribs

dist_check_SCRIPTS = run-test
EXTRA_DIST = $(TESTS) $(testdata) gschemrc common/gschemrc
# dist_check_DATA would be more appropriate for $(TESTS) and $(testdata)
# but triggers a lot of "Nothing to be done for '...'" messages.

mostlyclean-local:
	for test in $(TESTS); do \
	    rm -f "$${test%.out}.new"; \
	done
	rm -f *-extras attribs
	rm -rf devfiles

TESTS = stack_1-geda.out \
	hierarchy-geda.out \
	hierarchy2-geda.out \
	drc2/connected_noconnects-drc2.out \
	drc2/duplicated_refdes-drc2.out \
	drc2/duplicated_slot-drc2.out \
	drc2/gnd_with_nc_directive-drc2.out \
	drc2/gnd_without_nc_directive-drc2.out \
	drc2/net_not_driven-drc2.out \
	drc2/net_not_driven_with_dont_check_if_driven_directive-drc2.out \
	drc2/net_not_driven_with_nc_directive-drc2.out \
	drc2/net_with_only_one_connection-drc2.out \
	drc2/numslots_is_not_an_integer-drc2.out \
	drc2/numslots_not_defined-drc2.out \
	drc2/output_connected_to_output-drc2.out \
	drc2/pin_without_pintype_attrib-drc2.out \
	drc2/refdes_not_numbered-drc2.out \
	drc2/slot_is_not_an_integer-drc2.out \
	drc2/slot_not_defined-drc2.out \
	drc2/slot_out_of_range-drc2.out \
	drc2/unconnected_pin-drc2.out \
	drc2/unconnected_pin_with_drc_directive-drc2.out \
	drc2/unused_slot-drc2.out \
	common/JD-PCB.out \
	common/JD-allegro.out \
	common/JD-bae.out \
	common/JD-bom.out \
	common/JD-bom2.out \
	common/JD-calay.out \
	common/JD-drc.out \
	common/JD-drc2.out \
	common/JD-dumpindent.out \
	common/JD-dumpjson.out \
	common/JD-dumplihata.out \
	common/JD-dumptext.out \
	common/JD-dumpxml.out \
	common/JD-eagle.out \
	common/JD-ewnet.out \
	common/JD-futurenet2.out \
	common/JD-geda.out \
	common/JD-gossip.out \
	common/JD-gsch2pcb.out \
	common/JD-liquidpcb.out \
	common/JD-mathematica.out \
	common/JD-maxascii.out \
	common/JD-osmond.out \
	common/JD-pads.out \
	common/JD-partslist1.out \
	common/JD-partslist2.out \
	common/JD-partslist3.out \
	common/JD-pcbfwd.out \
	common/JD-pcbpins.out \
	common/JD-protelII.out \
	common/JD-redac.out \
	common/JD-spice.out \
	common/JD-spice_noqsi.out \
	common/JD-spice_sdb.out \
	common/JD-spice_sdb-nomunge.out \
	common/JD-spice_sdb-include.out \
	common/JD-spice_sdb-include-nomunge.out \
	common/JD-spice_sdb-sort.out \
	common/JD-spice_sdb-sort-nomunge.out \
	common/JD-switcap.out \
	common/JD-systemc.out \
	common/JD-tango.out \
	common/JD-vams.out \
	common/JD-verilog.out \
	common/JD-vhdl.out \
	common/JD-vipec.out \
	common/SlottedOpamps-PCB.out \
	common/SlottedOpamps-allegro.out \
	common/SlottedOpamps-bae.out \
	common/SlottedOpamps-bom.out \
	common/SlottedOpamps-bom2.out \
	common/SlottedOpamps-calay.out \
	common/SlottedOpamps-drc.out \
	common/SlottedOpamps-drc2.out \
	common/SlottedOpamps-dumpindent.out \
	common/SlottedOpamps-dumpjson.out \
	common/SlottedOpamps-dumplihata.out \
	common/SlottedOpamps-dumptext.out \
	common/SlottedOpamps-dumpxml.out \
	common/SlottedOpamps-eagle.out \
	common/SlottedOpamps-ewnet.out \
	common/SlottedOpamps-futurenet2.out \
	common/SlottedOpamps-geda.out \
	common/SlottedOpamps-gossip.out \
	common/SlottedOpamps-gsch2pcb.out \
	common/SlottedOpamps-liquidpcb.out \
	common/SlottedOpamps-mathematica.out \
	common/SlottedOpamps-maxascii.out \
	common/SlottedOpamps-osmond.out \
	common/SlottedOpamps-pads.out \
	common/SlottedOpamps-partslist1.out \
	common/SlottedOpamps-partslist2.out \
	common/SlottedOpamps-partslist3.out \
	common/SlottedOpamps-pcbfwd.out \
	common/SlottedOpamps-pcbpins.out \
	common/SlottedOpamps-protelII.out \
	common/SlottedOpamps-redac.out \
	common/SlottedOpamps-spice.out \
	common/SlottedOpamps-spice_noqsi.out \
	common/SlottedOpamps-spice_sdb.out \
	common/SlottedOpamps-switcap.out \
	common/SlottedOpamps-systemc.out \
	common/SlottedOpamps-tango.out \
	common/SlottedOpamps-vams.out \
	common/SlottedOpamps-verilog.out \
	common/SlottedOpamps-vhdl.out \
	common/SlottedOpamps-vipec.out \
	common/TwoStageAmp-PCB.out \
	common/TwoStageAmp-allegro.out \
	common/TwoStageAmp-bae.out \
	common/TwoStageAmp-bom.out \
	common/TwoStageAmp-bom2.out \
	common/TwoStageAmp-calay.out \
	common/TwoStageAmp-drc.out \
	common/TwoStageAmp-drc2.out \
	common/TwoStageAmp-dumpindent.out \
	common/TwoStageAmp-dumpjson.out \
	common/TwoStageAmp-dumplihata.out \
	common/TwoStageAmp-dumptext.out \
	common/TwoStageAmp-dumpxml.out \
	common/TwoStageAmp-eagle.out \
	common/TwoStageAmp-ewnet.out \
	common/TwoStageAmp-futurenet2.out \
	common/TwoStageAmp-geda.out \
	common/TwoStageAmp-gossip.out \
	common/TwoStageAmp-gsch2pcb.out \
	common/TwoStageAmp-liquidpcb.out \
	common/TwoStageAmp-mathematica.out \
	common/TwoStageAmp-maxascii.out \
	common/TwoStageAmp-osmond.out \
	common/TwoStageAmp-pads.out \
	common/TwoStageAmp-partslist1.out \
	common/TwoStageAmp-partslist2.out \
	common/TwoStageAmp-partslist3.out \
	common/TwoStageAmp-pcbfwd.out \
	common/TwoStageAmp-pcbpins.out \
	common/TwoStageAmp-protelII.out \
	common/TwoStageAmp-redac.out \
	common/TwoStageAmp-spice.out \
	common/TwoStageAmp-spice_noqsi.out \
	common/TwoStageAmp-spice_sdb.out \
	common/TwoStageAmp-spice_sdb-include.out \
	common/TwoStageAmp-spice_sdb-sort.out \
	common/TwoStageAmp-switcap.out \
	common/TwoStageAmp-systemc.out \
	common/TwoStageAmp-tango.out \
	common/TwoStageAmp-vams.out \
	common/TwoStageAmp-verilog.out \
	common/TwoStageAmp-vhdl.out \
	common/TwoStageAmp-vipec.out \
	common/cascade-PCB.out \
	common/cascade-allegro.out \
	common/cascade-bae.out \
	common/cascade-bom.out \
	common/cascade-bom2.out \
	common/cascade-calay.out \
	common/cascade-cascade.out \
	common/cascade-drc.out \
	common/cascade-drc2.out \
	common/cascade-dumpindent.out \
	common/cascade-dumpjson.out \
	common/cascade-dumplihata.out \
	common/cascade-dumptext.out \
	common/cascade-dumpxml.out \
	common/cascade-eagle.out \
	common/cascade-ewnet.out \
	common/cascade-futurenet2.out \
	common/cascade-geda.out \
	common/cascade-gossip.out \
	common/cascade-gsch2pcb.out \
	common/cascade-liquidpcb.out \
	common/cascade-mathematica.out \
	common/cascade-maxascii.out \
	common/cascade-osmond.out \
	common/cascade-pads.out \
	common/cascade-partslist1.out \
	common/cascade-partslist2.out \
	common/cascade-partslist3.out \
	common/cascade-pcbfwd.out \
	common/cascade-pcbpins.out \
	common/cascade-protelII.out \
	common/cascade-redac.out \
	common/cascade-spice.out \
	common/cascade-spice_noqsi.out \
	common/cascade-spice_sdb.out \
	common/cascade-switcap.out \
	common/cascade-systemc.out \
	common/cascade-tango.out \
	common/cascade-vams.out \
	common/cascade-verilog.out \
	common/cascade-vhdl.out \
	common/cascade-vipec.out \
	common/multiequal-PCB.out \
	common/multiequal-allegro.out \
	common/multiequal-bae.out \
	common/multiequal-bom.out \
	common/multiequal-bom2.out \
	common/multiequal-calay.out \
	common/multiequal-drc.out \
	common/multiequal-drc2.out \
	common/multiequal-dumpindent.out \
	common/multiequal-dumpjson.out \
	common/multiequal-dumplihata.out \
	common/multiequal-dumptext.out \
	common/multiequal-dumpxml.out \
	common/multiequal-eagle.out \
	common/multiequal-ewnet.out \
	common/multiequal-futurenet2.out \
	common/multiequal-geda.out \
	common/multiequal-gossip.out \
	common/multiequal-gsch2pcb.out \
	common/multiequal-liquidpcb.out \
	common/multiequal-mathematica.out \
	common/multiequal-maxascii.out \
	common/multiequal-osmond.out \
	common/multiequal-pads.out \
	common/multiequal-partslist1.out \
	common/multiequal-partslist2.out \
	common/multiequal-partslist3.out \
	common/multiequal-pcbfwd.out \
	common/multiequal-pcbpins.out \
	common/multiequal-protelII.out \
	common/multiequal-redac.out \
	common/multiequal-spice.out \
	common/multiequal-spice_noqsi.out \
	common/multiequal-spice_sdb.out \
	common/multiequal-switcap.out \
	common/multiequal-systemc.out \
	common/multiequal-tango.out \
	common/multiequal-vams.out \
	common/multiequal-verilog.out \
	common/multiequal-vhdl.out \
	common/multiequal-vipec.out \
	common/netattrib-PCB.out \
	common/netattrib-allegro.out \
	common/netattrib-bae.out \
	common/netattrib-bom.out \
	common/netattrib-bom2.out \
	common/netattrib-calay.out \
	common/netattrib-drc.out \
	common/netattrib-drc2.out \
	common/netattrib-dumpindent.out \
	common/netattrib-dumpjson.out \
	common/netattrib-dumplihata.out \
	common/netattrib-dumptext.out \
	common/netattrib-dumpxml.out \
	common/netattrib-eagle.out \
	common/netattrib-ewnet.out \
	common/netattrib-futurenet2.out \
	common/netattrib-geda.out \
	common/netattrib-gossip.out \
	common/netattrib-gsch2pcb.out \
	common/netattrib-liquidpcb.out \
	common/netattrib-mathematica.out \
	common/netattrib-maxascii.out \
	common/netattrib-osmond.out \
	common/netattrib-pads.out \
	common/netattrib-partslist1.out \
	common/netattrib-partslist2.out \
	common/netattrib-partslist3.out \
	common/netattrib-pcbfwd.out \
	common/netattrib-pcbpins.out \
	common/netattrib-protelII.out \
	common/netattrib-redac.out \
	common/netattrib-spice.out \
	common/netattrib-spice_noqsi.out \
	common/netattrib-spice_sdb.out \
	common/netattrib-switcap.out \
	common/netattrib-systemc.out \
	common/netattrib-tango.out \
	common/netattrib-vams.out \
	common/netattrib-verilog.out \
	common/netattrib-vhdl.out \
	common/netattrib-vipec.out \
	common/powersupply-PCB.out \
	common/powersupply-allegro.out \
	common/powersupply-bae.out \
	common/powersupply-bom.out \
	common/powersupply-bom2.out \
	common/powersupply-calay.out \
	common/powersupply-drc.out \
	common/powersupply-drc2.out \
	common/powersupply-dumpindent.out \
	common/powersupply-dumpjson.out \
	common/powersupply-dumplihata.out \
	common/powersupply-dumptext.out \
	common/powersupply-dumpxml.out \
	common/powersupply-eagle.out \
	common/powersupply-ewnet.out \
	common/powersupply-futurenet2.out \
	common/powersupply-geda.out \
	common/powersupply-gossip.out \
	common/powersupply-gsch2pcb.out \
	common/powersupply-liquidpcb.out \
	common/powersupply-mathematica.out \
	common/powersupply-maxascii.out \
	common/powersupply-osmond.out \
	common/powersupply-pads.out \
	common/powersupply-partslist1.out \
	common/powersupply-partslist2.out \
	common/powersupply-partslist3.out \
	common/powersupply-pcbfwd.out \
	common/powersupply-pcbpins.out \
	common/powersupply-protelII.out \
	common/powersupply-redac.out \
	common/powersupply-spice.out \
	common/powersupply-spice_noqsi.out \
	common/powersupply-spice_sdb.out \
	common/powersupply-switcap.out \
	common/powersupply-systemc.out \
	common/powersupply-tango.out \
	common/powersupply-vams.out \
	common/powersupply-verilog.out \
	common/powersupply-vhdl.out \
	common/powersupply-vipec.out \
	common/singlenet-PCB.out \
	common/singlenet-allegro.out \
	common/singlenet-bae.out \
	common/singlenet-bom.out \
	common/singlenet-bom2.out \
	common/singlenet-calay.out \
	common/singlenet-drc.out \
	common/singlenet-drc2.out \
	common/singlenet-dumpindent.out \
	common/singlenet-dumpjson.out \
	common/singlenet-dumplihata.out \
	common/singlenet-dumptext.out \
	common/singlenet-dumpxml.out \
	common/singlenet-eagle.out \
	common/singlenet-ewnet.out \
	common/singlenet-futurenet2.out \
	common/singlenet-geda.out \
	common/singlenet-gossip.out \
	common/singlenet-gsch2pcb.out \
	common/singlenet-liquidpcb.out \
	common/singlenet-mathematica.out \
	common/singlenet-maxascii.out \
	common/singlenet-osmond.out \
	common/singlenet-pads.out \
	common/singlenet-partslist1.out \
	common/singlenet-partslist2.out \
	common/singlenet-partslist3.out \
	common/singlenet-pcbfwd.out \
	common/singlenet-pcbpins.out \
	common/singlenet-protelII.out \
	common/singlenet-redac.out \
	common/singlenet-spice.out \
	common/singlenet-spice_noqsi.out \
	common/singlenet-spice_sdb.out \
	common/singlenet-switcap.out \
	common/singlenet-systemc.out \
	common/singlenet-tango.out \
	common/singlenet-vams.out \
	common/singlenet-verilog.out \
	common/singlenet-vhdl.out \
	common/singlenet-vipec.out

XFAIL_TESTS = \
	common/SlottedOpamps-futurenet2.out \
	common/SlottedOpamps-spice_noqsi.out \
	common/SlottedOpamps-spice_sdb.out \
	common/SlottedOpamps-switcap.out \
	common/cascade-spice_noqsi.out \
	common/netattrib-spice_noqsi.out \
	common/singlenet-spice_noqsi.out \
	common/singlenet-spice_sdb.out

testdata = \
	common/JD-extras/models/openIP_5.cir \
	common/JD-symbols/LVD.sym \
	common/JD.sch \
	common/SlottedOpamps-symbols/LM324_slotted-1.sym \
	common/SlottedOpamps.sch \
	common/TwoStageAmp-extras/Simulation.cmd \
	common/TwoStageAmp-extras/models/2N3904.mod \
	common/TwoStageAmp-symbols/transistor.sym \
	common/TwoStageAmp.sch \
	common/cascade.sch \
	common/multiequal.sch \
	common/netattrib.sch \
	common/powersupply.sch \
	common/singlenet.sch \
	drc2/connected_noconnects.sch \
	drc2/duplicated_refdes.sch \
	drc2/duplicated_slot.sch \
	drc2/gnd_with_nc_directive.sch \
	drc2/gnd_without_nc_directive.sch \
	drc2/net_not_driven.sch \
	drc2/net_not_driven_with_dont_check_if_driven_directive.sch \
	drc2/net_not_driven_with_nc_directive.sch \
	drc2/net_with_only_one_connection.sch \
	drc2/numslots_is_not_an_integer.sch \
	drc2/numslots_not_defined.sch \
	drc2/output_connected_to_output.sch \
	drc2/pin_without_pintype_attrib.sch \
	drc2/refdes_not_numbered.sch \
	drc2/slot_is_not_an_integer.sch \
	drc2/slot_not_defined.sch \
	drc2/slot_out_of_range.sch \
	drc2/unconnected_pin.sch \
	drc2/unconnected_pin_with_drc_directive.sch \
	drc2/unused_slot.sch \
	hierarchy-sources/bottom.sch \
	hierarchy-sources/middle.sch \
	hierarchy-sources/rock.sch \
	hierarchy-symbols/bottom.sym \
	hierarchy-symbols/middle.sym \
	hierarchy-symbols/rock.sym \
	hierarchy.sch \
	hierarchy2-sources/under.sch \
	hierarchy2-symbols/under.sym \
	hierarchy2.sch \
	stack_1.sch \
	std-symbols/74/7400-1.sym \
	std-symbols/74/7404-1.sym \
	std-symbols/74/74191-1.sym \
	std-symbols/74/74541-1.sym \
	std-symbols/analog/capacitor-1.sym \
	std-symbols/analog/capacitor-2.sym \
	std-symbols/analog/fuse-1.sym \
	std-symbols/analog/pnp-3.sym \
	std-symbols/analog/resistor-1.sym \
	std-symbols/analog/resistor-2.sym \
	std-symbols/analog/resistor-variable-1.sym \
	std-symbols/analog/transformer-1.sym \
	std-symbols/asic/asic-pmos-1.sym \
	std-symbols/cascade/cascade-amp-1.sym \
	std-symbols/cascade/cascade-defaults-1.sym \
	std-symbols/cascade/cascade-defaults-2.sym \
	std-symbols/cascade/cascade-filter-1.sym \
	std-symbols/cascade/cascade-mixer-1.sym \
	std-symbols/cascade/cascade-source-1.sym \
	std-symbols/cascade/cascade-transformer-1.sym \
	std-symbols/connector/mains-plug-2.sym \
	std-symbols/diode/diode-bridge-1.sym \
	std-symbols/io/in-1.sym \
	std-symbols/io/out-1.sym \
	std-symbols/linear/lm317-1.sym \
	std-symbols/memory/am9128-1.sym \
	std-symbols/misc/decon-1.sym \
	std-symbols/misc/nc-left-1.sym \
	std-symbols/misc/nc-top-1.sym \
	std-symbols/power/12V-minus-1.sym \
	std-symbols/power/gnd-1.sym \
	std-symbols/power/vcc-1.sym \
	std-symbols/spice/spice-directive-1.sym \
	std-symbols/spice/spice-include-1.sym \
	std-symbols/spice/spice-model-1.sym \
	std-symbols/spice/spice-options-1.sym \
	std-symbols/spice/vdc-1.sym \
	std-symbols/spice/vpulse-1.sym \
	std-symbols/spice/vsin-1.sym \
	std-symbols/switch/switch-spst-1.sym \
	std-symbols/titleblock/title-B.sym \
	std-symbols/titleblock/title-C.sym \
	std-symbols/titleblock/title-bordered-A.sym
