# The spice-noqsi back end for lepton-netlist
`gnet-spice-noqsi.scm` is a SPICE "back end" for the
lepton-netlist program.

`lepton-netlist -g spice-noqsi ...` produces output in SPICE
format. It is intended to allow the designer to produce schematics
that can be used as input for both SPICE simulation and printed
circuit layout. It supports this by providing flexible methods for
mapping schematic symbol attributes into the parameters of SPICE
declarations and commands.

For a tutorials and reference documentation see the
[Wiki](<https://github.com/noqsi/gnet-spice-noqsi/wiki>).
