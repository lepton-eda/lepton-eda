# The spice-noqsi back end for gnetlist
`gnet-spice-noqsi.scm` is a "back end" for the gnetlist program, which is part of gEDA. See <http://gpleda.org/> for details on gEDA and gnetlist.

`gnetlist -g spice-noqsi ...` produces output in SPICE format. It is intended to allow the designer to produce schematics that can be used as input for both SPICE simulation and printed circuit layout. It supports this by providing flexible methods for mapping schematic symbol attributes into the parameters of SPICE declarations and commands.

See the [INSTALL](<https://github.com/noqsi/gnet-spice-noqsi/blob/master/INSTALL>) file for installation instructions. 

For a tutorials and reference documentation see the
[Wiki](<https://github.com/noqsi/gnet-spice-noqsi/wiki>).
