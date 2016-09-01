# The spice-noqsi back end for gnetlist
`gnet-spice-noqsi.scm` is a "back end" for the gnetlist program, which is part of gEDA. See <http://gpleda.org/> for details on gEDA and gnetlist.

`gnetlist -g spice-noqsi ...` produces output in SPICE format. It is intended to allow the designer to produce schematics that can be used as input for both SPICE simulation and printed circuit layout. It supports this by providing flexible methods for mapping schematic symbol attributes into the parameters of SPICE declarations and commands.

See the INSTALL file for installation instructions. 

For a tutorial introduction, see:
<file:tutorial/HelloWorld/HelloWorld.md>


## Atributes and expansions
The process of creating a SPICE file is driven by attributes attached to symbols. In the definitions below, *italics* represent variable text you supply, normal text and symbols are literal. Quick reference for these attributes:

file=*F*

*F* is a file name to be included by .include. This attribute is allowed on any symbol.

device=*D*

*D* is used to look up a default "prototype" for the device, if no spice-prototype attribute is present.

spice-prototype=*P*

*P* is a series of fields separated by white space. If the field does not contain one of the "magic" characters in the set "?#=@%", it is copied literally. Otherwise, the leftmost magic character controls translation of the field. The translation patterns are:

* ?	refdes
* *T*?	refdes with *T* prepended if needed
* \#*n*	net attached to pinnumber *n* of current object
* *R*#*n*	net attached to pinnumber *n* of refdes *R*
* *A*=	attribute *A*=value if attribute *A* exists, blank if not
* *A*=*D* attribute *A*=value, if attribute *A* exists, *A*=*D* by default
* *A*@	value of attribute *A* if it exists, blank if not
* *A*@*D*	value of attribute *A* if it exists, defaulting to *D* if not
* %pinseq	all nets attached to current object in pinseq order
* %io	all nets attached to spice-IO symbols in numerical refdes order
* %up	all nets attached to hierarchical IO symbols in lexical refdes order
* %down	all nets attached to current object in lexical pinlabel order

In the above, *A* refers to an attribute attached to the current object.

The prototype, for when no spice-prototype= attribute is given, and the device= attribute is unrecognised, is:

`? %pinseq value@ model-name@ spice-args@`

This yields the literal refdes followed by the list of connected nets in pinseq order, similar to previous SPICE netlisters for gEDA. It thus works for many already-existing symbols. It follows this with the values of the value=, model-name=, and spice-args= attributes.

Whitespace in the spice-prototype attribute is copied literally, so multiline values turn into multiple SPICE "cards". Here's the default prototype for the DUAL_OPAMP device:

	X1? #3 #2 #8 #4 #1 model-name@
	X2? #5 #6 #8 #4 #7 model-name@

This maps the defacto industry standard pinout for a dual opamp onto the defacto industry standard pin order for an opamp SPICE model. It creates two instances, one with prefix "X1" and one with prefix "X2".

See the end of gnet-spice-noqsi.scm for more prototype examples.

If one of the translations creates a ".subckt" card, it will be the first non-comment card in the output, and the last card will be ".ends".

The toplevel attribute "spice-prolog", if present, will be expanded as if if was a spice-prototype, but before regular symbol processing. Similarly, a toplevel "spice-epilog" attribute will be expanded after all symbol processing. An attribute expansion in these will search for the named attribute at toplevel. 

## Built-in default prototypes

The built-in prototypes provide behavior similar to earlier SPICE netlist backends for gnetlist.

device | prototype
------ | ---------
unknown | ? %pinseq value@ model-name@ spice-args@
AOP-Standard | X? %pinseq model-name@
BATTERY | V? #1 #2 spice-args@
SPICE-cccs | F? #1 #2 V? value@
 | V? #3 #4 DC 0
SPICE-ccvs | H? #1 #2 V? value@
 | V? #3 #4 DC 0
directive | value@
include | *
options | .OPTIONS value@
CURRENT_SOURCE | I? %pinseq value@
K | K? inductors@ value@
SPICE-nullor | N? %pinseq value@1E6
SPICE-NPN | Q? %pinseq model-name@ spice-args@ ic= temp=
PNP_TRANSISTOR | Q? %pinseq model-name@ spice-args@ ic= temp=
NPN_TRANSISTOR | Q? %pinseq model-name@ spice-args@ ic= temp=
spice-subcircuit-LL | .SUBCKT model-name@ %io
spice-IO | *
SPICE-VC-switch | S? %pinseq model-name@ value@
T-line | T? %pinseq value@
vac | V? %pinseq value@
SPICE-vccs | G? %pinseq value@
SPICE-vcvs | E? %pinseq value@
VOLTAGE_SOURCE | V? %pinseq value@
vexp | V? %pinseq value@
vpulse | V? %pinseq value@
vpwl | V? %pinseq value@
vsin | V? %pinseq value@
VOLTAGE_SOURCE | V? %pinseq value@
INPUT | *
OUTPUT | *
CAPACITOR | C? %pinseq value@ model-name@ spice-args@ l= w= area= ic=
DIODE | D? %pinseq model-name@ spice-args@ area= ic= temp=
NMOS_TRANSISTOR | M? %pinseq model-name@ spice-args@ l= w= as= ad= pd= ps= nrd= nrs= temp= ic= m=
PMOS_TRANSISTOR | M? %pinseq model-name@ spice-args@ l= w= as= ad= pd= ps= nrd= nrs= temp= ic= m=
RESISTOR | R? %pinseq value@ model-name@ spice-args@ w= l= area= temp=
DUAL_OPAMP | X1? #3 #2 #8 #4 #1 model-name@
 | X2? #5 #6 #8 #4 #7 model-name@
QUAD_OPAMP | X1? #3 #2 #11 #4 #1 model-name@
 | X2? #5 #6 #11 #4 #7 model-name@
 | X3? #10 #9 #11 #4 #8 model-name@
 | X4? #12 #13 #11 #4 #14 model-name@
model | * refdes@

The **include**, **spice-IO**, **INPUT**, **OUTPUT**, and **model** devices simply produce comments. Their functions are more indirect. The spice-noqsi backend does not currently handle the model= attribute the way earlier backends did.