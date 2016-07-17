Xorn/gEDA XML schematic format {#geda-xml-format}
==============================

Xorn implements an alternative, XML-based file format for gEDA
schematic and symbol files.  This is basically a verbose form of the
original gEDA file format.  For example,

    B 0 0 17000 11000 15 0 0 0 -1 -1 0 -1 -1 -1 -1 -1

becomes

    <box x="0" y="0" width="170" height="110" color="lock" width="0"/>

However, there are some moderate changes to the format with the
intention to make the files more exactly represent what Xorn sees,
make the file format more readable, and make working with the files a
bit easier:

* Symbols and pixmaps aren't embedded directly into the object but are
  defined once at the beginning of the file and then referenced by ID.
  This allows multiple component/picture objects to share the same
  embedded symbol/pixmap.

* Non-embedded symbols and pixmaps are optionally included in the
  file, as well.  This way, the file can be used without the original
  dependencies at hand while updates in the library are still
  propagated to the schematic.

* Coordinates are represented as fixed-point values; a value of
  “12345” becomes “123.45”.  Floating-point values are supported by
  Xorn but are rounded to the nearest integer when saving to a file.
  The XML format has an optional \a hybridnum feature which allows
  preserving the fractional part; since it can't be represented
  exactly in base 10, it's appended in hexadecimal notation after the
  fixed-point part, so a value of “12345.75” would become “123.45:c”.

* Fields which have a default value are optional.

* Pin objects don't have a \a whichend flag; the first coordinate is
  always the end which connects to nets.  In order to allow an exact
  SCH->XML->SCH round-trip, there is an additional flag \a inverted
  which tells whether the pin should be inverted when stored to a
  <tt>.sch</tt> or <tt>.sym</tt> file but isn't of any relevance
  otherwise.

* Busses don't have a \a ripperdir field as this can be deduced from
  the existing ripper components' orientation.

* Text objects which are considered attributes by libgeda are
  represented by a special
    <tt><attribute name="...">...</attribute></tt>
  element.  In the future, this could allow non-attribute texts to
  contain “=” characters as well as arbitrary attribute names/values
  without concern for gEDA attribute splitting rules.  Also, it makes
  automatic processing of the attributes easier.

To convert a <tt>.sch</tt> or <tt>.sym</tt> file to an XML file, use
<tt>xorn convert</tt>:

    $ xorn convert resistor-1.sym resistor-1.sym.xml

and vice versa.  Since the format is automatically guessed from the
file name, you'll have to specify the format explicitly when reading
from stdin or writing to stdout:

    $ xorn convert -I sym -O symxml < resistor-1.sym > resistor-1.sym.xml

When converting <tt>.sch</tt> files to XML files, the symbols and
pixmaps referenced by the schematic are included in the XML file by
default.  In order to make this possible, you'll need to specify the
symbol library directories:

    $ xorn convert --symbol-library=sym/
                   --symbol-library-search=/path/to/library/
        example.sch example.sch.xml

When using the default library included with gEDA, you can't use
<tt>\--symbol-library-search=</tt>, unfortunately, because it contains
symbol name conflicts.

If you don't want to include referenced symbols and pixmaps in the XML
file---for example, because you are keeping everything under version
control and want to minimize changes---you can pass the options
<tt>\--omit-symbols</tt> and <tt>\--omit-pixmaps</tt>:

    $ xorn convert --omit-symbols example.sch example.sch.xml


# File format features

Instead of a sequential file format version, this format uses *file
format features*.  The names of the features used in a file are
specified as a space-separated list in the attribute \c
file-format-features of the root element of the document.
Currently, the following file format features are defined:

## <tt>hybridnum</tt>

Coordinates and lengths are specified in a hybrid fixed/floating point
number format.  For example, the value \c "400" would be represented
as \c "4.00" which can be abbreviated as \c "4", and the value \c
"0.75" would be represented as \c "0.00:c" which can be abbreviated as
\c ":c".

## <tt>experimental</tt>

Indicates that the file format is experimental and should not be
relied upon for productive purposes.  Non-experimental versions of the
package will refuse to parse a file with this feature.


# File structure

At the root of the XML document is either a \c schematic or a \c
symbol element, depending on the file type.  It contains a \c content
element which in turn contains all objects in the file.  Additionally,
for each symbol or pixmap which is referenced or embedded in the file,
the root element contains a \c symbol or \c pixmap element,
respectively.

    <?xml version="1.0" encoding="UTF-8"?>
    <schematic xmlns="https://hedmen.org/xorn/schematic/"
               file-format-features="experimental hybridnum">
      <content>
        ...
      </content>

      <symbol id="resistor-1" name="resistor-1.sym" mode="referenced">
        <content>
          ...
        </content>
      </symbol>

      ...
    </schematic>


# Elements

## Root element (<tt>schematic</tt> or <tt>symbol</tt>)

The format is the same for schematic and symbol files, but gEDA
imposes additional restrictions.  For example, pin objects are only
allowed in symbol files, and net, bus, and component objects are only
allowed in schematic files.  Therefore, it doesn't make sense for a \c
symbol root element to contain \c symbol reference elements.

attribute              |value
-----------------------|----------------------------------------------
\c xmlns               |<tt>%https://hedmen.org/xorn/schematic/</tt>
\c file-format-features|<em>space-separated list of feature names</em>

## Reference element (<tt>symbol</tt> or <tt>pixmap</tt>)

In contrast to the original <tt>.sym/.sch</tt> file format, symbols
and pixmaps aren't directly specified by their file name.  Instead,
the \c component or \c picture element contains a *symbol ID* or
*pixmap ID* which points to a \c symbol or \c pixmap reference
element, respectively.  The reference element contains the actual file
name and/or the embedded file contents.

attribute|description
---------|-----------------------------------------------------
id       |arbitrary unique string which identifies this element
name     |file name (optional if \c mode is \c embedded)
mode     |reference mode


There are three possible reference modes:

mode      |description
----------|-----------
referenced|The actual symbol or pixmap is found in the referenced file, but the contents of that file are included in case the file is lost, to be able to distribute the schematic as a single file, and to be able to detect and print a warning if the file has been modified.
omitted   |Same as \c referenced, but the contents of the file aren't included (for use with source code management).
embedded  |The contents of the symbol or pixmap are embedded.  The original file name may or may not be specified for informational purposes.

The way how the contents of the referenced file are included is
different for symbols and pixmaps.  For symbols, the \c symbol element
contains a \c content element which in turn contains all objects in
the symbol.  For pixmaps, the \c pixmap element contains the
base64-encoded pixmap file as character data.

## <tt>content</tt>

Contains an element for each top-level object in the file.  Does not
have any attributes.

## Line attributes

attribute    |type              |default |valid
-------------|------------------|--------|------
\c linewidth |fixed-point number|0       |always
\c capstyle  |cap style value   |\c none |always
\c dashstyle |dash style value  |\c solid|always
\c dashlength|fixed-point number|—       |dash styles \c dashed, \c center, and \c phantom only
\c dashspace |fixed-point number|—       |dash styles \c dotted, \c dashed, \c center, and \c phantom only

## Fill attributes

attribute   |type              |default  |valid
------------|------------------|---------|-------
\c filltype |fill type value   |\c hollow|always
\c fillwidth|fixed-point number|—        |fill styles \c mesh and \c hatch only
\c angle0   |integer           |—        |fill styles \c mesh and \c hatch only
\c pitch0   |fixed-point number|—        |fill styles \c mesh and \c hatch only
\c angle1   |integer           |—        |fill style \c mesh only
\c pitch1   |fixed-point number|—        |fill style \c mesh only

## <tt>arc</tt>

attribute    |type              |default
-------------|------------------|----------------
\c x         |fixed-point number|—
\c y         |fixed-point number|—
\c radius    |fixed-point number|—
\c startangle|integer           |—
\c sweepangle|integer           |—
\c color     |color value       |<tt>graphic</tt>

<i>+ line attributes</i>

## <tt>box</tt>

attribute|type              |default
---------|------------------|----------------
\c x     |fixed-point number|—
\c y     |fixed-point number|—
\c width |fixed-point number|—
\c height|fixed-point number|—
\c color |color value       |<tt>graphic</tt>

<i>+ line attributes</i>
<i>+ fill attributes</i>

## <tt>circle</tt>

attribute    |type              |default
-------------|------------------|----------------
\c x         |fixed-point number|—
\c y         |fixed-point number|—
\c radius    |fixed-point number|—
\c color     |color value       |<tt>graphic</tt>

<i>+ line attributes</i>
<i>+ fill attributes</i>

## <tt>component</tt>

attribute    |type              |default
-------------|------------------|-------
\c x         |fixed-point number|—
\c y         |fixed-point number|—
\c selectable|boolean value     |\c yes
\c angle     |0, 90, 180 or 270 |\c 0
\c mirror    |boolean value     |\c no
\c symbol    |symbol ID         |—

Contains a \c text or \c attribute child element for each attached
object.

## <tt>line</tt>

attribute|type              |default
---------|------------------|-------
\c x0    |fixed-point number|—
\c y0    |fixed-point number|—
\c x1    |fixed-point number|—
\c y1    |fixed-point number|—
\c color |color value       |<tt>graphic</tt>

<i>+ line attributes</i>

## <tt>net</tt> and <tt>pin</tt>

attribute  |type              |default                                |applies to
-----------|------------------|---------------------------------------|-------------------
\c x0      |fixed-point number|—                                      |all
\c y0      |fixed-point number|—                                      |all
\c x1      |fixed-point number|—                                      |all
\c y1      |fixed-point number|—                                      |all
\c color   |color value       |\c net, \c bus, or \c pin, respectively|all
\c type    |net/pin type value|\c normal                              |all
\c inverted|boolean value     |\c no                                  |\c pin element only

Contains a \c text or \c attribute child element for each attached
object.

## <tt>path</tt>

attribute|type       |default
---------|-----------|----------------
\c color |color value|<tt>graphic</tt>

<i>+ line attributes</i>
<i>+ fill attributes</i>

Contains the path data as character data.
Preserves whitespace.

### <tt>br</tt>

Represents a line break inside a \c path element.
This has the same effect as any other whitespace but allows preserving
the structure of path data in a <tt>.sym/.sch</tt> file.
Does not have any attributes.

## <tt>picture</tt>

attribute  |type              |default
-----------|------------------|----------------
\c x       |fixed-point number|—
\c y       |fixed-point number|—
\c width   |fixed-point number|—
\c height  |fixed-point number|—
\c angle   |0, 90, 180 or 270 |\c 0
\c mirrored|boolean value     |\c no
\c pixmap  |pixmap ID         |—

## <tt>text</tt> and <tt>attribute</tt>

Text objects which can be interpreted as attributes are represented as
\c attribute elements rather than \c text elements.

attribute   |type                 |default         |applies to
------------|---------------------|----------------|-------------------------
\c name     |string               |—               |\c attribute element only
\c x        |fixed-point number   |—               |all
\c y        |fixed-point number   |—               |all
\c color    |color value          |\c text or \c attribute, respectively|all
\c size     |integer              |—                                    |all
\c visible  |boolean value        |\c yes (\c text element only)        |all
\c show     |show name/value value|\c name-value (\c text element only) |all
\c angle    |0, 90, 180 or 270    |0                                    |all
\c alignment|alignment value      |\c lower-left                        |all

Contains the text or attribute value, respectively, as character data.

### <tt>br</tt>

Represents a line break inside a \c text or \c attribute element.
Does not have any attributes.

### <tt>overbar</tt>

Represents a section of text or attribute value which should be
rendered with a line over the letters.  (This usually indicates an
inverted logic level.)
Does not have any attributes.


# Enumeration values

## Boolean

#|value
-|-
0|<tt>no</tt>
1|<tt>yes</tt>

## Line cap style

#|value
-|-
0|<tt>none</tt>
1|<tt>square</tt>
2|<tt>round</tt>

## Line dash style

#|value
-|-
0|<tt>solid</tt>
1|<tt>dotted</tt>
2|<tt>dashed</tt>
3|<tt>center</tt>
4|<tt>phantom</tt>

## Fill type

#|value
-|-
0|<tt>hollow</tt>
1|<tt>fill</tt>
2|<tt>mesh</tt>
3|<tt>hatch</tt>
4|<tt>void</tt>

## Show attribute name/value

#|value
-|-
0|<tt>name-value</tt>
1|<tt>value</tt>
2|<tt>name</tt>

## Text alignment

#|value
-|-
0|<tt>lower-left</tt>
1|<tt>middle-left</tt>
2|<tt>upper-left</tt>
3|<tt>lower-middle</tt>
4|<tt>middle-middle</tt>
5|<tt>upper-middle</tt>
6|<tt>lower-right</tt>
7|<tt>middle-right</tt>
8|<tt>upper-right</tt>

## Net/pin type

#|value
-|-
0|<tt>normal</tt>
1|<tt>bus</tt>

## Color

#|value
-|-
0|<tt>background</tt>
1|<tt>pin</tt>
2|<tt>net-endpoint</tt>
3|<tt>graphic</tt>
4|<tt>net</tt>
5|<tt>attribute</tt>
6|<tt>logic-bubble</tt>
7|<tt>dots-grid</tt>
8|<tt>detached-attribute</tt>
9|<tt>text</tt>
10|<tt>bus</tt>
11|<tt>select</tt>
12|<tt>boundingbox</tt>
13|<tt>zoom-box</tt>
14|<tt>stroke</tt>
15|<tt>lock</tt>
16|<tt>output-background</tt>
17|<tt>freestyle1</tt>
18|<tt>freestyle2</tt>
19|<tt>freestyle3</tt>
20|<tt>freestyle4</tt>
21|<tt>junction</tt>
22|<tt>mesh-grid-major</tt>
23|<tt>mesh-grid-minor</tt>

--------------------------------------------------------------------------------

Copyright (C) 2013-2016 Roland Lutz

Permission is granted to copy, distribute and/or modify this document
under the terms of the [GNU Free Documentation License, Version 1.2]
(http://www.gnu.org/licenses/old-licenses/fdl-1.2.html) or any later
version published by the Free Software Foundation; with no Invariant
Sections, with no Front-Cover Texts, and with no Back-Cover Texts.
