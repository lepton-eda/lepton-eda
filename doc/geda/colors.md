gEDA colors {#geda-colors}
===========

The color of a gEDA/gaf object is specified via an integer index.  The
relationship between integer and color is based on object type.  Each
object type typically has one or more colors.  Here is a table of
color index to object type:

Index|Object type
-|-
0|BACKGROUND_COLOR
1|PIN_COLOR
2|NET_ENDPOINT_COLOR
3|GRAPHIC_COLOR
4|NET_COLOR
5|ATTRIBUTE_COLOR
6|LOGIC_BUBBLE_COLOR
7|DOTS_GRID_COLOR
8|DETACHED_ATTRIBUTE_COLOR
9|TEXT_COLOR
10|BUS_COLOR
11|SELECT_COLOR
12|BOUNDINGBOX_COLOR
13|ZOOM_BOX_COLOR
14|STROKE_COLOR
15|LOCK_COLOR
16|OUTPUT_BACKGROUND_COLOR
17|FREESTYLE1_COLOR
18|FREESTYLE2_COLOR
19|FREESTYLE3_COLOR
20|FREESTYLE4_COLOR
21|JUNCTION_COLOR
22|MESH_GRID_MAJOR_COLOR
23|MESH_GRID_MINOR_COLOR

The actual color associated with the color index is defined on a per
tool bases.  Objects are typically assigned their corresponding color
index, but it is (sometimes) permissible to assign other color index
values to different object types.

--------------------------------------------------------------------------------

Copyright (C) 1998-2004 Ales V. Hvezda \n
Copyright (C) 2013-2016 Roland Lutz

Permission is granted to copy, distribute and/or modify this document
under the terms of the [GNU Free Documentation License, Version 1.2]
(http://www.gnu.org/licenses/old-licenses/fdl-1.2.html) or any later
version published by the Free Software Foundation; with no Invariant
Sections, with no Front-Cover Texts, and with no Back-Cover Texts.
