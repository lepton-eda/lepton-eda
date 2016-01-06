/* Copyright (C) 2013-2016 Roland Lutz

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  */

#include <stdlib.h>
#include <string.h>
#include <xornstorage.h>


int main()
{
	xorn_revision_t rev;
	rev = xorn_new_revision(NULL);

	{
/** [add object] */
struct xornsch_line line_data;
memset(&line_data, 0, sizeof line_data);
line_data.pos.x = 0;
line_data.pos.y = 0;
line_data.size.x = 100;
line_data.size.y = 100;
line_data.line.width = 1;

xorn_object_t ob;
ob = xorn_add_object(rev, xornsch_obtype_line, &line_data);
if (ob == NULL)
    /* handle error */;
/** [add object] */

/** [get object data] */
const struct xornsch_line *data;

data = xorn_get_object_data(rev, ob, xornsch_obtype_line);

if (data == NULL)
    /* ob doesn't exist or isn't a line */;
/** [get object data] */
	}

	{
	struct xornsch_net net_data;
	xorn_object_t ob;

	memset(&net_data, 0, sizeof net_data);
	ob = xornsch_add_net(rev, &net_data);

/** [set object data] */
struct xornsch_line line_data;
memset(&line_data, 0, sizeof line_data);
line_data.pos.x = 0;
line_data.pos.y = 0;
line_data.size.x = 100;
line_data.size.y = 100;
line_data.line.width = 1;

if (xorn_set_object_data(rev, ob, xornsch_obtype_line, &line_data) == -1)
    /* handle error */;
/** [set object data] */
	}

	{
/** [get objects] */
xorn_object_t *objects;
size_t count;
unsigned int i;

if (xorn_get_objects(rev, &objects, &count) == -1)
    /* handle error */;

for (i = 0; i < count; i++)
    /* do something with objects[i] */;

free(objects);
/** [get objects] */
	}

	{
	struct xornsch_component component_data;
	xorn_object_t component;

	memset(&component_data, 0, sizeof component_data);
	component = xornsch_add_component(rev, &component_data);

/** [add attribute] */
struct xornsch_text text_data;
memset(&text_data, 0, sizeof text_data);
text_data.text.s = "refdes=R1";
text_data.text.len = strlen(text_data.text.s);

xorn_object_t ob;
ob = xornsch_add_text(rev, &text_data);
if (ob == NULL)
    /* handle error */;

if (xorn_relocate_object(rev, ob, component, NULL) == -1)
    /* handle error */;
/** [add attribute] */
	}

	xorn_free_revision(rev);
	return 0;
}
