/* Copyright (C) 2013, 2014 Roland Lutz

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
#include <xornstorage.h>


int main()
{
	xorn_revision_t rev;
	rev = xorn_new_revision(NULL);

/** [discrete] */
xorn_object_t *objects;
size_t count;
unsigned int i;

xorn_get_objects(rev, &objects, &count);

for (i = 0; i < count; i++) {
    xorn_obtype_t type = xorn_get_object_type(rev, objects[i]);

    if (type == xornsch_obtype_circle &&
        xornsch_get_circle_data(rev, objects[i])->radius == 0.)
        xorn_delete_object(rev, objects[i]);

    if (type == xornsch_obtype_arc &&
        xornsch_get_arc_data(rev, objects[i])->radius == 0.)
        xorn_delete_object(rev, objects[i]);
}

free(objects);
/** [discrete] */

/** [integrated] */
xorn_selection_t sel = xornsch_select_by_radius(rev, 0.);
xorn_delete_selected_objects(rev, sel);
xorn_free_selection(sel);
/** [integrated] */

	xorn_free_revision(rev);
	return 0;
}
