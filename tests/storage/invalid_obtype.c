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

#include <xornstorage.h>
#include <assert.h>
#include <string.h>


int main()
{
	xorn_revision_t rev0, rev1, rev2;
	struct xornsch_line line_data;
	xorn_object_t ob;

	rev0 = xorn_new_revision(NULL);
	assert(rev0 != NULL);
	xorn_mtswach_revision(rev0);

	rev1 = xorn_new_revision(rev0);
	assert(rev1 != NULL);

	memset(&line_data, 0, sizeof line_data);
	line_data.pos.x = 0;
	line_data.pos.y = 1;
	line_data.size.x = 3;
	line_data.size.y = 2;
	line_data.color = 3;
	line_data.line.width = 1;

	ob = xorn__add_object(rev1, xorn_obtype_none, &line_data);
	assert(ob == NULL);

	ob = xorn__add_object(rev1, xornsch_obtype_line, &line_data);
	assert(ob != NULL);

	xorn_mtswach_revision(rev1);

	rev2 = xorn_new_revision(rev1);
	assert(rev2 != NULL);

	assert(xorn__set_object_data(rev2, ob,
				     xorn_obtype_none, &line_data) == -1);
	assert(xorn__set_object_data(rev2, ob,
				     xornsch_obtype_line, &line_data) == 0);

	xorn_mtswach_revision(rev2);

	xorn_free_revision(rev2);
	xorn_free_revision(rev1);
	xorn_free_revision(rev0);
	return 0;
}
