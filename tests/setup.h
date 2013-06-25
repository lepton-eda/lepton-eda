/* Copyright (C) 2013 Roland Lutz

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  */

#include <xornstorage.h>
#include <assert.h>


static void setup(
	xorn_file_t *file, xorn_revision_t *rev0,
	xorn_revision_t *rev1, xorn_revision_t *rev2, xorn_revision_t *rev3,
	xorn_object_t *ob0, xorn_object_t *ob1a, xorn_object_t *ob1b)
{
	xorn_changeset_t chset;

	*file = xorn_new_file();
	assert(*file != NULL);

	*rev0 = xorn_get_empty_revision(*file);
	assert(*rev0 != NULL);

	/* first change */

	chset = xorn_alloc_changeset(*rev0);
	assert(chset != NULL);

	*ob0 = xorn_add_object(chset, 111, NULL);
	assert(*ob0 != NULL);

	*rev1 = xorn_apply_changeset(chset, "first change");
	assert(*rev1 != NULL);

	/* second change */

	chset = xorn_alloc_changeset(*rev1);
	assert(chset != NULL);

	*ob1a = xorn_add_object(chset, 123, NULL);
	assert(*ob1a != NULL);

	*ob1b = xorn_add_object(chset, 321, NULL);
	assert(*ob1b != NULL);

	*rev2 = xorn_apply_changeset(chset, "second change");
	assert(*rev2 != NULL);

	/* third change */

	chset = xorn_alloc_changeset(*rev2);
	assert(chset != NULL);

	assert(xorn_set_object_data(chset, *ob0, 333, NULL) == 0);

	xorn_delete_object(chset, *ob1a);

	*rev3 = xorn_apply_changeset(chset, "third change");
	assert(*rev3 != NULL);
}
