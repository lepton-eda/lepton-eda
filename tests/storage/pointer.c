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

#include <xornstorage.h>
#include <assert.h>
#include <string.h>

static int refcnt0 = 0, refcnt1 = 0;


static void inc(int *val)
{
	(*val)++;
}

static void dec(int *val)
{
	(*val)--;
}

int main()
{
	xorn_revision_t rev;
	xorn_object_t ob0, ob1;
	struct xornsch_component component_data;
	struct xornsch_picture picture_data;
	xorn_selection_t sel0, sel1;

	assert(rev = xorn_new_revision(NULL));

	memset(&component_data, 0, sizeof component_data);
	component_data.symbol.ptr = &refcnt0;
	component_data.symbol.incref = (void (*)(void *))inc;
	component_data.symbol.decref = (void (*)(void *))dec;

	assert(ob0 = xornsch_add_component(rev, &component_data));
	assert(refcnt0 == 1);

	memset(&picture_data, 0, sizeof picture_data);
	picture_data.pixmap.ptr = &refcnt1;
	picture_data.pixmap.incref = (void (*)(void *))inc;
	picture_data.pixmap.decref = (void (*)(void *))dec;

	assert(ob1 = xornsch_add_picture(rev, &picture_data));
	assert(refcnt0 == 1);
	assert(refcnt1 == 1);

	assert(sel0 = xorn_select_all(rev));
	assert(sel1 = xorn_copy_objects(rev, rev, sel0));
	assert(refcnt0 != 0);
	assert(refcnt1 != 0);

	memset(&picture_data, 0, sizeof picture_data);
	assert(xornsch_set_picture_data(rev, ob1, &picture_data) == 0);
	assert(refcnt0 != 0);
	assert(refcnt1 != 0);

	xorn_delete_selected_objects(rev, sel1);
	assert(refcnt0 == 1);
	assert(refcnt1 == 0);
	xorn_free_selection(sel1);
	xorn_free_selection(sel0);

	xorn_free_revision(rev);
	assert(refcnt0 == 0);
	assert(refcnt1 == 0);

	return 0;
}
