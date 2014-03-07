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

#include "Setup.h"


int main()
{
	xorn_revision_t rev0, rev1, rev2, rev3;
	xorn_object_t ob0, ob1a, ob1b;

	xorn_selection_t sel;

	setup(&rev0, &rev1, &rev2, &rev3, &ob0, &ob1a, &ob1b);

	assert(sel = xorn_select_none());
	assert(xorn_object_is_selected(rev0, sel, ob0) == false);
	assert(xorn_object_is_selected(rev0, sel, ob1a) == false);
	assert(xorn_object_is_selected(rev0, sel, ob1b) == false);
	assert(xorn_object_is_selected(rev1, sel, ob0) == false);
	assert(xorn_object_is_selected(rev1, sel, ob1a) == false);
	assert(xorn_object_is_selected(rev1, sel, ob1b) == false);
	assert(xorn_object_is_selected(rev2, sel, ob0) == false);
	assert(xorn_object_is_selected(rev2, sel, ob1a) == false);
	assert(xorn_object_is_selected(rev2, sel, ob1b) == false);
	assert(xorn_object_is_selected(rev3, sel, ob0) == false);
	assert(xorn_object_is_selected(rev3, sel, ob1a) == false);
	assert(xorn_object_is_selected(rev3, sel, ob1b) == false);
	xorn_free_selection(sel);

	assert(sel = xorn_select_object(ob1a));
	assert(xorn_object_is_selected(rev0, sel, ob0) == false);
	assert(xorn_object_is_selected(rev0, sel, ob1a) == false);
	assert(xorn_object_is_selected(rev0, sel, ob1b) == false);
	assert(xorn_object_is_selected(rev1, sel, ob0) == false);
	assert(xorn_object_is_selected(rev1, sel, ob1a) == false);
	assert(xorn_object_is_selected(rev1, sel, ob1b) == false);
	assert(xorn_object_is_selected(rev2, sel, ob0) == false);
	assert(xorn_object_is_selected(rev2, sel, ob1a) == true);
	assert(xorn_object_is_selected(rev2, sel, ob1b) == false);
	assert(xorn_object_is_selected(rev3, sel, ob0) == false);
	assert(xorn_object_is_selected(rev3, sel, ob1a) == false);
	assert(xorn_object_is_selected(rev3, sel, ob1b) == false);
	xorn_free_selection(sel);

	assert(sel = xorn_select_all(rev3));
	assert(xorn_object_is_selected(rev0, sel, ob0) == false);
	assert(xorn_object_is_selected(rev0, sel, ob1a) == false);
	assert(xorn_object_is_selected(rev0, sel, ob1b) == false);
	assert(xorn_object_is_selected(rev1, sel, ob0) == true);
	assert(xorn_object_is_selected(rev1, sel, ob1a) == false);
	assert(xorn_object_is_selected(rev1, sel, ob1b) == false);
	assert(xorn_object_is_selected(rev2, sel, ob0) == true);
	assert(xorn_object_is_selected(rev2, sel, ob1a) == false);
	assert(xorn_object_is_selected(rev2, sel, ob1b) == true);
	assert(xorn_object_is_selected(rev3, sel, ob0) == true);
	assert(xorn_object_is_selected(rev3, sel, ob1a) == false);
	assert(xorn_object_is_selected(rev3, sel, ob1b) == true);
	xorn_free_selection(sel);

	xorn_free_revision(rev3);
	xorn_free_revision(rev2);
	xorn_free_revision(rev1);
	xorn_free_revision(rev0);
	return 0;
}
