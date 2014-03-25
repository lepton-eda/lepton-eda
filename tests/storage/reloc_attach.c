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
#include <stdlib.h>
#include <string.h>

#define _ NULL


static void check(xorn_revision_t rev, xorn_object_t ob,
		  xorn_object_t attach_to, xorn_object_t insert_before,
		  int result,
		  xorn_object_t ob0, xorn_object_t ob1, xorn_object_t ob2)
{
	xorn_object_t *objects;
	size_t count;

	assert(xorn_relocate_object(
		       rev, ob, attach_to, insert_before) == result);

	assert(xorn_get_objects(rev, &objects, &count) == 0);
	assert(objects != NULL);
	assert(count == 3);
	assert(objects[0] == ob0);
	assert(objects[1] == ob1);
	assert(objects[2] == ob2);
	free(objects);
}

static void common_checks(
	xorn_revision_t rev,
	xorn_object_t N, xorn_object_t a, xorn_object_t b,
	xorn_object_t ob0, xorn_object_t ob1, xorn_object_t ob2)
{
	/* can't relocate NULL */

	check(rev, _, _, _, -1, ob0, ob1, ob2);
	check(rev, _, _, N, -1, ob0, ob1, ob2);
	check(rev, _, _, a, -1, ob0, ob1, ob2);
	check(rev, _, _, b, -1, ob0, ob1, ob2);

	check(rev, _, N, _, -1, ob0, ob1, ob2);
	check(rev, _, N, N, -1, ob0, ob1, ob2);
	check(rev, _, N, a, -1, ob0, ob1, ob2);
	check(rev, _, N, b, -1, ob0, ob1, ob2);

	check(rev, _, a, _, -1, ob0, ob1, ob2);
	check(rev, _, a, N, -1, ob0, ob1, ob2);
	check(rev, _, a, a, -1, ob0, ob1, ob2);
	check(rev, _, a, b, -1, ob0, ob1, ob2);

	check(rev, _, b, _, -1, ob0, ob1, ob2);
	check(rev, _, b, N, -1, ob0, ob1, ob2);
	check(rev, _, b, a, -1, ob0, ob1, ob2);
	check(rev, _, b, b, -1, ob0, ob1, ob2);

	/* can't embed N to itself */

	check(rev, N, N, _, -1, ob0, ob1, ob2);
	check(rev, N, N, N, -1, ob0, ob1, ob2);
	check(rev, N, N, a, -1, ob0, ob1, ob2);
	check(rev, N, N, b, -1, ob0, ob1, ob2);

	/* can't embed N to a */

	check(rev, N, a, _, -1, ob0, ob1, ob2);
	check(rev, N, a, N, -1, ob0, ob1, ob2);
	check(rev, N, a, a, -1, ob0, ob1, ob2);
	check(rev, N, a, b, -1, ob0, ob1, ob2);

	/* can't embed N to b */

	check(rev, N, b, _, -1, ob0, ob1, ob2);
	check(rev, N, b, N, -1, ob0, ob1, ob2);
	check(rev, N, b, a, -1, ob0, ob1, ob2);
	check(rev, N, b, b, -1, ob0, ob1, ob2);

	/* can't embed a to itself */

	check(rev, a, a, _, -1, ob0, ob1, ob2);
	check(rev, a, a, N, -1, ob0, ob1, ob2);
	check(rev, a, a, a, -1, ob0, ob1, ob2);
	check(rev, a, a, b, -1, ob0, ob1, ob2);

	/* can't embed a to b */

	check(rev, a, b, _, -1, ob0, ob1, ob2);
	check(rev, a, b, N, -1, ob0, ob1, ob2);
	check(rev, a, b, a, -1, ob0, ob1, ob2);
	check(rev, a, b, b, -1, ob0, ob1, ob2);

	/* can't embed b to a */

	check(rev, b, a, _, -1, ob0, ob1, ob2);
	check(rev, b, a, N, -1, ob0, ob1, ob2);
	check(rev, b, a, a, -1, ob0, ob1, ob2);
	check(rev, b, a, b, -1, ob0, ob1, ob2);

	/* can't embed b to itself */

	check(rev, b, b, _, -1, ob0, ob1, ob2);
	check(rev, b, b, N, -1, ob0, ob1, ob2);
	check(rev, b, b, a, -1, ob0, ob1, ob2);
	check(rev, b, b, b, -1, ob0, ob1, ob2);

	/* can't embed something to N before N */

	check(rev, a, N, N, -1, ob0, ob1, ob2);
	check(rev, b, N, N, -1, ob0, ob1, ob2);
}

static void check_delete_0(xorn_revision_t rev, xorn_object_t del)
{
	xorn_revision_t r;
	xorn_object_t *objects;
	size_t count;

	assert(r = xorn_new_revision(rev));
	xorn_delete_object(r, del);

	assert(xorn_get_objects(r, &objects, &count) == 0);
	assert(count == 0);
	free(objects);

	xorn_free_revision(r);
}

static void check_delete_1(xorn_revision_t rev, xorn_object_t del,
			   xorn_object_t ob0)
{
	xorn_revision_t r;
	xorn_object_t *objects;
	size_t count;

	assert(r = xorn_new_revision(rev));
	xorn_delete_object(r, del);

	assert(xorn_get_objects(r, &objects, &count) == 0);
	assert(objects != NULL);
	assert(count == 1);
	assert(objects[0] == ob0);
	free(objects);

	xorn_free_revision(r);
}

static void check_delete_2(xorn_revision_t rev, xorn_object_t del,
			   xorn_object_t ob0, xorn_object_t ob1)
{
	xorn_revision_t r;
	xorn_object_t *objects;
	size_t count;

	assert(r = xorn_new_revision(rev));
	xorn_delete_object(r, del);

	assert(xorn_get_objects(r, &objects, &count) == 0);
	assert(objects != NULL);
	assert(count == 2);
	assert(objects[0] == ob0);
	assert(objects[1] == ob1);
	free(objects);

	xorn_free_revision(r);
}

static void check0(xorn_revision_t rev,
		   xorn_object_t N, xorn_object_t a, xorn_object_t b)
{
	/* N() a b */
	common_checks(rev, N, a, b, N, a, b);

	check(rev, N, _, _,  0, a, b, N);
	check(rev, N, _, b,  0, a, N, b);
	check(rev, N, _, a,  0, N, a, b);

	check(rev, N, _, N,  0, N, a, b);
	check(rev, a, _, a,  0, N, a, b);
	check(rev, a, N, a, -1, N, a, b);
	check(rev, b, _, b,  0, N, a, b);
	check(rev, b, N, b, -1, N, a, b);

	check_delete_2(rev, N, a, b);
}

static void check1(xorn_revision_t rev,
		   xorn_object_t N, xorn_object_t a, xorn_object_t b)
{
	/* a N() b */
	common_checks(rev, N, a, b, a, N, b);

	check(rev, N, _, _,  0, a, b, N);
	check(rev, N, _, a,  0, N, a, b);
	check(rev, N, _, b,  0, a, N, b);

	check(rev, N, _, N,  0, a, N, b);
	check(rev, a, _, a,  0, a, N, b);
	check(rev, a, N, a, -1, a, N, b);
	check(rev, b, _, b,  0, a, N, b);
	check(rev, b, N, b, -1, a, N, b);

	check_delete_2(rev, N, a, b);
}

static void check2(xorn_revision_t rev,
		   xorn_object_t N, xorn_object_t a, xorn_object_t b)
{
	/* a b N() */
	common_checks(rev, N, a, b, a, b, N);

	check(rev, N, _, b,  0, a, N, b);
	check(rev, N, _, a,  0, N, a, b);
	check(rev, N, _, _,  0, a, b, N);

	check(rev, N, _, N,  0, a, b, N);
	check(rev, a, _, a,  0, a, b, N);
	check(rev, a, N, a, -1, a, b, N);
	check(rev, b, _, b,  0, a, b, N);
	check(rev, b, N, b, -1, a, b, N);

	check_delete_2(rev, N, a, b);
}

static void check3(xorn_revision_t rev,
		   xorn_object_t N, xorn_object_t a, xorn_object_t b)
{
	/* N(a) b */
	common_checks(rev, N, a, b, N, a, b);

	check(rev, N, _, _,  0, b, N, a);
	check(rev, N, _, a, -1, b, N, a);
	check(rev, N, _, b,  0, N, a, b);

	check(rev, N, _, N,  0, N, a, b);
	check(rev, a, _, a, -1, N, a, b);
	check(rev, a, N, a,  0, N, a, b);
	check(rev, b, _, b,  0, N, a, b);
	check(rev, b, N, b, -1, N, a, b);

	check_delete_1(rev, N, b);
}

static void check4(xorn_revision_t rev,
		   xorn_object_t N, xorn_object_t a, xorn_object_t b)
{
	/* a N(b) */
	common_checks(rev, N, a, b, a, N, b);

	check(rev, N, _, a,  0, N, b, a);
	check(rev, N, _, b, -1, N, b, a);
	check(rev, N, _, _,  0, a, N, b);

	check(rev, N, _, N,  0, a, N, b);
	check(rev, a, _, a,  0, a, N, b);
	check(rev, a, N, a, -1, a, N, b);
	check(rev, b, _, b, -1, a, N, b);
	check(rev, b, N, b,  0, a, N, b);

	check_delete_1(rev, N, a);
}

static void check5(xorn_revision_t rev,
		   xorn_object_t N, xorn_object_t a, xorn_object_t b)
{
	/* N(a b) */
	common_checks(rev, N, a, b, N, a, b);

	check(rev, N, _, _,  0, N, a, b);
	check(rev, N, _, a, -1, N, a, b);
	check(rev, N, _, b, -1, N, a, b);

	check(rev, N, _, N,  0, N, a, b);
	check(rev, a, _, a, -1, N, a, b);
	check(rev, a, N, a,  0, N, a, b);
	check(rev, b, _, b, -1, N, a, b);
	check(rev, b, N, b,  0, N, a, b);

	check_delete_0(rev, N);
}

static void do_it(xorn_revision_t rev, xorn_object_t ob,
		  xorn_object_t attach_to, xorn_object_t insert_before,
		  int result,
		  xorn_object_t ob0, xorn_object_t ob1, xorn_object_t ob2,
		  void (*fun)(xorn_revision_t rev, xorn_object_t N,
			      xorn_object_t a, xorn_object_t b),
		  xorn_object_t fN, xorn_object_t fa, xorn_object_t fb)
{
	check(rev, ob, attach_to, insert_before, result, ob0, ob1, ob2);
	(*fun)(rev, fN, fa, fb);

	check(rev, ob, attach_to, insert_before, result, ob0, ob1, ob2);
	(*fun)(rev, fN, fa, fb);
}

int main()
{
	xorn_revision_t rev;
	struct xornsch_net net_data;
	struct xornsch_text text_data;
	xorn_object_t N, a, b;

	xorn_revision_t rev1;
	struct xornsch_line line_data;
	struct xornsch_component component_data;

	assert(rev = xorn_new_revision(NULL));

	memset(&net_data, 0, sizeof net_data);
	assert(N = xornsch_add_net(rev, &net_data));

	memset(&text_data, 0, sizeof text_data);
	assert(a = xornsch_add_text(rev, &text_data));
	assert(b = xornsch_add_text(rev, &text_data));

	common_checks(rev, N, a, b, N, a, b);

	/* can move objects */

	do_it(rev, N, _, _,  0, a, b, N, &check2, N, a, b);
	do_it(rev, N, _, a,  0, N, a, b, &check0, N, a, b);
	do_it(rev, N, _, b,  0, a, N, b, &check1, N, a, b);

	do_it(rev, a, _, _,  0, N, b, a, &check0, N, b, a);
	do_it(rev, a, _, N,  0, a, N, b, &check1, N, a, b);
	do_it(rev, a, _, b,  0, N, a, b, &check0, N, a, b);

	do_it(rev, b, _, N,  0, b, N, a, &check1, N, b, a);
	do_it(rev, b, _, a,  0, N, b, a, &check0, N, b, a);
	do_it(rev, b, _, _,  0, N, a, b, &check0, N, a, b);

	/* can embed a to N, but not before b */

	do_it(rev, a, N, _,  0, N, a, b, &check3, N, a, b);
	do_it(rev, a, N, b, -1, N, a, b, &check3, N, a, b);

	do_it(rev, b, _, N,  0, b, N, a, &check4, N, b, a);
	do_it(rev, b, _, a, -1, b, N, a, &check4, N, b, a);
	do_it(rev, b, _, _,  0, N, a, b, &check3, N, a, b);

	do_it(rev, a, _, b,  0, N, a, b, &check0, N, a, b);

	/* can embed b to N, but not before a */

	do_it(rev, b, N, _,  0, N, b, a, &check3, N, b, a);
	do_it(rev, b, N, a, -1, N, b, a, &check3, N, b, a);

	do_it(rev, a, _, N,  0, a, N, b, &check4, N, a, b);
	do_it(rev, a, _, b, -1, a, N, b, &check4, N, a, b);
	do_it(rev, a, _, _,  0, N, b, a, &check3, N, b, a);

	do_it(rev, b, _, _,  0, N, a, b, &check0, N, a, b);

	/* can embed both */

	do_it(rev, a, N, _,  0, N, a, b, &check3, N, a, b);
	do_it(rev, b, N, _,  0, N, a, b, &check5, N, a, b);

	do_it(rev, a, N, _,  0, N, b, a, &check5, N, b, a);
	do_it(rev, a, N, b,  0, N, a, b, &check5, N, a, b);
	do_it(rev, b, N, a,  0, N, b, a, &check5, N, b, a);
	do_it(rev, b, N, _,  0, N, a, b, &check5, N, a, b);

	do_it(rev, a, _, _,  0, N, b, a, &check3, N, b, a);
	do_it(rev, b, _, _,  0, N, a, b, &check0, N, a, b);

	xorn_finalize_revision(rev);

	common_checks(rev, N, a, b, N, a, b);

	check(rev, N, _, _, -1, N, a, b);
	check(rev, N, _, a, -1, N, a, b);
	check(rev, N, _, b, -1, N, a, b);

	check(rev, a, _, _, -1, N, a, b);
	check(rev, a, _, N, -1, N, a, b);
	check(rev, a, _, b, -1, N, a, b);
	check(rev, a, N, _, -1, N, a, b);

	check(rev, b, _, _, -1, N, a, b);
	check(rev, b, _, N, -1, N, a, b);
	check(rev, b, _, a, -1, N, a, b);
	check(rev, b, N, _, -1, N, a, b);

	assert(rev1 = xorn_new_revision(rev));

	/* can't attach text to line */

	memset(&line_data, 0, sizeof line_data);
	assert(xornsch_set_line_data(rev1, N, &line_data) == 0);
	do_it(rev1, a, N, _, -1, N, a, b, &check0, N, a, b);

	/* can attach text to component */

	memset(&component_data, 0, sizeof component_data);
	assert(xornsch_set_component_data(rev1, N, &component_data) == 0);
	do_it(rev1, a, N, _,  0, N, a, b, &check3, N, a, b);

	xorn_free_revision(rev1);
	xorn_free_revision(rev);
	return 0;
}
