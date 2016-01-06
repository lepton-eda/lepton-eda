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


/* This test is only meaningful when being run under memcheck. */

int main()
{
	xorn_revision_t rev;
	struct xornsch_text data;
	xorn_object_t ob;
	char *s;

	rev = xorn_new_revision(NULL);
	memset(&data, 0, sizeof data);

	data.text.s = "Hello";
	data.text.len = 5;
	ob = xornsch_add_text(rev, &data);

	data.text.s = "World";
	data.text.len = 5;
	xornsch_set_text_data(rev, ob, &data);

	s = strdup("!");
	data.text.s = s;
	data.text.len = 1;
	xornsch_set_text_data(rev, ob, &data);
	free(s);

	xorn_free_revision(rev);
	return 0;
}
