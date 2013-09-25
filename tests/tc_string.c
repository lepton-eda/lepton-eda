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
#include <string.h>

static struct xornsch_text text_data;
static const char *caption = "Hello world";


int main()
{
	xorn_file_t file;
	xorn_revision_t rev0, rev1;
	xorn_object_t ob;

	memset(&text_data, 0, sizeof text_data);
	text_data.text.s = caption;
	text_data.text.len = strlen(caption) + 1;

	file = xorn_new_file();
	assert(file != NULL);
	rev0 = xorn_get_empty_revision(file);
	assert(rev0 != NULL);
	rev1 = xorn_new_revision(rev0);
	assert(rev1 != NULL);
	ob = xorn_add_object(rev1, xornsch_obtype_text, &text_data);
	assert(ob != NULL);
	xorn_mtswach_revision(rev1);

	const struct xornsch_text *text_return =
	    xorn_get_object_data(rev1, ob, xornsch_obtype_text);
	assert(text_return != NULL);
	assert(text_return->text.s != NULL);
	assert(text_return->text.s != caption);
	assert(text_return->text.len == strlen(caption) + 1);
	assert(memcmp(text_return->text.s, caption,
		      text_return->text.len) == 0);

	xorn_close_file(file);
	return 0;
}
