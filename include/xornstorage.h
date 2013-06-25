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

#ifndef _XORN_STORAGE_H
#define _XORN_STORAGE_H

#include <unistd.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef enum {
	xorn_obtype_none,
} xorn_obtype_t;

/* opaque types */
typedef struct xorn_file *xorn_file_t;
typedef struct xorn_revision *xorn_revision_t;
typedef struct xorn_object *xorn_object_t;
typedef struct xorn_changeset *xorn_changeset_t;

/* file functions */

xorn_file_t xorn_new_file();
void xorn_close_file(xorn_file_t file);
xorn_revision_t xorn_get_empty_revision(xorn_file_t file);

/* object functions */

bool xorn_object_exists_in_revision(
	xorn_revision_t rev, xorn_object_t ob);
xorn_obtype_t xorn_get_object_type(
	xorn_revision_t rev, xorn_object_t ob);
const void *xorn_get_object_data(
	xorn_revision_t rev, xorn_object_t ob, xorn_obtype_t type);

void xorn_get_objects(
	xorn_revision_t rev,
	xorn_object_t **objects_return, size_t *count_return);
void xorn_get_added_objects(
	xorn_revision_t from_rev, xorn_revision_t to_rev,
	xorn_object_t **objects_return, size_t *count_return);
void xorn_get_removed_objects(
	xorn_revision_t from_rev, xorn_revision_t to_rev,
	xorn_object_t **objects_return, size_t *count_return);
void xorn_get_modified_objects(
	xorn_revision_t from_rev, xorn_revision_t to_rev,
	xorn_object_t **objects_return, size_t *count_return);

/* manipulation functions */

xorn_changeset_t xorn_alloc_changeset(xorn_revision_t rev);
xorn_revision_t xorn_apply_changeset(xorn_changeset_t chset,
				     const char *message);

xorn_object_t xorn_add_object(xorn_changeset_t chset,
			      xorn_obtype_t type, const void *data);
int xorn_set_object_data(xorn_changeset_t chset, xorn_object_t ob,
			 xorn_obtype_t type, const void *data);
void xorn_delete_object(xorn_changeset_t chset, xorn_object_t ob);

#ifdef __cplusplus
}
#endif

#endif
