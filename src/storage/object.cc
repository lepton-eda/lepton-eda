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

#include "internal.h"
#include <stdlib.h>
#include <algorithm>
#include "key_iterator.h"


bool xorn_object_exists_in_revision(xorn_revision_t rev, xorn_object_t ob)
{
	return rev->obstates.find(ob) != rev->obstates.end();
}

xorn_obtype_t xorn_get_object_type(xorn_revision_t rev, xorn_object_t ob)
{
	std::map<xorn_object_t, obstate *>::const_iterator i
		= rev->obstates.find(ob);

	if (i == rev->obstates.end())
		return xorn_obtype_none;

	return (*i).second->type;
}

void const *xorn_get_object_data(xorn_revision_t rev, xorn_object_t ob,
				 xorn_obtype_t type)
{
	std::map<xorn_object_t, obstate *>::const_iterator i
		= rev->obstates.find(ob);

	if (i == rev->obstates.end() || (*i).second->type != type)
		return NULL;

	return (*i).second->data;
}

/* It is the caller's responsibility to free the returned list. */

void xorn_get_objects(
	xorn_revision_t rev,
	xorn_object_t **objects_return, size_t *count_return)
{
	*objects_return = (xorn_object_t *) malloc(
		rev->obstates.size() * sizeof(xorn_object_t));
	*count_return = 0;
	if (*objects_return == NULL)
		return;

	for (std::map<xorn_object_t, obstate *>::const_iterator i
		     = rev->obstates.begin(); i != rev->obstates.end(); ++i)
		(*objects_return)[(*count_return)++] = (*i).first;
}

void xorn_get_selected_objects(
	xorn_revision_t rev, xorn_selection_t sel,
	xorn_object_t **objects_return, size_t *count_return)
{
	*objects_return = (xorn_object_t *) malloc(
		std::min(rev->obstates.size(),
			 sel->size()) * sizeof(xorn_object_t));
	*count_return = 0;
	if (*objects_return == NULL)
		return;

	xorn_object_t *ptr = set_intersection(
		iterate_keys(rev->obstates.begin()),
		iterate_keys(rev->obstates.end()),
		sel->begin(), sel->end(), *objects_return);

	*count_return = ptr - *objects_return;
	*objects_return = (xorn_object_t *) realloc(
		*objects_return,
		std::max(*count_return, (size_t) 1) * sizeof(xorn_object_t));
}

void xorn_get_added_objects(
	xorn_revision_t from_rev, xorn_revision_t to_rev,
	xorn_object_t **objects_return, size_t *count_return)
{
	*objects_return = (xorn_object_t *) malloc(
		to_rev->obstates.size() * sizeof(xorn_object_t));
	*count_return = 0;
	if (*objects_return == NULL)
		return;

	xorn_object_t *ptr = set_difference(
		iterate_keys(to_rev->obstates.begin()),
		iterate_keys(to_rev->obstates.end()),
		iterate_keys(from_rev->obstates.begin()),
		iterate_keys(from_rev->obstates.end()), *objects_return);

	*count_return = ptr - *objects_return;
	*objects_return = (xorn_object_t *) realloc(
		*objects_return,
		std::max(*count_return, (size_t) 1) * sizeof(xorn_object_t));
}

void xorn_get_removed_objects(
	xorn_revision_t from_rev, xorn_revision_t to_rev,
	xorn_object_t **objects_return, size_t *count_return)
{
	*objects_return = (xorn_object_t *) malloc(
		from_rev->obstates.size() * sizeof(xorn_object_t));
	*count_return = 0;
	if (*objects_return == NULL)
		return;

	xorn_object_t *ptr = set_difference(
		iterate_keys(from_rev->obstates.begin()),
		iterate_keys(from_rev->obstates.end()),
		iterate_keys(to_rev->obstates.begin()),
		iterate_keys(to_rev->obstates.end()), *objects_return);

	*count_return = ptr - *objects_return;
	*objects_return = (xorn_object_t *) realloc(
		*objects_return,
		std::max(*count_return, (size_t) 1) * sizeof(xorn_object_t));
}

void xorn_get_modified_objects(
	xorn_revision_t from_rev, xorn_revision_t to_rev,
	xorn_object_t **objects_return, size_t *count_return)
{
	*objects_return = (xorn_object_t *) malloc(
		std::min(from_rev->obstates.size(),
			 to_rev->obstates.size()) * sizeof(xorn_object_t));
	*count_return = 0;
	if (*objects_return == NULL)
		return;

	std::map<xorn_object_t, obstate *>::const_iterator i
		= from_rev->obstates.begin();
	std::map<xorn_object_t, obstate *>::const_iterator j
		= to_rev->obstates.begin();

	while (i != from_rev->obstates.end() && j != to_rev->obstates.end())
		if ((*i).first < (*j).first)
			++i;
		else if ((*i).first > (*j).first)
			++j;
		else {
			if ((*i).second != (*j).second)
				(*objects_return)[(*count_return)++] =
					(*i).first;
			++i;
			++j;
		}

	*objects_return = (xorn_object_t *) realloc(
		*objects_return,
		std::max(*count_return, (size_t) 1) * sizeof(xorn_object_t));
}
