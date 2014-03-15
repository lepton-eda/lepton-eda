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

static const char *next_object_id = NULL;


static void set_object_data(xorn_revision_t rev, xorn_object_t ob,
			    xorn_obtype_t type, void const *data)
{
	obstate *tmp = new obstate(type, data);
	try {
		obstate *&p = rev->obstates[ob];
		if (p != NULL)
			p->dec_refcnt();
		p = tmp;
	} catch (std::bad_alloc const &) {
		tmp->dec_refcnt();
		throw;
	}
}

xorn_object_t xorn_add_object(xorn_revision_t rev,
			      xorn_obtype_t type, void const *data)
{
	if (!rev->is_transient)
		return NULL;

	xorn_object_t ob = (xorn_object_t)++next_object_id;
	try {
		set_object_data(rev, ob, type, data);
	} catch (std::bad_alloc const &) {
		return NULL;
	}
	return ob;
}

int xorn_set_object_data(xorn_revision_t rev, xorn_object_t ob,
			 xorn_obtype_t type, void const *data)
{
	if (!rev->is_transient)
		return -1;

	try {
		set_object_data(rev, ob, type, data);
	} catch (std::bad_alloc const &) {
		return -1;
	}
	return 0;
}

void xorn_delete_object(xorn_revision_t rev, xorn_object_t ob)
{
	if (!rev->is_transient)
		return;

	std::map<xorn_object_t, obstate *>::iterator i
		= rev->obstates.find(ob);

	if (i != rev->obstates.end()) {
		(*i).second->dec_refcnt();
		rev->obstates.erase(i);
	}
}

void xorn_delete_selected_objects(xorn_revision_t rev, xorn_selection_t sel)
{
	if (!rev->is_transient)
		return;

	for (std::set<xorn_object_t>::const_iterator i = sel->begin();
	     i != sel->end(); ++i) {
		std::map<xorn_object_t, obstate *>::iterator j
			= rev->obstates.find(*i);
		if (j != rev->obstates.end()) {
			(*j).second->dec_refcnt();
			rev->obstates.erase(j);
		}
	}
}

static xorn_object_t copy_object(xorn_revision_t dest, obstate *obstate)
{
	xorn_object_t ob = (xorn_object_t)++next_object_id;
	try {
		dest->obstates[ob] = obstate;
		obstate->inc_refcnt();
	} catch (std::bad_alloc const &) {
		throw;
	}
	return ob;
}

xorn_object_t xorn_copy_object(xorn_revision_t dest,
			       xorn_revision_t src, xorn_object_t ob)
{
	if (!dest->is_transient)
		return NULL;

	std::map<xorn_object_t, obstate *>::const_iterator i
		= src->obstates.find(ob);

	if (i == src->obstates.end())
		return NULL;

	try {
		return copy_object(dest, (*i).second);
	} catch (std::bad_alloc const &) {
		return NULL;
	}
}

xorn_selection_t xorn_copy_objects(xorn_revision_t dest,
				   xorn_revision_t src, xorn_selection_t sel)
{
	if (!dest->is_transient)
		return NULL;

	xorn_selection_t rsel;
	try {
		rsel = new xorn_selection();
	} catch (std::bad_alloc const &) {
		return NULL;
	}

	std::map<xorn_object_t, obstate *>::const_iterator i
		= src->obstates.begin();
	std::set<xorn_object_t>::const_iterator j = sel->begin();

	while (i != src->obstates.end() && j != sel->end())
	    if ((*i).first < *j)
		++i;
	    else if ((*i).first > *j)
		++j;
	    else {
		try {
			xorn_object_t ob = copy_object(dest, (*i).second);
			try {
				rsel->insert(ob);
			} catch (std::bad_alloc const &) {
				xorn_delete_object(dest, ob);
				throw;
			}
		} catch (std::bad_alloc const &) {
			for (xorn_selection::const_iterator i = rsel->begin();
			     i != rsel->end(); ++i)
				xorn_delete_object(dest, *i);
			delete rsel;
			return NULL;
		}
		++i;
		++j;
	    }

	return rsel;
}
