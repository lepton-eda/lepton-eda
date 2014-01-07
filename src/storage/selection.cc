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
#include "key_iterator.h"
#include <algorithm>


xorn_selection_t xorn_select_none()
{
	try {
		return new xorn_selection();
	} catch (std::bad_alloc const &) {
		return NULL;
	}
}

xorn_selection_t xorn_select_object(xorn_object_t ob)
{
	xorn_selection_t rsel;
	try {
		rsel = new xorn_selection();
	} catch (std::bad_alloc const &) {
		return NULL;
	}
	try {
		rsel->insert(ob);
	} catch (std::bad_alloc const &) {
		delete rsel;
		return NULL;
	}
	return rsel;
}

xorn_selection_t xorn_select_all(xorn_revision_t rev)
{
	xorn_selection_t rsel;
	try {
		rsel = new xorn_selection();
	} catch (std::bad_alloc const &) {
		return NULL;
	}
	try {
		copy(iterate_keys(rev->obstates.begin()),
		     iterate_keys(rev->obstates.end()),
		     inserter(*rsel, rsel->begin()));
	} catch (std::bad_alloc const &) {
		delete rsel;
		return NULL;
	}
	return rsel;
}

xorn_selection_t xorn_select_all_except(
	xorn_revision_t rev, xorn_selection_t sel)
{
	xorn_selection_t rsel;
	try {
		rsel = new xorn_selection();
	} catch (std::bad_alloc const &) {
		return NULL;
	}
	try {
		set_difference(iterate_keys(rev->obstates.begin()),
			       iterate_keys(rev->obstates.end()),
			       sel->begin(), sel->end(),
			       inserter(*rsel, rsel->begin()));
	} catch (std::bad_alloc const &) {
		delete rsel;
		return NULL;
	}
	return rsel;
}

xorn_selection_t xorn_select_union(
	xorn_selection_t sel0, xorn_selection_t sel1)
{
	xorn_selection_t rsel;
	try {
		rsel = new xorn_selection();
	} catch (std::bad_alloc const &) {
		return NULL;
	}
	try {
		set_union(sel0->begin(), sel0->end(),
			  sel1->begin(), sel1->end(),
			  inserter(*rsel, rsel->begin()));
	} catch (std::bad_alloc const &) {
		delete rsel;
		return NULL;
	}
	return rsel;
}

xorn_selection_t xorn_select_intersection(
	xorn_selection_t sel0, xorn_selection_t sel1)
{
	xorn_selection_t rsel;
	try {
		rsel = new xorn_selection();
	} catch (std::bad_alloc const &) {
		return NULL;
	}
	try {
		set_intersection(sel0->begin(), sel0->end(),
				 sel1->begin(), sel1->end(),
				 inserter(*rsel, rsel->begin()));
	} catch (std::bad_alloc const &) {
		delete rsel;
		return NULL;
	}
	return rsel;
}

bool xorn_selection_is_empty(xorn_revision_t rev, xorn_selection_t sel)
{
	std::map<xorn_object_t, obstate *>::const_iterator i
		= rev->obstates.begin();
	std::set<xorn_object_t>::const_iterator j = sel->begin();

	while (i != rev->obstates.end() && j != sel->end())
		if ((*i).first < *j)
			++i;
		else if ((*i).first > *j)
			++j;
		else
			return false;

	return true;
}

void xorn_free_selection(xorn_selection_t sel)
{
	delete sel;
}
