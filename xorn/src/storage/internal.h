/* Copyright (C) 2013-2015 Roland Lutz

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

#ifndef INTERNAL_H
#define INTERNAL_H

#include <xornstorage.h>
#include <map>
#include <vector>
#include <set>

void *copy_data(xorn_obtype_t type, void const *src);

class obstate {
	~obstate();
	unsigned int refcnt;
public:
	obstate(xorn_obtype_t type, void const *data);
	void inc_refcnt();
	void dec_refcnt();
	xorn_obtype_t const type;
	void *const data;
};

struct xorn_revision {
	xorn_revision();
	xorn_revision(xorn_revision_t rev);
	~xorn_revision();
	bool is_transient;
	std::map<xorn_object_t, obstate *> obstates;
	std::map<xorn_object_t, std::vector<xorn_object_t> > children;
	std::map<xorn_object_t, xorn_object_t> parent;
};

/* There is no struct xorn_object. */

struct xorn_selection : public std::set<xorn_object_t> {
};

#endif
