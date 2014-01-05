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


xorn_revision::xorn_revision() : is_transient(true)
{
}

xorn_revision::xorn_revision(xorn_revision_t rev)
	: is_transient(true), obstates(rev->obstates)
{
	for (std::map<xorn_object_t, obstate *>::const_iterator i
		     = obstates.begin(); i != obstates.end(); ++i)
		(*i).second->inc_refcnt();
}

xorn_revision::~xorn_revision()
{
	for (std::map<xorn_object_t, obstate *>::const_iterator i
		     = obstates.begin(); i != obstates.end(); ++i)
		(*i).second->dec_refcnt();
}


xorn_revision_t xorn_new_revision(xorn_revision_t rev)
{
	try {
		if (rev == NULL)
			return new xorn_revision();
		else
			return new xorn_revision(rev);
	} catch (std::bad_alloc const &) {
		return NULL;
	}
}

bool xorn_revision_is_transient(xorn_revision_t rev)
{
	return rev->is_transient;
}

void xorn_mtswach_revision(xorn_revision_t rev)
{
	rev->is_transient = false;
}

void xorn_free_revision(xorn_revision_t rev)
{
	delete rev;
}
