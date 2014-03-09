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
	: is_transient(true), obstates(rev->obstates), sequence(rev->sequence)
{
	for (std::map<xorn_object_t, obstate *>::const_iterator i
		     = obstates.begin(); i != obstates.end(); ++i)
		i->second->inc_refcnt();
}

xorn_revision::~xorn_revision()
{
	for (std::map<xorn_object_t, obstate *>::const_iterator i
		     = obstates.begin(); i != obstates.end(); ++i)
		i->second->dec_refcnt();
}


/** \brief Create a new revision, either from scratch or by copying an
 *         existing one.
 *
 * You should free the revision using \ref xorn_free_revision once it
 * isn't used any more.
 *
 * \param rev Revision to copy, or \c NULL.
 *
 * There is a slight difference between creating two empty revisions
 * and copying an empty one: only in the second case, objects of one
 * revision will be valid in the other.
 *
 * \return Returns the newly created revision, or NULL if there is not
 *         enough memory.  */

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

/** \brief Return whether a revision can be changed.
 *
 * When a revision is created, it is initially \a transient,
 * i.e. changeable.  This can be changed by calling \ref
 * xorn_finalize_revision.  After that, it can't be changed directly
 * any more---you will have to create a transient copy if you want to
 * change it again.  */

bool xorn_revision_is_transient(xorn_revision_t rev)
{
	return rev->is_transient;
}

/** \brief Prevent further changes to a revision.
 *
 * When a revision is created, it is initially \a transient,
 * i.e. changeable.  However, it is typically not desired for a
 * revision to change once it is in its desired state.  Using this
 * function, you can prevent further changes to the revision by means
 * of the Xorn functions (though nobody can prevent you from poking
 * into memory and messing things up).  It will still be possible to
 * create a copy of the revision and change that.  */

void xorn_finalize_revision(xorn_revision_t rev)
{
	rev->is_transient = false;
}

/** \brief Free the memory associated with a revision.
 *
 * \warning \a rev must not be passed to any Xorn function again.
 *
 * Some memory may be shared between revisions.  This memory will be
 * released once all revisions using it have been freed.  */

void xorn_free_revision(xorn_revision_t rev)
{
	delete rev;
}
