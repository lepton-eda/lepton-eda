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
#include <algorithm>

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

/** \brief Add a new object to a transient revision.
 *
 * The object is appended to the end of the object list.
 *
 * \a data must point to a data structure matching the object type
 * indicated by \a type (e.g., if \a type is \c xornsch_obtype_net,
 * then \a data must point to a \c xornsch_net structure).  The data
 * structure (including referenced strings) will not be accessed after
 * this function has returned.
 *
 * \return Returns the newly created object.  If the revision isn't
 * transient, \a type is not a valid Xorn object type, \a data is
 * NULL, or there is not enough memory, returns \c NULL.
 *
 * Example:
 * \snippet functions.c add object
 *
 * \note Try not to use this function.  There are type-specific
 * functions available (\c xornsch_add_net etc.) which offer the same
 * functionality but are type-safe.  */

xorn_object_t xorn_add_object(xorn_revision_t rev,
			      xorn_obtype_t type, void const *data)
{
	if (!rev->is_transient)
		return NULL;

	xorn_object_t ob = (xorn_object_t)++next_object_id;
	try {
		rev->sequence.push_back(ob);
	} catch (std::bad_alloc const &) {
		return NULL;
	}
	try {
		set_object_data(rev, ob, type, data);
	} catch (std::bad_alloc const &) {
		rev->sequence.pop_back();
		return NULL;
	}
	return ob;
}

/** \brief Set an object in a transient revision to the given object
 *         type and data.
 *
 * If the object does not exist in the revision, it is created and
 * appended to the end of the object list.
 *
 * \param rev  Revision to be changed (must be transient)
 *
 * \param ob   An object which has previously been returned by a Xorn
 *             function for either this revision, one of its
 *             ancestors, or a revision which has a common ancestor
 *             with it.
 *
 * \param type New object type (may be different from previous one)
 *
 * \param data Pointer to a data structure matching the object type
 *             indicated by \a type (e.g., if \a type is \c
 *             xornsch_obtype_net, a pointer to a \c xornsch_net
 *             structure).  The data structure (including referenced
 *             strings) will not be accessed after this function has
 *             returned.
 *
 * \return Returns \c 0 if the object has been changed.  If the
 * revision isn't transient, \a type is not a valid Xorn object type,
 * \a data is NULL, or there is not enough memory, returns \c -1.
 *
 * Example:
 * \snippet functions.c set object data
 *
 * \note Try not to use this function.  There are type-specific
 * functions available (\c xornsch_set_net_data etc.) which offer the
 * same functionality but are type-safe.  */

int xorn_set_object_data(xorn_revision_t rev, xorn_object_t ob,
			 xorn_obtype_t type, void const *data)
{
	if (!rev->is_transient)
		return -1;

	bool add = rev->obstates.find(ob) == rev->obstates.end();
	if (add)
		try {
			rev->sequence.push_back(ob);
		} catch (std::bad_alloc const &) {
			return -1;
		}

	try {
		set_object_data(rev, ob, type, data);
	} catch (std::bad_alloc const &) {
		if (add)
			rev->sequence.pop_back();
		return -1;
	}
	return 0;
}

/** \brief Change the location of an object in the object structure of
 *         a transient revision.
 *
 * Changes the order in which an object is drawn and written to files
 * as compared to its sibling objects.
 *
 * If \a ob and \a insert_before are identical, the revision is left
 * unchanged.
 *
 * \param rev            Revision to modify (must be transient)
 * \param ob             The object which should be reordered
 * \param insert_before  An object before which \a ob should be
 *                       inserted, or \c NULL to append it at the end.
 *
 * \return Returns \c 0 on success.  Returns \c -1 if
 * - the revision isn't transient,
 * - \a ob or (if not \c NULL) \a insert_before doesn't exist in the
 *   revision, or
 * - there is not enough memory.  */

int xorn_relocate_object(xorn_revision_t rev, xorn_object_t ob,
			 xorn_object_t insert_before)
{
	if (!rev->is_transient)
		return -1;

	std::vector<xorn_object_t>::iterator i = find(rev->sequence.begin(),
						      rev->sequence.end(), ob);
	if (i == rev->sequence.end())
		return -1;
	std::vector<xorn_object_t>::size_type ip = i - rev->sequence.begin();

	/* Try adding the new entry first, removing the old one won't fail. */

	try {
		if (insert_before == NULL)
			rev->sequence.push_back(ob);
		else {
			std::vector<xorn_object_t>::iterator j =
				find(rev->sequence.begin(),
				     rev->sequence.end(), insert_before);
			if (j == rev->sequence.end())
				return -1;
			if (j <= i)
				++ip;
			rev->sequence.insert(j, ob);
		}
	} catch (std::bad_alloc const &) {
		return -1;
	}

	rev->sequence.erase(rev->sequence.begin() + ip);
	return 0;
}

/** \brief Delete an object from a transient revision.
 *
 * If the revision isn't transient or the object doesn't exist in the
 * revision, nothing is changed.
 *
 * \a ob will stay a valid object and can later be re-added using \ref
 * xorn_set_object_data or its type-safe equivalents.  */

void xorn_delete_object(xorn_revision_t rev, xorn_object_t ob)
{
	if (!rev->is_transient)
		return;

	std::map<xorn_object_t, obstate *>::iterator i
		= rev->obstates.find(ob);

	if (i != rev->obstates.end()) {
		i->second->dec_refcnt();
		rev->obstates.erase(i);
		rev->sequence.erase(find(rev->sequence.begin(),
					 rev->sequence.end(), ob));
	}
}

/** \brief Delete some objects from a transient revision.
 *
 * Objects that don't exist in the revision are ignored.  If the
 * revision isn't transient, nothing is changed.
 *
 * The objects will stay valid and can later be re-added using \ref
 * xorn_set_object_data or its type-safe equivalents.  */

void xorn_delete_selected_objects(xorn_revision_t rev, xorn_selection_t sel)
{
	if (!rev->is_transient)
		return;

	for (std::set<xorn_object_t>::const_iterator i = sel->begin();
	     i != sel->end(); ++i) {
		std::map<xorn_object_t, obstate *>::iterator j
			= rev->obstates.find(*i);
		if (j != rev->obstates.end()) {
			j->second->dec_refcnt();
			rev->obstates.erase(j);
			rev->sequence.erase(find(rev->sequence.begin(),
						 rev->sequence.end(), *i));
		}
	}
}

static xorn_object_t copy_object(xorn_revision_t dest, obstate *obstate)
{
	xorn_object_t ob = (xorn_object_t)++next_object_id;
	dest->sequence.push_back(ob);
	try {
		dest->obstates[ob] = obstate;
		obstate->inc_refcnt();
	} catch (std::bad_alloc const &) {
		dest->sequence.pop_back();
		throw;
	}
	return ob;
}

/** \brief Copy an object to a transient revision.
 *
 * The object is appended to the end of the object list.
 *
 * \param dest Destination revision (must be transient)
 * \param src Source revision (does not need to be transient)
 * \param ob Object in the source revision which should be copied
 *
 * \return Returns the newly created object.  If the destination
 * revision isn't transient, the object doesn't exist in the source
 * revision, or there is not enough memory, returns \c NULL.  */

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
		return copy_object(dest, i->second);
	} catch (std::bad_alloc const &) {
		return NULL;
	}
}

/** \brief Copy some objects to a transient revision.
 *
 * The objects are appended to the end of the object list in an
 * unspecified order.
 *
 * \param dest Destination revision (must be transient)
 * \param src Source revision (does not need to be transient)
 * \param sel Objects in the source revision which should be copied
 *
 * \return Returns a selection containing the newly created objects.
 * If the destination revision isn't transient or there is not enough
 * memory, returns \c NULL.
 *
 * \note You should free the returned selection using \ref
 * xorn_free_selection once it isn't needed any more.  */

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
	    if (i->first < *j)
		++i;
	    else if (i->first > *j)
		++j;
	    else {
		try {
			xorn_object_t ob = copy_object(dest, i->second);
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
