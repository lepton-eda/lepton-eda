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

#include "internal.h"
#include <assert.h>
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
		rev->children[NULL].push_back(ob);
	} catch (std::bad_alloc const &) {
		return NULL;
	}
	try {
		rev->parent[ob] = NULL;
	} catch (std::bad_alloc const &) {
		rev->children[NULL].pop_back();
		return NULL;
	}
	try {
		set_object_data(rev, ob, type, data);
	} catch (std::bad_alloc const &) {
		rev->parent.erase(rev->parent.find(ob));
		rev->children[NULL].pop_back();
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
 * \param type New object type (may be different from previous one,
 *             but must be \c xornsch_obtype_net or \c
 *             xornsch_obtype_component if there are objects attached
 *             to \a ob, and must be \c xornsch_obtype_text if \a ob
 *             itself is attached to an object)
 *
 * \param data Pointer to a data structure matching the object type
 *             indicated by \a type (e.g., if \a type is \c
 *             xornsch_obtype_net, a pointer to a \c xornsch_net
 *             structure).  The data structure (including referenced
 *             strings) will not be accessed after this function has
 *             returned.
 *
 * \return Returns \c 0 if the object has been changed.  Returns \c -1 if
 * - the revision isn't transient,
 * - \a type is not a valid Xorn object type,
 * - \a data is NULL,
 * - \a ob is attached to an object but \a type doesn't permit
 *   attaching the object,
 * - there are objects attached to \a ob but \a type doesn't permit
 *   attaching objects, or
 * - there is not enough memory.
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

	if (type != xornsch_obtype_net &&
	    type != xornsch_obtype_component) {
		std::map<xorn_object_t,
			 std::vector<xorn_object_t> >::const_iterator i
			= rev->children.find(ob);
		if (i != rev->children.end() && !i->second.empty())
			return -1;
	}

	std::map<xorn_object_t, xorn_object_t>::const_iterator i
		= rev->parent.find(ob);
	if (type != xornsch_obtype_text &&
	    i != rev->parent.end() && i->second != NULL)
		return -1;

	bool add = i == rev->parent.end();
	if (add) {
		try {
			rev->children[NULL].push_back(ob);
		} catch (std::bad_alloc const &) {
			return -1;
		}
		try {
			rev->parent[ob] = NULL;
		} catch (std::bad_alloc const &) {
			rev->children[NULL].pop_back();
			return -1;
		}
	}

	try {
		set_object_data(rev, ob, type, data);
	} catch (std::bad_alloc const &) {
		if (add) {
			rev->parent.erase(rev->parent.find(ob));
			rev->children[NULL].pop_back();
		}
		return -1;
	}
	return 0;
}

/** \brief Change the location of an object in the object structure of
 *         a transient revision.
 *
 * This function performs two distinct operations:
 *
 * 1. Change the order in which an object is drawn and written to
 *    files as compared to its sibling objects.
 *
 * 2. Attach a schematic text object to a schematic net or component
 *    object.  As far as this library is concerned, this will cause
 *    the text to be copied and deleted along with the net or component.
 *
 * If \a attach_to is \c NULL, the object becomes un-attached.  If \a
 * ob and \a insert_before are identical and \a ob is already
 * attached to \a attach_to, the revision is left unchanged.
 *
 * \param rev            Revision to modify (must be transient)
 * \param ob             The object which should be reordered and/or
 *                       attached (must be xornsch_text if \a
 *                       attach_to is not \c NULL)
 * \param attach_to      The object to which \a ob should be attached
 *                       (must be xornsch_net or xornsch_component, or
 *                       \c NULL)
 * \param insert_before  An object already attached to \a attach_to
 *                       before which \a ob should be inserted, or \c
 *                       NULL to append it at the end.
 *
 * \return Returns \c 0 on success.  Returns \c -1 if
 * - the revision isn't transient,
 * - \a ob or (if not \c NULL) \a attach_to or \a insert_before don't
 *   exist in the revision,
 * - \a attach_to is not \c NULL and
 *   - \a ob is not a schematic text or
 *   - \a attach_to is not a schematic net or schematic component,
 * - \a insert_before is not \c NULL and not attached to \a attach_to, or
 * - there is not enough memory.
 *
 * Example:
 * \snippet functions.c add attribute  */

int xorn_relocate_object(xorn_revision_t rev, xorn_object_t ob,
			 xorn_object_t attach_to, xorn_object_t insert_before)
{
	if (!rev->is_transient)
		return -1;

	if (attach_to != NULL) {
		if (xorn_get_object_type(rev, ob) != xornsch_obtype_text)
			return -1;
		xorn_obtype_t type = xorn_get_object_type(rev, attach_to);
		if (type != xornsch_obtype_net &&
		    type != xornsch_obtype_component)
			return -1;
	} else if (!xorn_object_exists_in_revision(rev, ob))
		return -1;

	xorn_object_t &parent = rev->parent[ob];
	std::vector<xorn_object_t> &old_children = rev->children[parent];
	std::vector<xorn_object_t> &new_children = rev->children[attach_to];

	std::vector<xorn_object_t>::iterator i = find(old_children.begin(),
						      old_children.end(), ob);
	assert(i != old_children.end());
	std::vector<xorn_object_t>::size_type pos = i - old_children.begin();

	/* Try adding the new entry first, removing the old one won't fail. */

	try {
		if (insert_before == NULL)
			new_children.push_back(ob);
		else {
			std::vector<xorn_object_t>::iterator j =
				find(new_children.begin(),
				     new_children.end(), insert_before);
			if (j == new_children.end())
				return -1;
			if (&new_children == &old_children && j <= i)
				++pos;
			new_children.insert(j, ob);
		}
	} catch (std::bad_alloc const &) {
		return -1;
	}

	old_children.erase(old_children.begin() + pos);
	parent = attach_to;
	return 0;
}

static void delete_object_but_leave_entry(
	xorn_revision_t rev, xorn_object_t ob)
{
	std::map<xorn_object_t, obstate *>::iterator i
		= rev->obstates.find(ob);
	i->second->dec_refcnt();
	rev->obstates.erase(i);

	rev->parent.erase(rev->parent.find(ob));

	std::map<xorn_object_t, std::vector<xorn_object_t> >::iterator j
		= rev->children.find(ob);

	if (j == rev->children.end())
		return;

	for (std::vector<xorn_object_t>::const_iterator k = j->second.begin();
	     k != j->second.end(); ++k)
		delete_object_but_leave_entry(rev, *k);

	rev->children.erase(j);
}

/** \brief Delete an object from a transient revision.
 *
 * Any objects attached to \a ob are deleted as well.
 *
 * The deleted object(s) stay valid and can later be re-added using
 * \ref xorn_set_object_data or its type-safe equivalents.
 *
 * If the revision isn't transient or the object doesn't exist in the
 * revision, nothing is changed.  */

void xorn_delete_object(xorn_revision_t rev, xorn_object_t ob)
{
	if (!rev->is_transient)
		return;

	std::map<xorn_object_t, xorn_object_t>::const_iterator i
		= rev->parent.find(ob);
	if (i == rev->parent.end())
		return;
	xorn_object_t parent = i->second;

	delete_object_but_leave_entry(rev, ob);

	std::vector<xorn_object_t> &children = rev->children[parent];
	children.erase(find(children.begin(), children.end(), ob));
}

/** \brief Delete some objects from a transient revision.
 *
 * Any objects attached to a deleted object are deleted as well.
 *
 * The deleted objects stay valid and can later be re-added using \ref
 * xorn_set_object_data or its type-safe equivalents.
 *
 * Objects that don't exist in the revision are ignored.  If the
 * revision isn't transient, nothing is changed.  */

void xorn_delete_selected_objects(xorn_revision_t rev, xorn_selection_t sel)
{
	if (!rev->is_transient)
		return;

	for (std::set<xorn_object_t>::const_iterator i = sel->begin();
	     i != sel->end(); ++i)
		xorn_delete_object(rev, *i);
}

static xorn_object_t copy_object(
	xorn_revision_t dest, xorn_revision_t src, xorn_object_t src_ob,
	obstate *obstate, xorn_object_t attach_to,
	std::vector<xorn_object_t> &copied)
{
	xorn_object_t dest_ob = (xorn_object_t)++next_object_id;
	dest->children[attach_to].push_back(dest_ob);
	try {
		dest->parent[dest_ob] = attach_to;
	} catch (std::bad_alloc const &) {
		dest->children[attach_to].pop_back();
		throw;
	}
	try {
		dest->obstates[dest_ob] = obstate;
		obstate->inc_refcnt();
	} catch (std::bad_alloc const &) {
		dest->parent.erase(dest->parent.find(dest_ob));
		dest->children[attach_to].pop_back();
		throw;
	}
	try {
		copied.push_back(dest_ob);
	} catch (std::bad_alloc const &) {
		xorn_delete_object(dest, dest_ob);
		throw;
	}

	std::map<xorn_object_t, std::vector<xorn_object_t> >::const_iterator i
		= src->children.find(src_ob);

	if (i != src->children.end())
		for (std::vector<xorn_object_t>::const_iterator j
			     = i->second.begin(); j != i->second.end(); ++j)
			copy_object(dest, src, *j, src->obstates[*j],
				    dest_ob, copied);

	return dest_ob;
}

/** \brief Copy an object to a transient revision.
 *
 * Any objects attached to \a ob are copied as well, their copies
 * being attached to the copy of \a ob, which is appended to the end
 * of the object list.
 *
 * \param dest Destination revision (must be transient)
 * \param src Source revision (does not need to be transient)
 * \param ob Object in the source revision which should be copied
 *
 * \return Returns the copy of \a ob.  If the destination revision
 * isn't transient, \a ob doesn't exist in the source revision, or
 * there is not enough memory, returns \c NULL.  */

xorn_object_t xorn_copy_object(xorn_revision_t dest,
			       xorn_revision_t src, xorn_object_t ob)
{
	if (!dest->is_transient)
		return NULL;

	std::map<xorn_object_t, obstate *>::const_iterator i
		= src->obstates.find(ob);

	if (i == src->obstates.end())
		return NULL;

	std::vector<xorn_object_t> copied;

	try {
		return copy_object(dest, src, ob, i->second, NULL, copied);
	} catch (std::bad_alloc const &) {
		for (std::vector<xorn_object_t>::const_iterator i
			     = copied.begin(); i != copied.end(); ++i)
			xorn_delete_object(dest, *i);
		return NULL;
	}
}

/** \brief Copy some objects to a transient revision.
 *
 * Any objects attached to the objects are copied as well and attached
 * to the corresponding new object.  The copied objects are appended
 * to the end of the object list in an unspecified order.
 *
 * \param dest Destination revision (must be transient)
 * \param src Source revision (does not need to be transient)
 * \param sel Objects in the source revision which should be copied
 *
 * \return Returns a selection containing the copied objects,
 * excluding attached objects.  If the destination revision isn't
 * transient or there is not enough memory, returns \c NULL.
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
	std::vector<xorn_object_t> copied;

	while (i != src->obstates.end() && j != sel->end())
	    if (i->first < *j)
		++i;
	    else if (i->first > *j)
		++j;
	    else {
		try {
			xorn_object_t ob = copy_object(
			    dest, src, i->first, i->second, NULL, copied);
			rsel->insert(ob);
		} catch (std::bad_alloc const &) {
			for (std::vector<xorn_object_t>::const_iterator i
				     = copied.begin(); i != copied.end(); ++i)
				xorn_delete_object(dest, *i);
			delete rsel;
			return NULL;
		}
		++i;
		++j;
	    }

	return rsel;
}
