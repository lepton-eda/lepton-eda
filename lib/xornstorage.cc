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
#include <err.h>
#include <stdlib.h>
#include <string.h>
#include <algorithm>
#include <map>
#include <set>
#include <vector>
#include "key_iterator.h"

class obstate;

struct xorn_file {
	xorn_file();
	~xorn_file();
	std::vector<xorn_revision_t> revisions;
	std::vector<xorn_object_t> objects;
	/* empty_revision will add itself to the revision list */
	xorn_revision_t const empty_revision;
};

struct xorn_revision {
	xorn_revision(xorn_file_t file);
	xorn_revision(xorn_revision_t rev);
	~xorn_revision();
	xorn_file_t const file;
	std::map<xorn_object_t, obstate *> obstates;
};

struct xorn_object {
	xorn_object(xorn_file_t file);
};

struct xorn_selection : public std::set<xorn_object_t> {
};

struct xorn_changeset {
	xorn_changeset(xorn_revision_t rev);
	xorn_revision_t r;
};

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


xorn_file::xorn_file() : empty_revision(new xorn_revision(this))
{
}

xorn_file::~xorn_file()
{
	for (std::vector<xorn_revision_t>::const_iterator i
		     = revisions.begin(); i != revisions.end(); ++i)
		delete *i;
	for (std::vector<xorn_object_t>::const_iterator i
		     = objects.begin(); i != objects.end(); ++i)
		delete *i;
}

xorn_revision::xorn_revision(xorn_file_t file) : file(file)
{
	file->revisions.push_back(this);
}

xorn_revision::xorn_revision(xorn_revision_t rev)
	: file(rev->file), obstates(rev->obstates)
{
	file->revisions.push_back(this);

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

xorn_object::xorn_object(xorn_file_t file)
{
	file->objects.push_back(this);
}

xorn_changeset::xorn_changeset(xorn_revision_t rev) : r(new xorn_revision(rev))
{
}

static void *copy_data(xorn_obtype_t type, void const *src)
{
	size_t size;

	switch (type) {
	case xornsch_obtype_arc:       size = sizeof(xornsch_arc); break;
	case xornsch_obtype_box:       size = sizeof(xornsch_box); break;
	case xornsch_obtype_circle:    size = sizeof(xornsch_circle); break;
	case xornsch_obtype_component: size = sizeof(xornsch_component); break;
	case xornsch_obtype_line:      size = sizeof(xornsch_line); break;
	case xornsch_obtype_net:       size = sizeof(xornsch_net); break;
	case xornsch_obtype_path:      size = sizeof(xornsch_path); break;
	case xornsch_obtype_picture:   size = sizeof(xornsch_picture); break;
	case xornsch_obtype_text:      size = sizeof(xornsch_text); break;
	default:                       throw std::bad_alloc();
	}

	void *dest = malloc(size);
	if (dest == NULL)
		throw std::bad_alloc();

	memcpy(dest, src, size);
	return dest;
}

static void duplicate_string(xorn_string &str)
{
	char *buf = (char *)malloc(str.len);
	if (buf == NULL)
		throw std::bad_alloc();
	memcpy(buf, str.s, str.len);
	str.s = buf;
}

obstate::obstate(xorn_obtype_t type, void const *data)
	: refcnt(1), type(type), data(copy_data(type, data))
{
	try {
		switch(type) {
		case xornsch_obtype_path:
			duplicate_string(
				((xornsch_path *)this->data)->pathdata);
			break;
		case xornsch_obtype_text:
			duplicate_string(((xornsch_text *)this->data)->text);
			break;
		default:
			/* do nothing */;
		}
	} catch (std::bad_alloc const &) {
		free(this->data);
		throw;
	}
}

obstate::~obstate()
{
	switch(type) {
	case xornsch_obtype_path:
		free(const_cast<char *>(((xornsch_path *)data)->pathdata.s));
		break;
	case xornsch_obtype_text:
		free(const_cast<char *>(((xornsch_text *)data)->text.s));
		break;
	default:
		/* do nothing */;
	}
	free(this->data);
}

void obstate::inc_refcnt()
{
	++refcnt;
}

void obstate::dec_refcnt()
{
	if (--refcnt == 0)
		delete this;
}

/****************************************************************************/

xorn_file_t xorn_new_file()
{
	try {
		return new xorn_file();
	} catch (std::bad_alloc const &) {
		return NULL;
	}
}

void xorn_close_file(xorn_file_t file)
{
	delete file;
}

xorn_revision_t xorn_get_empty_revision(xorn_file_t file)
{
	return file->empty_revision;
}

/****************************************************************************/

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

/****************************************************************************/

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

void xorn_deselect(xorn_selection_t sel)
{
	delete sel;
}

/****************************************************************************/

xorn_changeset_t xorn_alloc_changeset(xorn_revision_t rev)
{
	try {
		return new xorn_changeset(rev);
	} catch (std::bad_alloc const &) {
		return NULL;
	}
}

xorn_revision_t xorn_apply_changeset(xorn_changeset_t chset,
				     const char *message)
{
	xorn_revision_t rev = chset->r;
	chset->r = 0;
	delete chset;
	return rev;
}

static void set_object_data(xorn_changeset_t chset, xorn_object_t ob,
			    xorn_obtype_t type, void const *data)
{
	obstate *tmp = new obstate(type, data);
	try {
		obstate *&p = chset->r->obstates[ob];
		if (p != NULL)
			p->dec_refcnt();
		p = tmp;
	} catch (std::bad_alloc const &) {
		tmp->dec_refcnt();
		throw;
	}
}

xorn_object_t xorn_add_object(xorn_changeset_t chset,
			      xorn_obtype_t type, void const *data)
{
	xorn_object_t ob;
	try {
		ob = new xorn_object(chset->r->file);
	} catch (std::bad_alloc const &) {
		return NULL;
	}
	try {
		set_object_data(chset, ob, type, data);
	} catch (std::bad_alloc const &) {
		chset->r->file->objects.pop_back();
		delete ob;
		return NULL;
	}
	return ob;
}

int xorn_set_object_data(xorn_changeset_t chset, xorn_object_t ob,
			 xorn_obtype_t type, void const *data)
{
	try {
		set_object_data(chset, ob, type, data);
	} catch (std::bad_alloc const &) {
		return -1;
	}
	return 0;
}

void xorn_delete_object(xorn_changeset_t chset, xorn_object_t ob)
{
	std::map<xorn_object_t, obstate *>::iterator i
		= chset->r->obstates.find(ob);

	if (i != chset->r->obstates.end()) {
		(*i).second->dec_refcnt();
		chset->r->obstates.erase(i);
	}
}

void xorn_delete_selected_objects(xorn_changeset_t chset, xorn_selection_t sel)
{
	for (std::set<xorn_object_t>::const_iterator i = sel->begin();
	     i != sel->end(); ++i) {
		std::map<xorn_object_t, obstate *>::iterator j
			= chset->r->obstates.find(*i);
		if (j != chset->r->obstates.end()) {
			(*j).second->dec_refcnt();
			chset->r->obstates.erase(j);
		}
	}
}

static xorn_object_t copy_object(xorn_changeset_t chset, obstate *obstate)
{
	xorn_object_t ob = new xorn_object(chset->r->file);
	try {
		chset->r->obstates[ob] = obstate;
		obstate->inc_refcnt();
	} catch (std::bad_alloc const &) {
		chset->r->file->objects.pop_back();
		delete ob;
		throw;
	}
	return ob;
}

xorn_object_t xorn_copy_object(
	xorn_changeset_t chset, xorn_revision_t rev, xorn_object_t ob)
{
	std::map<xorn_object_t, obstate *>::const_iterator i
		= rev->obstates.find(ob);

	if (i == rev->obstates.end())
		return NULL;

	try {
		return copy_object(chset, (*i).second);
	} catch (std::bad_alloc const &) {
		return NULL;
	}
}

xorn_selection_t xorn_copy_objects(
	xorn_changeset_t chset, xorn_revision_t rev, xorn_selection_t sel)
{
	xorn_selection_t rsel;
	try {
		rsel = new xorn_selection();
	} catch (std::bad_alloc const &) {
		return NULL;
	}

	std::map<xorn_object_t, obstate *>::const_iterator i
		= rev->obstates.begin();
	std::set<xorn_object_t>::const_iterator j = sel->begin();

	while (i != rev->obstates.end() && j != sel->end())
	    if ((*i).first < *j)
		++i;
	    else if ((*i).first > *j)
		++j;
	    else {
		try {
			xorn_object_t ob = copy_object(chset, (*i).second);
			try {
				rsel->insert(ob);
			} catch (std::bad_alloc const &) {
				xorn_delete_object(chset, ob);
				chset->r->file->objects.pop_back();
				delete ob;
				throw;
			}
		} catch (std::bad_alloc const &) {
			for (xorn_selection::const_iterator i = rsel->begin();
			     i != rsel->end(); i++) {
				xorn_delete_object(chset, *i);
				delete *i;
			}
			chset->r->file->objects.erase(
				chset->r->file->objects.end() - rsel->size(),
				chset->r->file->objects.end());
			delete rsel;
			return NULL;
		}
		++i;
		++j;
	    }

	return rsel;
}
