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
#include <stdlib.h>
#include <string.h>
#include <algorithm>
#include <map>
#include <vector>
#include "key_iterator.h"

class obstate;

struct xorn_file {
	xorn_file();
	~xorn_file();
	xorn_revision_t const empty_revision;
	std::vector<xorn_revision_t> revisions;
	std::vector<xorn_object_t> objects;
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
	xorn_obtype_t type;
	void *data;
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

static size_t sizeof_obtype(xorn_obtype_t type)
{
	return 0;
}

obstate::obstate(xorn_obtype_t type, void const *data)
	: refcnt(1), type(type), data(malloc(sizeof_obtype(type)))
{
	if (this->data == NULL)
		throw std::bad_alloc();
	memcpy(this->data, data, sizeof_obtype(type));
}

obstate::~obstate()
{
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

void set_object_data(xorn_changeset_t chset, xorn_object_t ob,
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
