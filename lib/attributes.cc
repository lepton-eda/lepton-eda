/* Copyright (C) 2013 Roland Lutz

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
#include <string.h>


#define BASIC_TYPE_int int
#define BASIC_TYPE_bool bool
#define BASIC_TYPE_double double
#define BASIC_TYPE_double2d struct xorn_double2d
#define BASIC_TYPE_string struct xorn_string

#define PARAM_TYPE_int int
#define PARAM_TYPE_bool bool
#define PARAM_TYPE_double double
#define PARAM_TYPE_double2d const struct xorn_double2d *
#define PARAM_TYPE_string const struct xorn_string *

#define ASSIGN_FROM_VAR_int(dst, src) (dst) = (src)
#define ASSIGN_FROM_VAR_bool(dst, src) (dst) = (src)
#define ASSIGN_FROM_VAR_double(dst, src) (dst) = (src)
#define ASSIGN_FROM_VAR_double2d(dst, src) memcpy(&(dst), &(src), sizeof (dst))
#define ASSIGN_FROM_VAR_string(dst, src) memcpy(&(dst), &(src), sizeof (dst))

#define ASSIGN_FROM_ARG_int(dst, src) (dst) = (src)
#define ASSIGN_FROM_ARG_bool(dst, src) (dst) = (src)
#define ASSIGN_FROM_ARG_double(dst, src) (dst) = (src)
#define ASSIGN_FROM_ARG_double2d(dst, src) memcpy(&(dst), (src), sizeof (dst))
#define ASSIGN_FROM_ARG_string(dst, src) memcpy(&(dst), (src), sizeof (dst))

#define EQ_VAR_int(a, b) ((a) == (b))
#define EQ_VAR_bool(a, b) ((a) == (b))
#define EQ_VAR_double(a, b) ((a) == (b))
#define EQ_VAR_double2d(a, b) ((a).x == (b).x && (a).y == (b).y)
#define EQ_VAR_string(a, b) ((a).len == (b).len && \
			     memcmp((a).s, (b).s, (a).len) == 0)

#define EQ_ARG_int(a, b) ((a) == (b))
#define EQ_ARG_bool(a, b) ((a) == (b))
#define EQ_ARG_double(a, b) ((a) == (b))
#define EQ_ARG_double2d(a, b) ((a).x == (b)->x && (a).y == (b)->y)
#define EQ_ARG_string(a, b) ((a).len == (b)->len && \
			     memcmp((a).s, (b)->s, (a).len) == 0)

#define CLEAR_int(dst) (dst) = 0
#define CLEAR_bool(dst) (dst) = false
#define CLEAR_double(dst) (dst) = 0.
#define CLEAR_double2d(dst) do { (dst).x = 0.; (dst).y = 0.; } while (0)
#define CLEAR_string(dst) do { (dst).s = NULL; (dst).len = 0; } while (0)

#define GET_yes(type, attname, atttype)					\
	case xornsch_obtype_##type:					\
		ASSIGN_FROM_VAR_##atttype(v,				\
			((struct xornsch_##type *)data)->attname);	\
		break;
#define GET_no(type, attname, atttype)

#define SET_yes(type, attname, atttype)					\
	case xornsch_obtype_##type:					\
		ASSIGN_FROM_ARG_##atttype(				\
			((struct xornsch_##type *)data)->attname,	\
			value);						\
		break;
#define SET_no(type, attname, atttype)

#define DEFINE_ATTRIBUTE_METHODS(attname, attid, atttype,		\
				 in_arc, in_box, in_circle,		\
				 in_component, in_line, in_net,		\
				 in_path, in_picture, in_text)		\
    void xornsch_get_##attname(						\
	xorn_revision_t rev, xorn_selection_t sel,			\
	xorn_attst_t *state_return, BASIC_TYPE_##atttype *value_return)	\
    {									\
	bool found = false;						\
	BASIC_TYPE_##atttype result;					\
	CLEAR_##atttype(result);					\
									\
	std::map<xorn_object_t, obstate *>::const_iterator i		\
	    = rev->obstates.begin();					\
	std::set<xorn_object_t>::const_iterator j = sel->begin();	\
									\
	while (i != rev->obstates.end() && j != sel->end())		\
	    if ((*i).first < *j)					\
		++i;							\
	    else if ((*i).first > *j)					\
		++j;							\
	    else {							\
		BASIC_TYPE_##atttype v;					\
		CLEAR_##atttype(v);					\
		xorn_obtype_t type = (*i).second->type;			\
		void *data = (*i).second->data;				\
		++i;							\
		++j;							\
									\
		switch (type) {						\
		    GET_##in_arc(arc, attid, atttype)			\
		    GET_##in_box(box, attid, atttype)			\
		    GET_##in_circle(circle, attid, atttype)		\
		    GET_##in_component(component, attid, atttype)	\
		    GET_##in_line(line, attid, atttype)			\
		    GET_##in_net(net, attid, atttype)			\
		    GET_##in_path(path, attid, atttype)			\
		    GET_##in_picture(picture, attid, atttype)		\
		    GET_##in_text(text, attid, atttype)			\
		default:						\
		    continue;						\
		}							\
									\
		if (!found) {						\
		    found = true;					\
		    ASSIGN_FROM_VAR_##atttype(result, v);		\
		} else if (!EQ_VAR_##atttype(v, result)) {		\
		    if (state_return != NULL)				\
			*state_return = xorn_attst_inconsistent;	\
		    if (value_return != NULL)				\
			CLEAR_##atttype(*value_return);			\
		    return;						\
		}							\
	    }								\
									\
	if (state_return != NULL)					\
	    *state_return = found ? xorn_attst_consistent		\
				  : xorn_attst_na;			\
	if (value_return != NULL)					\
	    ASSIGN_FROM_VAR_##atttype(*value_return, result);		\
	return;								\
    }									\
									\
    int xornsch_set_##attname(						\
	xorn_revision_t rev, xorn_selection_t sel,			\
	PARAM_TYPE_##atttype value)					\
    {									\
	if (!rev->is_transient)						\
	    return -1;							\
									\
	std::map<xorn_object_t, obstate *> new_obstates;		\
	try {								\
	    new_obstates = rev->obstates;				\
	} catch (std::bad_alloc const &) {				\
	    return -1;							\
	}								\
	for (std::map<xorn_object_t, obstate *>::const_iterator i	\
		 = new_obstates.begin(); i != new_obstates.end(); ++i)	\
	    (*i).second->inc_refcnt();					\
									\
	try {								\
	    std::map<xorn_object_t, obstate *>::const_iterator i	\
		= rev->obstates.begin();				\
	    std::set<xorn_object_t>::const_iterator j = sel->begin();	\
									\
	    while (i != rev->obstates.end() && j != sel->end())		\
		if ((*i).first < *j)					\
		    ++i;						\
		else if ((*i).first > *j)				\
		    ++j;						\
		else {							\
		    xorn_object_t ob = (*i).first;			\
		    xorn_obtype_t type = (*i).second->type;		\
		    void *data = copy_data(type, (*i).second->data);	\
		    ++i;						\
		    ++j;						\
									\
		    switch (type) {					\
			SET_##in_arc(arc, attid, atttype)		\
			SET_##in_box(box, attid, atttype)		\
			SET_##in_circle(circle, attid, atttype)		\
			SET_##in_component(component, attid, atttype)	\
			SET_##in_line(line, attid, atttype)		\
			SET_##in_net(net, attid, atttype)		\
			SET_##in_path(path, attid, atttype)		\
			SET_##in_picture(picture, attid, atttype)	\
			SET_##in_text(text, attid, atttype)		\
		    default:						\
			free(data);					\
			continue;					\
		    }							\
									\
		    try {						\
			obstate *tmp = new obstate(type, data);		\
			try {						\
			    obstate *&p = new_obstates[ob];		\
			    if (p != NULL)				\
				p->dec_refcnt();			\
			    p = tmp;					\
			} catch (std::bad_alloc const &) {		\
			    tmp->dec_refcnt();				\
			    throw;					\
			}						\
		    } catch (std::bad_alloc const &) {			\
			free(data);					\
			throw;						\
		    }							\
		    free(data);						\
		}							\
	} catch (std::bad_alloc const &) {				\
	    for (std::map<xorn_object_t, obstate *>::const_iterator	\
		     i = new_obstates.begin();				\
		 i != new_obstates.end(); ++i)				\
		(*i).second->dec_refcnt();				\
	    return -1;							\
	}								\
									\
	for (std::map<xorn_object_t, obstate *>::const_iterator		\
		 i = rev->obstates.begin();				\
	     i != rev->obstates.end(); ++i)				\
	    (*i).second->dec_refcnt();					\
									\
	rev->obstates = new_obstates;					\
	return 0;							\
    }									\
									\
    xorn_selection_t xornsch_select_by_##attname(			\
	xorn_revision_t rev, PARAM_TYPE_##atttype value)		\
    {									\
	xorn_selection_t rsel;						\
	try {								\
	    rsel = new xorn_selection();				\
	} catch (std::bad_alloc const &) {				\
	    return NULL;						\
	}								\
									\
	try {								\
	    for (std::map<xorn_object_t, obstate *>::const_iterator	\
		     i = rev->obstates.begin();				\
		 i != rev->obstates.end(); ++i) {			\
		BASIC_TYPE_##atttype v;					\
		CLEAR_##atttype(v);					\
		xorn_object_t ob = (*i).first;				\
		xorn_obtype_t type = (*i).second->type;			\
		void *data = (*i).second->data;				\
									\
		switch (type) {						\
		    GET_##in_arc(arc, attid, atttype)			\
		    GET_##in_box(box, attid, atttype)			\
		    GET_##in_circle(circle, attid, atttype)		\
		    GET_##in_component(component, attid, atttype)	\
		    GET_##in_line(line, attid, atttype)			\
		    GET_##in_net(net, attid, atttype)			\
		    GET_##in_path(path, attid, atttype)			\
		    GET_##in_picture(picture, attid, atttype)		\
		    GET_##in_text(text, attid, atttype)			\
		default:						\
		    continue;						\
		}							\
									\
		if (EQ_ARG_##atttype(v, value))				\
		    rsel->insert(ob);					\
	    }								\
	} catch (std::bad_alloc const &) {				\
	    delete rsel;						\
	    return NULL;						\
	}								\
	return rsel;							\
    }

#define DAM0(attname, atttype, h0, h1, h2, h3, h4, h5, h6, h7, h8)	\
	DEFINE_ATTRIBUTE_METHODS(attname, attname, atttype,		\
				 h0, h1, h2, h3, h4, h5, h6, h7, h8)

#define DAM1(an0, an1, atttype, h0, h1, h2, h3, h4, h5, h6, h7, h8)	\
	DEFINE_ATTRIBUTE_METHODS(an0##_##an1, an0.an1, atttype,		\
				 h0, h1, h2, h3, h4, h5, h6, h7, h8)

DAM0(alignment,		int,	  no,  no,  no,  no,  no,  no,  no,  no,  yes)
DAM0(angle,		int,	  no,  no,  no,  yes, no,  no,  no,  yes, yes)
DAM0(color,		int,	  yes, yes, yes, no,  yes, yes, yes, no,  yes)
DAM1(fill, angle0,	int,	  no,  yes, yes, no,  no,  no,  yes, no,  no)
DAM1(fill, angle1,	int,	  no,  yes, yes, no,  no,  no,  yes, no,  no)
DAM1(fill, pitch0,	double,	  no,  yes, yes, no,  no,  no,  yes, no,  no)
DAM1(fill, pitch1,	double,	  no,  yes, yes, no,  no,  no,  yes, no,  no)
DAM1(fill, type,	int,	  no,  yes, yes, no,  no,  no,  yes, no,  no)
DAM1(fill, width,	double,	  no,  yes, yes, no,  no,  no,  yes, no,  no)
DAM0(is_bus,		bool,	  no,  no,  no,  no,  no,  yes, no,  no,  no)
DAM0(is_inverted,	bool,	  no,  no,  no,  no,  no,  yes, no,  no,  no)
DAM0(is_pin,		bool,	  no,  no,  no,  no,  no,  yes, no,  no,  no)
DAM1(line, cap_style,	int,	  yes, yes, yes, no,  yes, no,  yes, no,  no)
DAM1(line, dash_length,	double,	  yes, yes, yes, no,  yes, no,  yes, no,  no)
DAM1(line, dash_space,	double,	  yes, yes, yes, no,  yes, no,  yes, no,  no)
DAM1(line, dash_style,	int,	  yes, yes, yes, no,  yes, no,  yes, no,  no)
DAM1(line, width,	double,	  yes, yes, yes, no,  yes, no,  yes, no,  no)
DAM0(mirror,		bool,	  no,  no,  no,  yes, no,  no,  no,  yes, no)
DAM0(pathdata,		string,	  no,  no,  no,  no,  no,  no,  yes, no,  no)
DAM0(pos,		double2d, yes, yes, yes, yes, yes, yes, no,  yes, yes)
DAM1(pos, x,		double,	  yes, yes, yes, yes, yes, yes, no,  yes, yes)
DAM1(pos, y,		double,	  yes, yes, yes, yes, yes, yes, no,  yes, yes)
DAM0(radius,		double,	  yes, no,  yes, no,  no,  no,  no,  no,  no)
DAM0(selectable,	bool,	  no,  no,  no,  yes, no,  no,  no,  no,  no)
DAM0(show_name_value,	int,	  no,  no,  no,  no,  no,  no,  no,  no,  yes)
DAM0(size,		double2d, no,  yes, no,  no,  yes, yes, no,  yes, no)
DAM1(size, x,		double,	  no,  yes, no,  no,  yes, yes, no,  yes, no)
DAM1(size, y,		double,	  no,  yes, no,  no,  yes, yes, no,  yes, no)
DAM0(startangle,	int,	  yes, no,  no,  no,  no,  no,  no,  no,  no)
DAM0(sweepangle,	int,	  yes, no,  no,  no,  no,  no,  no,  no,  no)
DAM0(text,		string,	  no,  no,  no,  no,  no,  no,  no,  no,  yes)
DAM0(text_size,		int,	  no,  no,  no,  no,  no,  no,  no,  no,  yes)
DAM0(visibility,	bool,	  no,  no,  no,  no,  no,  no,  no,  no,  yes)
