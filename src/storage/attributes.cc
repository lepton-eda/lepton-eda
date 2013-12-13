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


/* overloaded helper methods */

static void assign(int &dst, int src) {
    dst = src;
}
static void assign(bool &dst, bool src) {
    dst = src;
}
static void assign(double &dst, double src) {
    dst = src;
}
static void assign(struct xorn_double2d &dst,
		   struct xorn_double2d const &src) {
    memcpy(&dst, &src, sizeof dst);
}
static void assign(struct xorn_string &dst,
		   struct xorn_string const &src) {
    memcpy(&dst, &src, sizeof dst);
}
static void assign(struct xornsch_line_attr &dst,
		   struct xornsch_line_attr const &src) {
    memcpy(&dst, &src, sizeof dst);
}
static void assign(struct xornsch_fill_attr &dst,
		   struct xornsch_fill_attr const &src) {
    memcpy(&dst, &src, sizeof dst);
}

static bool equals(int a, int b) {
    return a == b;
}
static bool equals(bool a, bool b) {
    return a == b;
}
static bool equals(double a, double b) {
    return a == b;
}
static bool equals(const struct xorn_double2d &a,
		   const struct xorn_double2d &b) {
    return a.x == b.x && a.y == b.y;
}
static bool equals(const struct xorn_string &a,
		   const struct xorn_string &b) {
    return a.len == b.len && memcmp(a.s, b.s, a.len) == 0;
}
static bool equals(const struct xornsch_line_attr &a,
		   const struct xornsch_line_attr &b) {
    return memcmp(&a, &b, sizeof a) == 0;
}
static bool equals(const struct xornsch_fill_attr &a,
		   const struct xornsch_fill_attr &b) {
    return memcmp(&a, &b, sizeof a) == 0;
}

static void clear(int &dst) {
    dst = 0;
}
static void clear(bool &dst) {
    dst = false;
}
static void clear(double &dst) {
    dst = 0.;
}
static void clear(struct xorn_double2d &dst) {
    dst.x = 0.;
    dst.y = 0.;
}
static void clear(struct xorn_string &dst) {
    dst.s = NULL;
    dst.len = 0;
}
static void clear(struct xornsch_line_attr &dst) {
    memset(&dst, 0, sizeof dst);
}
static void clear(struct xornsch_fill_attr &dst) {
    memset(&dst, 0, sizeof dst);
}

/****************************************************************************/

#define DEFAULTS							\
    static inline bool get(basic_type &, void const *) {		\
	return false;							\
    }									\
    static inline bool set(basic_type const &, void *) {		\
	return false;							\
    }

#define HAVE_IN(type, field)						\
    static inline bool get(basic_type &v,				\
			   struct xornsch_##type const *data) {		\
	assign(v, data->field);						\
	return true;							\
    }									\
    static inline bool set(basic_type const &v,				\
			   struct xornsch_##type *data) {		\
	assign(data->field, v);						\
	return true;							\
    }

class Attr_alignment {
public:
    typedef int basic_type;
    DEFAULTS
    HAVE_IN(text, alignment)
};

class Attr_angle {
public:
    typedef int basic_type;
    DEFAULTS
    HAVE_IN(component, angle)
    HAVE_IN(picture, angle)
    HAVE_IN(text, angle)
};

class Attr_color {
public:
    typedef int basic_type;
    DEFAULTS
    HAVE_IN(arc, color)
    HAVE_IN(box, color)
    HAVE_IN(circle, color)
    HAVE_IN(line, color)
    HAVE_IN(net, color)
    HAVE_IN(path, color)
    HAVE_IN(text, color)
};

class Attr_fill {
public:
    typedef struct xornsch_fill_attr basic_type;
    DEFAULTS
    HAVE_IN(box, fill)
    HAVE_IN(circle, fill)
    HAVE_IN(path, fill)
};

class Attr_fill_angle0 {
public:
    typedef int basic_type;
    DEFAULTS
    HAVE_IN(box, fill.angle0)
    HAVE_IN(circle, fill.angle0)
    HAVE_IN(path, fill.angle0)
};

class Attr_fill_angle1 {
public:
    typedef int basic_type;
    DEFAULTS
    HAVE_IN(box, fill.angle1)
    HAVE_IN(circle, fill.angle1)
    HAVE_IN(path, fill.angle1)
};

class Attr_fill_pitch0 {
public:
    typedef double basic_type;
    DEFAULTS
    HAVE_IN(box, fill.pitch0)
    HAVE_IN(circle, fill.pitch0)
    HAVE_IN(path, fill.pitch0)
};

class Attr_fill_pitch1 {
public:
    typedef double basic_type;
    DEFAULTS
    HAVE_IN(box, fill.pitch1)
    HAVE_IN(circle, fill.pitch1)
    HAVE_IN(path, fill.pitch1)
};

class Attr_fill_type {
public:
    typedef int basic_type;
    DEFAULTS
    HAVE_IN(box, fill.type)
    HAVE_IN(circle, fill.type)
    HAVE_IN(path, fill.type)
};

class Attr_fill_width {
public:
    typedef double basic_type;
    DEFAULTS
    HAVE_IN(box, fill.width)
    HAVE_IN(circle, fill.width)
    HAVE_IN(path, fill.width)
};

class Attr_is_bus {
public:
    typedef bool basic_type;
    DEFAULTS
    HAVE_IN(net, is_bus)
};

class Attr_is_inverted {
public:
    typedef bool basic_type;
    DEFAULTS
    HAVE_IN(net, is_inverted)
};

class Attr_is_pin {
public:
    typedef bool basic_type;
    DEFAULTS
    HAVE_IN(net, is_pin)
};

class Attr_line {
public:
    typedef struct xornsch_line_attr basic_type;
    DEFAULTS
    HAVE_IN(arc, line)
    HAVE_IN(box, line)
    HAVE_IN(circle, line)
    HAVE_IN(line, line)
    HAVE_IN(path, line)
};

class Attr_line_cap_style {
public:
    typedef int basic_type;
    DEFAULTS
    HAVE_IN(arc, line.cap_style)
    HAVE_IN(box, line.cap_style)
    HAVE_IN(circle, line.cap_style)
    HAVE_IN(line, line.cap_style)
    HAVE_IN(path, line.cap_style)
};

class Attr_line_dash_length {
public:
    typedef double basic_type;
    DEFAULTS
    HAVE_IN(arc, line.dash_length)
    HAVE_IN(box, line.dash_length)
    HAVE_IN(circle, line.dash_length)
    HAVE_IN(line, line.dash_length)
    HAVE_IN(path, line.dash_length)
};

class Attr_line_dash_space {
public:
    typedef double basic_type;
    DEFAULTS
    HAVE_IN(arc, line.dash_space)
    HAVE_IN(box, line.dash_space)
    HAVE_IN(circle, line.dash_space)
    HAVE_IN(line, line.dash_space)
    HAVE_IN(path, line.dash_space)
};

class Attr_line_dash_style {
public:
    typedef int basic_type;
    DEFAULTS
    HAVE_IN(arc, line.dash_style)
    HAVE_IN(box, line.dash_style)
    HAVE_IN(circle, line.dash_style)
    HAVE_IN(line, line.dash_style)
    HAVE_IN(path, line.dash_style)
};

class Attr_line_width {
public:
    typedef double basic_type;
    DEFAULTS
    HAVE_IN(arc, line.width)
    HAVE_IN(box, line.width)
    HAVE_IN(circle, line.width)
    HAVE_IN(line, line.width)
    HAVE_IN(path, line.width)
};

class Attr_mirror {
public:
    typedef bool basic_type;
    DEFAULTS
    HAVE_IN(component, mirror)
    HAVE_IN(picture, mirror)
};

class Attr_pathdata {
public:
    typedef struct xorn_string basic_type;
    DEFAULTS
    HAVE_IN(path, pathdata)
};

class Attr_pos {
public:
    typedef struct xorn_double2d basic_type;
    DEFAULTS
    HAVE_IN(arc, pos)
    HAVE_IN(box, pos)
    HAVE_IN(circle, pos)
    HAVE_IN(component, pos)
    HAVE_IN(line, pos)
    HAVE_IN(net, pos)
    HAVE_IN(picture, pos)
    HAVE_IN(text, pos)
};

class Attr_pos_x {
public:
    typedef double basic_type;
    DEFAULTS
    HAVE_IN(arc, pos.x)
    HAVE_IN(box, pos.x)
    HAVE_IN(circle, pos.x)
    HAVE_IN(component, pos.x)
    HAVE_IN(line, pos.x)
    HAVE_IN(net, pos.x)
    HAVE_IN(picture, pos.x)
    HAVE_IN(text, pos.x)
};

class Attr_pos_y {
public:
    typedef double basic_type;
    DEFAULTS
    HAVE_IN(arc, pos.y)
    HAVE_IN(box, pos.y)
    HAVE_IN(circle, pos.y)
    HAVE_IN(component, pos.y)
    HAVE_IN(line, pos.y)
    HAVE_IN(net, pos.y)
    HAVE_IN(picture, pos.y)
    HAVE_IN(text, pos.y)
};

class Attr_radius {
public:
    typedef double basic_type;
    DEFAULTS
    HAVE_IN(arc, radius)
    HAVE_IN(circle, radius)
};

class Attr_selectable {
public:
    typedef bool basic_type;
    DEFAULTS
    HAVE_IN(component, selectable)
};

class Attr_show_name_value {
public:
    typedef int basic_type;
    DEFAULTS
    HAVE_IN(text, show_name_value)
};

class Attr_size {
public:
    typedef struct xorn_double2d basic_type;
    DEFAULTS
    HAVE_IN(box, size)
    HAVE_IN(line, size)
    HAVE_IN(net, size)
    HAVE_IN(picture, size)
};

class Attr_size_x {
public:
    typedef double basic_type;
    DEFAULTS
    HAVE_IN(box, size.x)
    HAVE_IN(line, size.x)
    HAVE_IN(net, size.x)
    HAVE_IN(picture, size.x)
};

class Attr_size_y {
public:
    typedef double basic_type;
    DEFAULTS
    HAVE_IN(box, size.y)
    HAVE_IN(line, size.y)
    HAVE_IN(net, size.y)
    HAVE_IN(picture, size.y)
};

class Attr_startangle {
public:
    typedef int basic_type;
    DEFAULTS
    HAVE_IN(arc, startangle)
};

class Attr_sweepangle {
public:
    typedef int basic_type;
    DEFAULTS
    HAVE_IN(arc, sweepangle)
};

class Attr_text {
public:
    typedef struct xorn_string basic_type;
    DEFAULTS
    HAVE_IN(text, text)
};

class Attr_text_size {
public:
    typedef int basic_type;
    DEFAULTS
    HAVE_IN(text, text_size)
};

class Attr_visibility {
public:
    typedef bool basic_type;
    DEFAULTS
    HAVE_IN(text, visibility)
};

/****************************************************************************/

template<typename Attr> static void get_attr(
    xorn_revision_t rev, xorn_selection_t sel,
    xorn_attst_t *state_return, typename Attr::basic_type *value_return)
{
    bool found = false;
    typename Attr::basic_type result;
    clear(result);

    std::map<xorn_object_t, obstate *>::const_iterator i
	= rev->obstates.begin();
    std::set<xorn_object_t>::const_iterator j = sel->begin();

    while (i != rev->obstates.end() && j != sel->end())
	if ((*i).first < *j)
	    ++i;
	else if ((*i).first > *j)
	    ++j;
	else {
	    typename Attr::basic_type v;
	    clear(v);
	    xorn_obtype_t type = (*i).second->type;
	    void *data = (*i).second->data;
	    ++i;
	    ++j;

	    if ((type != xornsch_obtype_arc ||
		     !Attr::get(v, (struct xornsch_arc *)data)) &&
		(type != xornsch_obtype_box ||
		     !Attr::get(v, (struct xornsch_box *)data)) &&
		(type != xornsch_obtype_circle ||
		     !Attr::get(v, (struct xornsch_circle *)data)) &&
		(type != xornsch_obtype_component ||
		     !Attr::get(v, (struct xornsch_component *)data)) &&
		(type != xornsch_obtype_line ||
		     !Attr::get(v, (struct xornsch_line *)data)) &&
		(type != xornsch_obtype_net ||
		     !Attr::get(v, (struct xornsch_net *)data)) &&
		(type != xornsch_obtype_path ||
		     !Attr::get(v, (struct xornsch_path *)data)) &&
		(type != xornsch_obtype_picture ||
		     !Attr::get(v, (struct xornsch_picture *)data)) &&
		(type != xornsch_obtype_text ||
		     !Attr::get(v, (struct xornsch_text *)data)))
		continue;

	    if (!found) {
		found = true;
		assign(result, v);
	    } else if (!equals(v, result)) {
		if (state_return != NULL)
		    *state_return = xorn_attst_inconsistent;
		if (value_return != NULL)
		    clear(*value_return);
		return;
	    }
	}

    if (state_return != NULL)
	*state_return = found ? xorn_attst_consistent
			      : xorn_attst_na;
    if (value_return != NULL)
	assign(*value_return, result);
    return;
}

template<typename Attr> static int set_attr(
    xorn_revision_t rev, xorn_selection_t sel,
    typename Attr::basic_type const &value)
{
    if (!rev->is_transient)
	return -1;

    std::map<xorn_object_t, obstate *> new_obstates;
    try {
	new_obstates = rev->obstates;
    } catch (std::bad_alloc const &) {
	return -1;
    }
    for (std::map<xorn_object_t, obstate *>::const_iterator i
	     = new_obstates.begin(); i != new_obstates.end(); ++i)
	(*i).second->inc_refcnt();

    try {
	std::map<xorn_object_t, obstate *>::const_iterator i
	    = rev->obstates.begin();
	std::set<xorn_object_t>::const_iterator j = sel->begin();

	while (i != rev->obstates.end() && j != sel->end())
	    if ((*i).first < *j)
		++i;
	    else if ((*i).first > *j)
		++j;
	    else {
		xorn_object_t ob = (*i).first;
		xorn_obtype_t type = (*i).second->type;
		void *data = copy_data(type, (*i).second->data);
		++i;
		++j;

		if ((type != xornsch_obtype_arc ||
			 !Attr::set(value, (struct xornsch_arc *)data)) &&
		    (type != xornsch_obtype_box ||
			 !Attr::set(value, (struct xornsch_box *)data)) &&
		    (type != xornsch_obtype_circle ||
			 !Attr::set(value, (struct xornsch_circle *)data)) &&
		    (type != xornsch_obtype_component ||
			 !Attr::set(value, (struct xornsch_component *)data)) &&
		    (type != xornsch_obtype_line ||
			 !Attr::set(value, (struct xornsch_line *)data)) &&
		    (type != xornsch_obtype_net ||
			 !Attr::set(value, (struct xornsch_net *)data)) &&
		    (type != xornsch_obtype_path ||
			 !Attr::set(value, (struct xornsch_path *)data)) &&
		    (type != xornsch_obtype_picture ||
			 !Attr::set(value, (struct xornsch_picture *)data)) &&
		    (type != xornsch_obtype_text ||
			 !Attr::set(value, (struct xornsch_text *)data))) {
		    free(data);
		    continue;
		}

		try {
		    obstate *tmp = new obstate(type, data);
		    try {
			obstate *&p = new_obstates[ob];
			if (p != NULL)
			    p->dec_refcnt();
			p = tmp;
		    } catch (std::bad_alloc const &) {
			tmp->dec_refcnt();
			throw;
		    }
		} catch (std::bad_alloc const &) {
		    free(data);
		    throw;
		}
		free(data);
	    }
    } catch (std::bad_alloc const &) {
	for (std::map<xorn_object_t, obstate *>::const_iterator
		 i = new_obstates.begin();
	     i != new_obstates.end(); ++i)
	    (*i).second->dec_refcnt();
	return -1;
    }

    for (std::map<xorn_object_t, obstate *>::const_iterator
	     i = rev->obstates.begin();
	 i != rev->obstates.end(); ++i)
	(*i).second->dec_refcnt();

    rev->obstates = new_obstates;
    return 0;
}

template<typename Attr> static xorn_selection_t select_by_attr(
    xorn_revision_t rev, typename Attr::basic_type const &value)
{
    xorn_selection_t rsel;
    try {
	rsel = new xorn_selection();
    } catch (std::bad_alloc const &) {
	return NULL;
    }

    try {
	for (std::map<xorn_object_t, obstate *>::const_iterator
		 i = rev->obstates.begin();
	     i != rev->obstates.end(); ++i) {
	    typename Attr::basic_type v;
	    clear(v);
	    xorn_object_t ob = (*i).first;
	    xorn_obtype_t type = (*i).second->type;
	    void *data = (*i).second->data;

	    if ((type != xornsch_obtype_arc ||
		     !Attr::get(v, (struct xornsch_arc *)data)) &&
		(type != xornsch_obtype_box ||
		     !Attr::get(v, (struct xornsch_box *)data)) &&
		(type != xornsch_obtype_circle ||
		     !Attr::get(v, (struct xornsch_circle *)data)) &&
		(type != xornsch_obtype_component ||
		     !Attr::get(v, (struct xornsch_component *)data)) &&
		(type != xornsch_obtype_line ||
		     !Attr::get(v, (struct xornsch_line *)data)) &&
		(type != xornsch_obtype_net ||
		     !Attr::get(v, (struct xornsch_net *)data)) &&
		(type != xornsch_obtype_path ||
		     !Attr::get(v, (struct xornsch_path *)data)) &&
		(type != xornsch_obtype_picture ||
		     !Attr::get(v, (struct xornsch_picture *)data)) &&
		(type != xornsch_obtype_text ||
		     !Attr::get(v, (struct xornsch_text *)data)))
		continue;

	    if (equals(v, value))
		rsel->insert(ob);
	}
    } catch (std::bad_alloc const &) {
	delete rsel;
	return NULL;
    }
    return rsel;
}

#define DEFINE_ATTRIBUTE_FUNCTIONS(ns, name, intype, outtype, ast)	\
    void ns##_get_##name(						\
	xorn_revision_t rev, xorn_selection_t sel,			\
	xorn_attst_t *state_return, outtype value_return)		\
    {									\
	get_attr<Attr_##name>(rev, sel, state_return, value_return);	\
    }									\
    int ns##_set_##name(						\
	xorn_revision_t rev, xorn_selection_t sel, intype value)	\
    {									\
	return set_attr<Attr_##name>(rev, sel, ast value);		\
    }									\
    xorn_selection_t ns##_select_by_##name(				\
	xorn_revision_t rev, intype value)				\
    {									\
	return select_by_attr<Attr_##name>(rev, ast value);		\
    }

#define INT_ATTRIBUTE(ns, name) \
    DEFINE_ATTRIBUTE_FUNCTIONS(ns, name, int, int *, )
#define BOOL_ATTRIBUTE(ns, name) \
    DEFINE_ATTRIBUTE_FUNCTIONS(ns, name, bool, bool *, )
#define DOUBLE_ATTRIBUTE(ns, name) \
    DEFINE_ATTRIBUTE_FUNCTIONS(ns, name, double, double *, )
#define DOUBLE2D_ATTRIBUTE(ns, name) \
    DEFINE_ATTRIBUTE_FUNCTIONS(ns, name, const struct xorn_double2d *,	\
					       struct xorn_double2d *, *)
#define STRING_ATTRIBUTE(ns, name) \
    DEFINE_ATTRIBUTE_FUNCTIONS(ns, name, const struct xorn_string *,	\
					       struct xorn_string *, *)
#define LINE_ATTRIBUTE(ns, name) \
    DEFINE_ATTRIBUTE_FUNCTIONS(ns, name, const struct xornsch_line_attr *, \
					       struct xornsch_line_attr *, *)
#define FILL_ATTRIBUTE(ns, name) \
    DEFINE_ATTRIBUTE_FUNCTIONS(ns, name, const struct xornsch_fill_attr *, \
					       struct xornsch_fill_attr *, *)

     INT_ATTRIBUTE(xornsch, alignment)
     INT_ATTRIBUTE(xornsch, angle)
     INT_ATTRIBUTE(xornsch, color)
    FILL_ATTRIBUTE(xornsch, fill)
     INT_ATTRIBUTE(xornsch, fill_angle0)
     INT_ATTRIBUTE(xornsch, fill_angle1)
  DOUBLE_ATTRIBUTE(xornsch, fill_pitch0)
  DOUBLE_ATTRIBUTE(xornsch, fill_pitch1)
     INT_ATTRIBUTE(xornsch, fill_type)
  DOUBLE_ATTRIBUTE(xornsch, fill_width)
    BOOL_ATTRIBUTE(xornsch, is_bus)
    BOOL_ATTRIBUTE(xornsch, is_inverted)
    BOOL_ATTRIBUTE(xornsch, is_pin)
    LINE_ATTRIBUTE(xornsch, line)
     INT_ATTRIBUTE(xornsch, line_cap_style)
  DOUBLE_ATTRIBUTE(xornsch, line_dash_length)
  DOUBLE_ATTRIBUTE(xornsch, line_dash_space)
     INT_ATTRIBUTE(xornsch, line_dash_style)
  DOUBLE_ATTRIBUTE(xornsch, line_width)
    BOOL_ATTRIBUTE(xornsch, mirror)
  STRING_ATTRIBUTE(xornsch, pathdata)
DOUBLE2D_ATTRIBUTE(xornsch, pos)
  DOUBLE_ATTRIBUTE(xornsch, pos_x)
  DOUBLE_ATTRIBUTE(xornsch, pos_y)
  DOUBLE_ATTRIBUTE(xornsch, radius)
    BOOL_ATTRIBUTE(xornsch, selectable)
     INT_ATTRIBUTE(xornsch, show_name_value)
DOUBLE2D_ATTRIBUTE(xornsch, size)
  DOUBLE_ATTRIBUTE(xornsch, size_x)
  DOUBLE_ATTRIBUTE(xornsch, size_y)
     INT_ATTRIBUTE(xornsch, startangle)
     INT_ATTRIBUTE(xornsch, sweepangle)
  STRING_ATTRIBUTE(xornsch, text)
     INT_ATTRIBUTE(xornsch, text_size)
    BOOL_ATTRIBUTE(xornsch, visibility)
