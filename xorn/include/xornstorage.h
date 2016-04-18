/* Copyright (C) 2013-2016 Roland Lutz

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

#ifndef XORN_STORAGE_H
#define XORN_STORAGE_H

#include <stddef.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef enum {
	xorn_obtype_none,	/* object does not exist */
	xornsch_obtype_arc,
	xornsch_obtype_box,
	xornsch_obtype_circle,
	xornsch_obtype_component,
	xornsch_obtype_line,
	xornsch_obtype_net,
	xornsch_obtype_path,
	xornsch_obtype_picture,
	xornsch_obtype_text,
} xorn_obtype_t;

typedef enum {
	xorn_attst_na,
	/* None of the selected objects has this attribute.
	   Don't show this attribute in the property editor. */
	xorn_attst_consistent,
	/* All selected objects have the same value of this attribute.
	   Show this value for this attribute in the property editor. */
	xorn_attst_inconsistent,
	/* There are different values of this attribute between the
	   selected objects.  Show this attribute in the property
	   editor, but don't show a value. */
} xorn_attst_t;

/* opaque types */
typedef struct xorn_revision *xorn_revision_t;
typedef struct xorn_object *xorn_object_t;
typedef struct xorn_selection *xorn_selection_t;

/* revision functions */

xorn_revision_t xorn_new_revision(xorn_revision_t rev);
bool xorn_revision_is_transient(xorn_revision_t rev);
void xorn_finalize_revision(xorn_revision_t rev);
void xorn_free_revision(xorn_revision_t rev);

/* object functions */

bool xorn_object_exists_in_revision(
	xorn_revision_t rev, xorn_object_t ob);
xorn_obtype_t xorn_get_object_type(
	xorn_revision_t rev, xorn_object_t ob);
const void *xorn_get_object_data(
	xorn_revision_t rev, xorn_object_t ob, xorn_obtype_t type);
int xorn_get_object_location(
	xorn_revision_t rev, xorn_object_t ob,
	xorn_object_t *attached_to_return,
	unsigned int *position_return);

int xorn_get_objects(
	xorn_revision_t rev,
	xorn_object_t **objects_return, size_t *count_return);
int xorn_get_objects_attached_to(
	xorn_revision_t rev, xorn_object_t ob,
	xorn_object_t **objects_return, size_t *count_return);
int xorn_get_selected_objects(
	xorn_revision_t rev, xorn_selection_t sel,
	xorn_object_t **objects_return, size_t *count_return);
int xorn_get_added_objects(
	xorn_revision_t from_rev, xorn_revision_t to_rev,
	xorn_object_t **objects_return, size_t *count_return);
int xorn_get_removed_objects(
	xorn_revision_t from_rev, xorn_revision_t to_rev,
	xorn_object_t **objects_return, size_t *count_return);
int xorn_get_modified_objects(
	xorn_revision_t from_rev, xorn_revision_t to_rev,
	xorn_object_t **objects_return, size_t *count_return);

/* selection functions */

xorn_selection_t xorn_select_none();
xorn_selection_t xorn_select_object(
	xorn_object_t ob);
xorn_selection_t xorn_select_attached_to(
	xorn_revision_t rev, xorn_object_t ob);
xorn_selection_t xorn_select_all(
	xorn_revision_t rev);
xorn_selection_t xorn_select_all_except(
	xorn_revision_t rev, xorn_selection_t sel);
xorn_selection_t xorn_select_including(
	xorn_selection_t sel, xorn_object_t ob);
xorn_selection_t xorn_select_excluding(
	xorn_selection_t sel, xorn_object_t ob);
xorn_selection_t xorn_select_union(
	xorn_selection_t sel0, xorn_selection_t sel1);
xorn_selection_t xorn_select_intersection(
	xorn_selection_t sel0, xorn_selection_t sel1);
xorn_selection_t xorn_select_difference(
	xorn_selection_t sel0, xorn_selection_t sel1);

bool xorn_selection_is_empty(
	xorn_revision_t rev, xorn_selection_t sel);
bool xorn_object_is_selected(
	xorn_revision_t rev, xorn_selection_t sel, xorn_object_t ob);
void xorn_free_selection(
	xorn_selection_t sel);

/* manipulation functions */

xorn_object_t xorn_add_object(xorn_revision_t rev,
			      xorn_obtype_t type, const void *data);
int xorn_set_object_data(xorn_revision_t rev, xorn_object_t ob,
			 xorn_obtype_t type, const void *data);
int xorn_relocate_object(xorn_revision_t rev, xorn_object_t ob,
			 xorn_object_t attach_to, xorn_object_t insert_before);
void xorn_delete_object(xorn_revision_t rev, xorn_object_t ob);
void xorn_delete_selected_objects(xorn_revision_t rev,
				  xorn_selection_t sel);

xorn_object_t xorn_copy_object(xorn_revision_t dest,
			       xorn_revision_t src, xorn_object_t ob);
xorn_selection_t xorn_copy_objects(xorn_revision_t dest,
				   xorn_revision_t src, xorn_selection_t sel);

/* object data definition */

struct xorn_string {
	const char *s;
	size_t len;
};

struct xorn_double2d {
	double x, y;
};

struct xorn_pointer {
	void *ptr;
	void (*incref)(void *ptr);
	void (*decref)(void *ptr);
};

struct xornsch_line_attr {
	double width;
	int cap_style;
	int dash_style;
	double dash_length;
	double dash_space;
};

struct xornsch_fill_attr {
	int type;
	double width;
	int angle0;
	double pitch0;
	int angle1;
	double pitch1;
};

struct xornsch_arc {
	struct xorn_double2d pos;
	double radius;
	int startangle;
	int sweepangle;
	int color;
	struct xornsch_line_attr line;
};

struct xornsch_box {
	struct xorn_double2d pos;
	struct xorn_double2d size;
	int color;
	struct xornsch_line_attr line;
	struct xornsch_fill_attr fill;
};

struct xornsch_circle {
	struct xorn_double2d pos;
	double radius;
	int color;
	struct xornsch_line_attr line;
	struct xornsch_fill_attr fill;
};

struct xornsch_component {
	struct xorn_double2d pos;
	bool selectable;
	int angle;
	bool mirror;
	struct xorn_pointer symbol;
};

struct xornsch_line {
	struct xorn_double2d pos;
	struct xorn_double2d size;
	int color;
	struct xornsch_line_attr line;
};

struct xornsch_net {
	struct xorn_double2d pos;
	struct xorn_double2d size;
	int color;
	bool is_bus;
	bool is_pin;
	bool is_inverted;
};

struct xornsch_path {
	struct xorn_string pathdata;
	int color;
	struct xornsch_line_attr line;
	struct xornsch_fill_attr fill;
};

struct xornsch_picture {
	struct xorn_double2d pos;
	struct xorn_double2d size;
	int angle;
	bool mirror;
	struct xorn_pointer pixmap;
};

struct xornsch_text {
	struct xorn_double2d pos;
	int color;
	int text_size;
	bool visibility;
	int show_name_value;
	int angle;
	int alignment;
	struct xorn_string text;
};

/* object type-specific functions */

#define DECLARE_OBJECT_FUNCTIONS(type) \
	const struct xornsch_##type *xornsch_get_##type##_data( \
		xorn_revision_t rev, xorn_object_t ob); \
	xorn_object_t xornsch_add_##type(xorn_revision_t rev, \
					 const struct xornsch_##type *data); \
	int xornsch_set_##type##_data(xorn_revision_t rev, xorn_object_t ob, \
				      const struct xornsch_##type *data);

DECLARE_OBJECT_FUNCTIONS(arc)
DECLARE_OBJECT_FUNCTIONS(box)
DECLARE_OBJECT_FUNCTIONS(circle)
DECLARE_OBJECT_FUNCTIONS(component)
DECLARE_OBJECT_FUNCTIONS(line)
DECLARE_OBJECT_FUNCTIONS(net)
DECLARE_OBJECT_FUNCTIONS(path)
DECLARE_OBJECT_FUNCTIONS(picture)
DECLARE_OBJECT_FUNCTIONS(text)

#undef DECLARE_OBJECT_FUNCTIONS

/* attribute functions */

#define DECLARE_ATTRIBUTE_FUNCTIONS(ns, name, intype, outtype)		\
	void ns##_get_##name(						\
		xorn_revision_t rev, xorn_selection_t sel,		\
		xorn_attst_t *state_return, outtype value_return);	\
	int ns##_set_##name(						\
		xorn_revision_t rev, xorn_selection_t sel,		\
		intype value);						\
	xorn_selection_t ns##_select_by_##name(				\
		xorn_revision_t rev, intype value);

#define INT_ATTRIBUTE(ns, name) \
	DECLARE_ATTRIBUTE_FUNCTIONS(ns, name, int, int *)
#define BOOL_ATTRIBUTE(ns, name) \
	DECLARE_ATTRIBUTE_FUNCTIONS(ns, name, bool, bool *)
#define DOUBLE_ATTRIBUTE(ns, name) \
	DECLARE_ATTRIBUTE_FUNCTIONS(ns, name, double, double *)
#define DOUBLE2D_ATTRIBUTE(ns, name) \
	DECLARE_ATTRIBUTE_FUNCTIONS(ns, name, const struct xorn_double2d *, \
						    struct xorn_double2d *)
#define STRING_ATTRIBUTE(ns, name) \
	DECLARE_ATTRIBUTE_FUNCTIONS(ns, name, const struct xorn_string *, \
						    struct xorn_string *)
#define LINE_ATTRIBUTE(ns, name) \
	DECLARE_ATTRIBUTE_FUNCTIONS(ns, name, const struct xornsch_line_attr *,\
						    struct xornsch_line_attr *)
#define FILL_ATTRIBUTE(ns, name) \
	DECLARE_ATTRIBUTE_FUNCTIONS(ns, name, const struct xornsch_fill_attr *,\
						    struct xornsch_fill_attr *)

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

#undef INT_ATTRIBUTE
#undef BOOL_ATTRIBUTE
#undef DOUBLE_ATTRIBUTE
#undef DOUBLE2D_ATTRIBUTE
#undef STRING_ATTRIBUTE
#undef LINE_ATTRIBUTE
#undef FILL_ATTRIBUTE

#undef DECLARE_ATTRIBUTE_FUNCTIONS

#ifdef __cplusplus
}
#endif

#endif
