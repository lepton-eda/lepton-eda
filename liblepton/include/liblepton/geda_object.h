/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */
/*! \file geda_object.h
 */

struct st_object
{
  int type;				/* Basic information */
  int sid;
  char *name;

  PAGE *page; /* Parent page */

  GedaBounds bounds;
  TOPLEVEL *w_bounds_valid_for;

  COMPLEX *complex;
  GedaLine *line;
  GedaCircle *circle;
  GedaArc *arc;
  BOX *box;
  TEXT *text;
  PICTURE *picture;
  PATH *path;

  GList *conn_list;			/* List of connections */
  /* to and from this object */

  /* every graphical primitive have more or less the same options. */
  /* depending on its nature a primitive is concerned with one or more */
  /* of these fields. If not, value must be ignored. */
  OBJECT_END line_end;
  OBJECT_TYPE line_type;
  int line_width;
  int line_space;
  int line_length;

  OBJECT_FILLING fill_type;
  int fill_width;
  int fill_angle1, fill_pitch1;
  int fill_angle2, fill_pitch2;

  gboolean complex_embedded;                    /* is embedded component? */
  gchar *complex_basename;              /* Component Library Symbol name */
  OBJECT *parent;                       /* Parent object pointer */

  int color; 				/* Which color */
  int dont_redraw;			/* Flag to skip redrawing */
  int selectable;			/* object selectable flag */
  int selected;				/* object selected flag */

  /* controls which direction bus rippers go */
  /* it is either 0 for un-inited, */
  /* 1 for right, -1 for left (horizontal bus) */
  /* 1 for up, -1 for down (vertial bus) */
  int bus_ripper_direction;             /* only valid on buses */


  int font_text_size;			/* used only with fonts defs */
  GList *font_prim_objs;			/* used only with fonts defs */

  int whichend;    /* for pins only, either 0 or 1 */
  int pin_type;    /* for pins only, either NET or BUS */

  GList *attribs;       /* attribute stuff */
  int show_name_value;
  int visibility;
  OBJECT *attached_to;  /* when object is an attribute */
  OBJECT *copied_to;    /* used when copying attributes */

  GList *weak_refs; /* Weak references */
};

/* construction, destruction */

OBJECT*
s_basic_new_object (int type, char const *prefix);

OBJECT*
o_object_copy (TOPLEVEL *toplevel, OBJECT *selected);

void
s_delete_object (TOPLEVEL *toplevel, OBJECT *o_current);

/* methods */

gboolean
geda_object_calculate_visible_bounds (TOPLEVEL *toplevel,
                                      OBJECT *o_current,
                                      gint *rleft,
                                      gint *rtop,
                                      gint *rright,
                                      gint *rbottom);

gint
geda_object_get_color (const GedaObject *object);

gint
geda_object_get_drawing_color (const GedaObject *object);

gboolean
geda_object_get_position (const GedaObject *object, gint *x, gint *y);

gboolean
geda_object_get_selectable (const GedaObject *object);

gint
geda_object_get_visible (const GedaObject *object);

void
geda_object_rotate (TOPLEVEL *toplevel,
                    int world_centerx,
                    int world_centery,
                    int angle,
                    OBJECT *object);

void
geda_object_mirror (TOPLEVEL *toplevel,
                    int world_centerx,
                    int world_centery,
                    OBJECT *object);

void
geda_object_set_selectable (GedaObject *object, gboolean selectable);

double
geda_object_shortest_distance (TOPLEVEL *toplevel,
                               OBJECT *object,
                               int x,
                               int y);

double
geda_object_shortest_distance_full (TOPLEVEL *toplevel,
                                    OBJECT *object,
                                    int x,
                                    int y,
                                    int force_solid);

void
geda_object_translate (GedaObject *object, gint dx, gint dy);

gboolean
o_get_fill_options (OBJECT *object,
                    OBJECT_FILLING *type,
                    int *width,
                    int *pitch1,
                    int *angle1,
                    int *pitch2,
                    int *angle2);

gboolean
o_get_line_options (OBJECT *object,
                    OBJECT_END *end,
                    OBJECT_TYPE *type,
                    int *width,
                    int *length,
                    int *space);

PAGE*
o_get_page (TOPLEVEL *toplevel, OBJECT *object);

OBJECT*
o_get_parent (TOPLEVEL *toplevel, OBJECT *object);

gboolean
o_is_visible (const TOPLEVEL *toplevel, const OBJECT *object);

void
o_set_color(TOPLEVEL *toplevel, OBJECT *object, int color);

void
o_set_fill_options(TOPLEVEL *toplevel,
                   OBJECT *o_current,
                   OBJECT_FILLING type,
                   int width,
                   int pitch1,
                   int angle1,
                   int pitch2,
                   int angle2);

void
o_set_line_options (TOPLEVEL *toplevel,
                    OBJECT *o_current,
                    OBJECT_END end,
                    OBJECT_TYPE type,
                    int width,
                    int length,
                    int space);

void
o_set_visibility (TOPLEVEL *toplevel, OBJECT *object, int visibility);

void
o_add_change_notify (TOPLEVEL *toplevel,
                     ChangeNotifyFunc pre_change_func,
                     ChangeNotifyFunc change_func,
                     void *user_data);

void
o_remove_change_notify (TOPLEVEL *toplevel,
                        ChangeNotifyFunc pre_change_func,
                        ChangeNotifyFunc change_func,
                        void *user_data);

void
s_object_weak_ref (OBJECT *object,
                   void (*notify_func)(void *, void *),
                   void *user_data);

void
s_object_weak_unref (OBJECT *object,
                     void (*notify_func)(void *, void *),
                     void *user_data);

void
s_object_add_weak_ptr (OBJECT *object, void *weak_pointer_loc);

void
s_object_remove_weak_ptr (OBJECT *object, void *weak_pointer_loc);
