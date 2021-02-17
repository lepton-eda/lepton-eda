/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2021 Lepton EDA Contributors
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

G_BEGIN_DECLS

struct st_object
{
  int type;                             /* Basic information */
  int sid;
  char *name;

  LeptonPage *page; /* Parent page */

  GedaBounds bounds;

  COMPONENT *component;
  GedaLine *line;
  GedaCircle *circle;
  GedaArc *arc;
  BOX *box;
  TEXT *text;
  PICTURE *picture;
  PATH *path;

  GList *conn_list;                     /* List of connections */
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

  gchar *component_basename;            /* Component Library Symbol name */
  LeptonObject *parent;                 /* Parent object pointer */

  int color;                            /* Which color */
  int dont_redraw;                      /* Flag to skip redrawing */
  int selectable;                       /* object selectable flag */
  int selected;                         /* object selected flag */

  /* controls which direction bus rippers go */
  /* it is either 0 for un-inited, */
  /* 1 for right, -1 for left (horizontal bus) */
  /* 1 for up, -1 for down (vertial bus) */
  int bus_ripper_direction;             /* only valid on buses */


  int font_text_size;                   /* used only with fonts defs */
  GList *font_prim_objs;                /* used only with fonts defs */

  int whichend;    /* for pins only, either 0 or 1 */
  int pin_type;    /* for pins only, either NET or BUS */

  GList *attribs;       /* attribute stuff */
  int show_name_value;
  int visibility;
  LeptonObject *attached_to;  /* when object is an attribute */
  LeptonObject *copied_to;    /* used when copying attributes */

  GList *weak_refs; /* Weak references */
};


/* Accessors. */
int
lepton_object_get_id (LeptonObject *object);

void
lepton_object_set_id (LeptonObject *object,
                      int id);
int
lepton_object_get_type (const LeptonObject *object);

void
lepton_object_set_type (LeptonObject *object,
                        int type);

/* Helpers. */

gboolean
lepton_object_is_arc (const LeptonObject *object);

gboolean
lepton_object_is_box (const LeptonObject *object);

/* construction, destruction */

LeptonObject*
s_basic_new_object (int type, char const *prefix);

LeptonObject*
o_object_copy (LeptonObject *selected);

void
s_delete_object (LeptonObject *o_current);

/* methods */

gboolean
geda_object_calculate_visible_bounds (LeptonObject *o_current,
                                      gboolean include_hidden,
                                      gint *rleft,
                                      gint *rtop,
                                      gint *rright,
                                      gint *rbottom);

gint
lepton_object_get_color (const LeptonObject *object);

gint
lepton_object_get_drawing_color (const LeptonObject *object);

gboolean
geda_object_get_position (const LeptonObject *object, gint *x, gint *y);

gboolean
geda_object_get_selectable (const LeptonObject *object);

gint
geda_object_get_visible (const LeptonObject *object);

void
geda_object_rotate (int world_centerx,
                    int world_centery,
                    int angle,
                    LeptonObject *object);

void
geda_object_mirror (int world_centerx,
                    int world_centery,
                    LeptonObject *object);

void
geda_object_set_selectable (LeptonObject *object, gboolean selectable);

double
geda_object_shortest_distance (LeptonObject *object,
                               int x,
                               int y,
                               gboolean include_hidden);

double
geda_object_shortest_distance_full (LeptonObject *object,
                                    int x,
                                    int y,
                                    int force_solid,
                                    gboolean include_hidden);

void
geda_object_translate (LeptonObject *object, gint dx, gint dy);

gboolean
o_get_fill_options (LeptonObject *object,
                    OBJECT_FILLING *type,
                    int *width,
                    int *pitch1,
                    int *angle1,
                    int *pitch2,
                    int *angle2);

gboolean
o_get_line_options (LeptonObject *object,
                    OBJECT_END *end,
                    OBJECT_TYPE *type,
                    int *width,
                    int *length,
                    int *space);

LeptonPage*
o_get_page (LeptonObject *object);

LeptonObject*
o_get_parent (LeptonObject *object);

gboolean
o_is_visible (const LeptonObject *object);

void
lepton_object_set_color (LeptonObject *object,
                         int color);

void
o_set_fill_options (LeptonObject *o_current,
                    OBJECT_FILLING type,
                    int width,
                    int pitch1,
                    int angle1,
                    int pitch2,
                    int angle2);

void
o_set_line_options (LeptonObject *o_current,
                    OBJECT_END end,
                    OBJECT_TYPE type,
                    int width,
                    int length,
                    int space);

void
o_set_visibility (LeptonObject *object,
                  int visibility);

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
s_object_weak_ref (LeptonObject *object,
                   void (*notify_func)(void *, void *),
                   void *user_data);

void
s_object_weak_unref (LeptonObject *object,
                     void (*notify_func)(void *, void *),
                     void *user_data);

void
s_object_add_weak_ptr (LeptonObject *object, void *weak_pointer_loc);

void
s_object_remove_weak_ptr (LeptonObject *object, void *weak_pointer_loc);

G_END_DECLS
