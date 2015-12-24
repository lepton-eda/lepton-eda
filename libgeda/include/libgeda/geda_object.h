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

  int w_top;				/* Bounding box information */
  int w_left;				/* in world coords */
  int w_right;
  int w_bottom;
  TOPLEVEL *w_bounds_valid_for;

  COMPLEX *complex;
  LINE *line;
  CIRCLE *circle;
  ARC *arc;
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
  int locked_color; 			/* Locked color (used to save */
  /* the object's real color */
  /* when the object is locked) */

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

