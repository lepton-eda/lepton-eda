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

/*! \file o_basic.c
 *  \brief functions for the basic object type
 *  
 *  This file contains the code used to handle OBJECTs (st_object).
 *  The object is the basic type of all elements stored in schematic
 *  and symbol files.
 *
 *  The object be extended to become concrete objects like a line,
 *  a pin, text, a circle or a picture. These extentions are substructures
 *  in the object struct.
 *  The subobjects are picture (st_picture), path (st_path), arcs (st_arc),
 *  a line (st_line), box (st_box), circle (st_circle), text (st_text) and
 *  a complex type (st_complex).
 *
 *  Pins, nets and busses are just a kind of a line.
 *
 *  The complex object can carry many primary objects. If the complex
 *  object is a symbol, then the complex symbol contains all the pins,
 *  the text and the graphics.
 *  
 *  \image html o_object_relations.png
 *  \image latex o_object_relations.pdf "object relations" width=14cm
 */

#include <config.h>

#include <stdio.h>

/* instrumentation code */
#if 0
#include <sys/time.h>
#include <unistd.h>
#endif

#include "libgeda_priv.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! Default setting for object selection function. */
void (*select_func)() = NULL;


/*! \brief Check if point is inside a region
 *  \par Function Description
 *  This function takes a rectangular region and a point.  It will check
 *  if the point is located in the region or not.
 *
 *  \param [in] xmin    Smaller x coordinate of the region.
 *  \param [in] ymin    Smaller y coordinate of the region.
 *  \param [in] xmax    Larger x coordinate of the region.
 *  \param [in] ymax    Larger y coordinate of the region.
 *  \param [in] x       x coordinate of the point to check.
 *  \param [in] y       y coordinate of the point to check.
 *  \return 1 if the point is inside the region, 0 otherwise.
 */
int inside_region(int xmin, int ymin, int xmax, int ymax, int x, int y)
{
  return ((x >= xmin && x <= xmax && y >= ymin && y <= ymax) ? 1 : 0);
}

/*! \brief Recalculate position of the given object.
 *  \par Function Description
 *  This function will take an object and recalculate its
 *  position on the screen.
 *
 *  \param [in]     toplevel    The TOPLEVEL object.
 *  \param [in,out] o_current    OBJECT to recalculate.
 *
 */
void o_recalc_single_object(TOPLEVEL *toplevel, OBJECT *o_current)
{
  if (o_current != NULL) {
    switch(o_current->type) {

      case(OBJ_LINE):
        o_line_recalc(toplevel, o_current);
        break;

      case(OBJ_NET):
        o_net_recalc(toplevel, o_current);
        break;

      case(OBJ_BUS):
        o_bus_recalc(toplevel, o_current);
        break;

      case(OBJ_BOX):
        o_box_recalc(toplevel, o_current);
        break;

      case(OBJ_PATH):
        o_path_recalc(toplevel, o_current);
        break;

      case(OBJ_PICTURE):
        o_picture_recalc(toplevel, o_current);
        break;

      case(OBJ_CIRCLE):
        o_circle_recalc(toplevel, o_current);
        break;

      case(OBJ_COMPLEX):
      case(OBJ_PLACEHOLDER):
        o_complex_recalc(toplevel, o_current);
        break;

      case(OBJ_PIN):
        o_pin_recalc(toplevel, o_current);
        break;

      case(OBJ_ARC):
        o_arc_recalc(toplevel, o_current);
        break;

      case(OBJ_TEXT):
        o_text_recalc(toplevel, o_current);
        break;
    }
  }
}


/*! \brief Recalculate position of a list (GList) of objects.
 *  \par Function Description
 *  This function will take a list (GList) of objects and recalculate their
 *  positions on the screen.
 *
 *  \param [in]     toplevel    The TOPLEVEL object.
 *  \param [in,out] object_glist  OBJECT list to recalculate.
 *
 */
void
o_recalc_object_glist(TOPLEVEL *toplevel, GList *object_glist)
{
  GList *list = object_glist;
  OBJECT *o_current;

  while (list != NULL) {
    o_current = (OBJECT *) list->data;
    o_recalc_single_object(toplevel, o_current);
   list = g_list_next(list);
  }
}


/*! \brief Set an #OBJECT's line options.
 *  \par Function Description
 *  This function allows a line's end, type, width, length and space to be set.
 *  See #OBJECT_END and #OBJECT_TYPE for information on valid
 *  object end and type values.
 *
 *  \param [in]     toplevel  The TOPLEVEL object.
 *  \param [in,out] o_current  OBJECT to set line options on.
 *  \param [in]     end        An OBJECT_END.
 *  \param [in]     type       An OBJECT_TYPE.
 *  \param [in]     width      Line width.
 *  \param [in]     length     Line length.
 *  \param [in]     space      Spacing between dashes/dots. Cannot be negative.
 *
 *  \todo Make space an unsigned int and check for a max value instead.
 *        If a max value is not required, then it would simplify the code.
 */
void o_set_line_options(TOPLEVEL *toplevel, OBJECT *o_current,
			OBJECT_END end, OBJECT_TYPE type,
			int width, int length, int space) 
{
  if(o_current == NULL) {
    return;
  }

  /* do some error checking / correcting */
  switch(type) {
    case(TYPE_DOTTED):
    if (space < 1) {
      space = 100;
      s_log_message (_("Invalid space specified, setting to 100\n"));
    }
    break;
    case(TYPE_DASHED):
    case(TYPE_CENTER):
    case(TYPE_PHANTOM):
    if (length < 1) {
      length = 100;
      s_log_message (_("Invalid length specified, setting to 100\n"));
    }
    if (space < 1) {
      space = 100;
      s_log_message (_("Invalid space specified, setting to 100\n"));
    }
    break;
    default:
    
    break;
  }
  
  o_current->line_width = width;
  o_current->line_end   = end;
  o_current->line_type  = type;

  o_current->line_length = length;
  o_current->line_space  = space;

  /* Recalculate the object's bounding box */
  o_recalc_single_object( toplevel, o_current );
}

/*! \brief get #OBJECT's line properties.
 *  \par Function Description
 *  This function get's the #OBJECT's line options.
 *  See #OBJECT_END and #OBJECT_TYPE for information on valid
 *  object end and type values.
 *
 *  \param [in]   object    OBJECT to read the properties
 *  \param [out]  end       An OBJECT_END.
 *  \param [out]  type      An OBJECT_TYPE.
 *  \param [out]  width     Line width.
 *  \param [out]  length    Line length.
 *  \param [out]  space     Spacing between dashes/dots.
 *  \return TRUE on succes, FALSE otherwise
 *
 */
gboolean o_get_line_options(OBJECT *object,
                            OBJECT_END *end, OBJECT_TYPE *type,
                            int *width, int *length, int *space)
{
  if (object->type != OBJ_LINE
      && object->type != OBJ_ARC
      && object->type != OBJ_BOX
      && object->type != OBJ_CIRCLE
      && object->type != OBJ_PATH)
    return FALSE;

  *end = object->line_end;
  *type = object->line_type;
  *width = object->line_width;
  *length = object->line_length;
  *space = object->line_space;

  return TRUE;
}

/*! \brief Set #OBJECT's fill options.
 *  \par Function Description
 *  This function allows an #OBJECT's fill options to be configured.
 *  See #OBJECT_FILLING for information on valid fill types.
 *
 *  \param [in]      toplevel  The TOPLEVEL object.
 *  \param [in,out]  o_current  OBJECT to be updated.
 *  \param [in]      type       OBJECT_FILLING type.
 *  \param [in]      width      fill width.
 *  \param [in]      pitch1     cross hatch line distance
 *  \param [in]      angle1     cross hatch angle
 *  \param [in]      pitch2     cross hatch line distance
 *  \param [in]      angle2     cross hatch angle
 *
 */
void o_set_fill_options(TOPLEVEL *toplevel, OBJECT *o_current,
			OBJECT_FILLING type, int width,
			int pitch1, int angle1,
			int pitch2, int angle2) 
{
  if(o_current == NULL) {
    return;
  }

  o_current->fill_type = type;
  o_current->fill_width = width;

  o_current->fill_pitch1 = pitch1;
  o_current->fill_angle1 = angle1;

  o_current->fill_pitch2 = pitch2;
  o_current->fill_angle2 = angle2;
	
}

/*! \brief get #OBJECT's fill properties.
 *  \par Function Description
 *  This function get's the #OBJECT's fill options.
 *  See #OBJECT_FILLING for information on valid fill types.
 *
 *  \param [in]   object    OBJECT to read the properties
 *  \param [out]  type      OBJECT_FILLING type
 *  \param [out]  width     fill width.
 *  \param [out]  pitch1    cross hatch line distance
 *  \param [out]  angle1    cross hatch angle
 *  \param [out]  pitch2    cross hatch line distance
 *  \param [out]  angle2    cross hatch angle
 *  \return TRUE on succes, FALSE otherwise
 *
 */
gboolean o_get_fill_options(OBJECT *object,
                            OBJECT_FILLING *type, int *width,
                            int *pitch1, int *angle1,
                            int *pitch2, int *angle2)
{
  if (object->type != OBJ_BOX
      && object->type != OBJ_CIRCLE
      && object->type != OBJ_PATH)
    return FALSE;

  *type = object->fill_type;
  *width = object->fill_width;
  *pitch1 = object->fill_pitch1;
  *angle1 = object->fill_angle1;
  *pitch2 = object->fill_pitch2;
  *angle2 = object->fill_angle2;

  return TRUE;
}

/*! \brief get the base position of an object
 *  \par Function Description
 *  This function gets the position of an object in world coordinates.
 *
 *  \param [in] toplevel The toplevel environment.
 *  \param [out] x       pointer to the x-position
 *  \param [out] y       pointer to the y-position
 *  \param [in] object   The object to get the position.
 *  \return TRUE if successfully determined the position, FALSE otherwise
 */
gboolean o_get_position (TOPLEVEL *toplevel, gint *x, gint *y, OBJECT *object)
{
  gboolean (*func) (TOPLEVEL*, int*, int*, OBJECT*) = NULL;

  switch (object->type) {
      case OBJ_LINE:    func = o_line_get_position;    break;
      case OBJ_NET:     func = o_net_get_position;     break;
      case OBJ_BUS:     func = o_bus_get_position;     break;
      case OBJ_BOX:     func = o_box_get_position;     break;
      case OBJ_PICTURE: func = o_picture_get_position; break;
      case OBJ_CIRCLE:  func = o_circle_get_position;  break;
      case OBJ_PLACEHOLDER:
      case OBJ_COMPLEX: func = o_complex_get_position; break;
      case OBJ_TEXT:    func = o_text_get_position;    break;
      case OBJ_PATH:    func = o_path_get_position;    break;
      case OBJ_PIN:     func = o_pin_get_position;     break;
      case OBJ_ARC:     func = o_arc_get_position;     break;
      default:
        g_critical ("o_get_position: object %p has bad type '%c'\n",
                    object, object->type);
  }

  if (func != NULL) {
    return (*func) (toplevel, x, y, object);
  }
  return FALSE;
}


/*! \brief Translates an object in world coordinates
 *  \par Function Description
 *  This function translates the object <B>object</B> by
 *  <B>dx</B> and <B>dy</B>.
 *
 *  \param [in] toplevel The toplevel environment.
 *  \param [in] dx       Amount to horizontally translate object
 *  \param [in] dy       Amount to vertically translate object
 *  \param [in] object   The object to translate.
 */
void o_translate_world (TOPLEVEL *toplevel, gint dx, gint dy, OBJECT *object)
{
  void (*func) (TOPLEVEL*, int, int, OBJECT*) = NULL;

  switch (object->type) {
      case OBJ_LINE:    func = o_line_translate_world;    break;
      case OBJ_NET:     func = o_net_translate_world;     break;
      case OBJ_BUS:     func = o_bus_translate_world;     break;
      case OBJ_BOX:     func = o_box_translate_world;     break;
      case OBJ_PICTURE: func = o_picture_translate_world; break;
      case OBJ_CIRCLE:  func = o_circle_translate_world;  break;
      case OBJ_PLACEHOLDER:
      case OBJ_COMPLEX: func = o_complex_translate_world; break;
      case OBJ_TEXT:    func = o_text_translate_world;    break;
      case OBJ_PATH:    func = o_path_translate_world;    break;
      case OBJ_PIN:     func = o_pin_translate_world;     break;
      case OBJ_ARC:     func = o_arc_translate_world;     break;
      default:
        g_critical ("o_translate_world: object %p has bad type '%c'\n",
                    object, object->type);
  }

  if (func != NULL) {
    (*func) (toplevel, dx, dy, object);
  }
}


/*! \brief Rotates an object in world coordinates
 *  \par Function Description
 *  This function rotates the object <B>object</B> about the coordinates
 *  <B>world_centerx</B> and <B>world_centery</B>, by <B>angle</B>degrees.
 *
 *  \param [in] toplevel The toplevel environment.
 *  \param [in] world_centerx  X coordinate of rotation center (world coords)
 *  \param [in] world_centery  Y coordinate of rotation center (world coords)
 *  \param [in] angle          Angle of rotation (degrees)
 *  \param [in] object         The object to rotate.
 */
void o_rotate_world (TOPLEVEL *toplevel, int world_centerx, int world_centery, int angle, OBJECT *object)
{
  void (*func) (TOPLEVEL*, int, int, int, OBJECT*) = NULL;

  switch (object->type) {
      case OBJ_LINE:    func = o_line_rotate_world;       break;
      case OBJ_NET:     func = o_net_rotate_world;        break;
      case OBJ_BUS:     func = o_bus_rotate_world;        break;
      case OBJ_BOX:     func = o_box_rotate_world;        break;
      case OBJ_PICTURE: func = o_picture_rotate_world;    break;
      case OBJ_CIRCLE:  func = o_circle_rotate_world;     break;
      case OBJ_PLACEHOLDER:
      case OBJ_COMPLEX: func = o_complex_rotate_world;    break;
      case OBJ_TEXT:    func = o_text_rotate_world;       break;
      case OBJ_PATH:    func = o_path_rotate_world;       break;
      case OBJ_PIN:     func = o_pin_rotate_world;        break;
      case OBJ_ARC:     func = o_arc_rotate_world;        break;
      default:
        g_critical ("o_rotate_world: object %p has bad type '%c'\n",
                    object, object->type);
  }

  if (func != NULL) {
    (*func) (toplevel, world_centerx, world_centery, angle, object);
  }
}


/*! \brief Mirrors an object in world coordinates
 *  \par Function Description
 *  This function mirrors an object about the point
 *  (<B>world_centerx</B>,<B>world_centery</B>) in world units.
 *
 *  \param [in]     toplevel       The TOPLEVEL object.
 *  \param [in]     world_centerx  Origin x coordinate in WORLD units.
 *  \param [in]     world_centery  Origin y coordinate in WORLD units.
 *  \param [in,out] object         The OBJECT to mirror.
 */
void o_mirror_world (TOPLEVEL *toplevel, int world_centerx, int world_centery, OBJECT *object)
{
  void (*func) (TOPLEVEL*, int, int, OBJECT*) = NULL;

  switch (object->type) {
      case OBJ_LINE:    func = o_line_mirror_world;       break;
      case OBJ_NET:     func = o_net_mirror_world;        break;
      case OBJ_BUS:     func = o_bus_mirror_world;        break;
      case OBJ_BOX:     func = o_box_mirror_world;        break;
      case OBJ_PICTURE: func = o_picture_mirror_world;    break;
      case OBJ_CIRCLE:  func = o_circle_mirror_world;     break;
      case OBJ_PLACEHOLDER:
      case OBJ_COMPLEX: func = o_complex_mirror_world;    break;
      case OBJ_TEXT:    func = o_text_mirror_world;       break;
      case OBJ_PATH:    func = o_path_mirror_world;       break;
      case OBJ_PIN:     func = o_pin_mirror_world;        break;
      case OBJ_ARC:     func = o_arc_mirror_world;        break;
      default:
        g_critical ("o_mirror_world: object %p has bad type '%c'\n",
                    object, object->type);
  }

  if (func != NULL) {
    (*func) (toplevel, world_centerx, world_centery, object);
  }
}


/*! \brief Calculates the distance between the given point and the closest
 * point on the given object.
 *
 *  \param [in] object       The given object.
 *  \param [in] x            The x coordinate of the given point.
 *  \param [in] y            The y coordinate of the given point.
 *  \return The shortest distance from the object to the point. If the
 *  distance cannot be calculated, this function returns a really large
 *  number (G_MAXDOUBLE).  If an error occurs, this function returns
 *  G_MAXDOUBLE.
 */
double o_shortest_distance (OBJECT *object, int x, int y)
{
  return o_shortest_distance_full (object, x, y, FALSE);
}

/*! \brief Calculates the distance between the given point and the closest
 * point on the given object. Allows forcing objects to solid.
 *
 *  \param [in] object       The given object.
 *  \param [in] x            The x coordinate of the given point.
 *  \param [in] y            The y coordinate of the given point.
 *  \param [in] force_solid  If true, force treating the object as solid.
 *  \return The shortest distance from the object to the point. If the
 *  distance cannot be calculated, this function returns a really large
 *  number (G_MAXDOUBLE).  If an error occurs, this function returns
 *  G_MAXDOUBLE.
 */
double o_shortest_distance_full (OBJECT *object, int x, int y, int force_solid)
{
  double shortest_distance = G_MAXDOUBLE;
  double (*func) (OBJECT *, int, int, int) = NULL;

  g_return_val_if_fail (object != NULL, G_MAXDOUBLE);

  switch(object->type) {
    case OBJ_BUS:
    case OBJ_NET:
    case OBJ_PIN:
    case OBJ_LINE:        func = o_line_shortest_distance;     break;
    case OBJ_BOX:         func = o_box_shortest_distance;      break;
    case OBJ_PICTURE:     func = o_picture_shortest_distance;  break;
    case OBJ_CIRCLE:      func = o_circle_shortest_distance;   break;
    case OBJ_PLACEHOLDER:
    case OBJ_COMPLEX:     func = o_complex_shortest_distance;  break;
    case OBJ_TEXT:        func = o_text_shortest_distance;     break;
    case OBJ_PATH:        func = o_path_shortest_distance;     break;
    case OBJ_ARC:         func = o_arc_shortest_distance;      break;
    default:
      g_critical ("o_shortest_distance: object %p has bad type '%c'\n",
                  object, object->type);
  }

  if (func != NULL) {
    shortest_distance = (*func) (object, x, y, force_solid);
  }

  return shortest_distance;
}

/*! \brief Mark an OBJECT's cached bounds as invalid
 *  \par Function Description
 *  Marks the cached bounds of the given OBJECT as having been
 *  invalidated and in need of an update. They will be recalculated
 *  next time the OBJECT's bounds are requested (e.g. via
 *  world_get_single_object_bounds() ).
 *
 *  \param [in] toplevel
 *  \param [in] obj
 *
 *  \todo Turn this into a macro?
 */
void o_bounds_invalidate(TOPLEVEL *toplevel, OBJECT *obj)
{
  obj->w_bounds_valid = FALSE;
}


/*! \brief Change the color of an object
 *
 *  \par Function Description
 *  This function changes the color of an object.
 *
 *  \param [in] toplevel  The TOPLEVEL structure.
 *  \param [in] object    The OBJECT to change color.
 *  \param [in] color     The new color.
 */
void o_set_color (TOPLEVEL *toplevel, OBJECT *object, int color)
{
  g_return_if_fail (object != NULL);

  object->color = color;

  if (object->type == OBJ_COMPLEX ||
      object->type == OBJ_PLACEHOLDER)
    o_glist_set_color (toplevel, object->complex->prim_objs, color);
}
