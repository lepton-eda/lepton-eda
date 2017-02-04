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

/*! this is modified here and in o_list.c */
int global_sid=0;


/*! \brief Get the color index of the object
 *
 *  If this function fails, it returns the DEFAULT_COLOR.
 *
 *  \param [in] object the object to obtain the color of
 *  \return the color index of the object
 */
gint
geda_object_get_color (const GedaObject *object)
{
  g_return_val_if_fail (object != NULL, DEFAULT_COLOR);
  g_return_val_if_fail (object->color >= 0, DEFAULT_COLOR);
  g_return_val_if_fail (object->color < MAX_COLORS, DEFAULT_COLOR);

  return object->color;
}

/*! \brief Get the color for drawing the object
 *
 *  If this function fails, it returns the DEFAULT_COLOR.
 *
 *  The output of this funtion is ependent on other variables than just the
 *  object color. If the object is locked, it will return the LOCK_COLOR.
 *  This value should not be used for saving or editing the object.
 *
 *  \param [in] object the object to obtain the color of
 *  \return the color index the draw the object
 */
gint
geda_object_get_drawing_color (const GedaObject *object)
{
  gint color;

  g_return_val_if_fail (object != NULL, DEFAULT_COLOR);

  color = object->selectable ? object->color : LOCK_COLOR;

  g_return_val_if_fail (color >= 0, DEFAULT_COLOR);
  g_return_val_if_fail (color < MAX_COLORS, DEFAULT_COLOR);

  return color;
}

/*! \brief Determines if the object can be selected
 *
 *  Locked is alternate terminology for not selectable.
 *
 *  \param [in] object the object
 *  \retval TRUE if the object can be selected
 *  \retval FALSE if the object cannot be selected
 *  \retval FALSE if a failure occured
 */
gboolean
geda_object_get_selectable (const GedaObject *object)
{
  g_return_val_if_fail (object != NULL, FALSE);

  return object->selectable;
}

/*! \brief Sets if the object can be selected
 *
 *  Locked is alternate terminology for not selectable.
 *
 *  \param [in,out] object the object
 *  \param [in] selectable true if the object is selectable
 */
void
geda_object_set_selectable (GedaObject *object, gboolean selectable)
{
  g_return_if_fail (object != NULL);

  object->selectable = selectable;
}

static OBJECT*
s_basic_init_object (OBJECT *new_node, int type, char const *name);

/*! \brief Helper to allocate and initialise an object.
 *
 *  \par Function Description
 *  Allocates memory for an OBJECT and then calls s_basic_init_object() on it.
 *
 *  \param [in] type      The sub-type of the object to create; one of the OBJ_* constants.
 *  \param [in] prefix    The name prefix for the session-unique object name.
 *  \return A pointer to the fully constructed OBJECT.
 */
OBJECT*
s_basic_new_object (int type, char const *prefix)
{
  return s_basic_init_object(g_malloc(sizeof (OBJECT)), type, prefix);
}

/*! \todo Finish documentation!!!!
 *  \brief
 *  \par Function Description
 *  returns head !!!!!!!!!!!!!!!!!!!
 *  look at above.. this returns what was passed in!!!!
 *  copies selected to list_head (!! returns new list)
 *
 *  \param [in]  toplevel   The TOPLEVEL object.
 *  \param [in]  selected
 *  \return OBJECT pointer.
 */
OBJECT *o_object_copy (TOPLEVEL *toplevel,
                       OBJECT *selected)
{
  OBJECT *new_obj;

  g_return_val_if_fail (toplevel != NULL, NULL);
  g_return_val_if_fail (selected != NULL, NULL);

  switch(selected->type) {

    case(OBJ_LINE):
      new_obj = geda_line_object_copy (toplevel, selected);
      break;

    case(OBJ_NET):
      new_obj = geda_net_object_copy (toplevel, selected);
      break;

    case(OBJ_BUS):
      new_obj = geda_bus_object_copy (toplevel, selected);
      break;

    case(OBJ_BOX):
      new_obj = geda_box_object_copy (toplevel, selected);
      break;

    case(OBJ_PICTURE):
      new_obj = o_picture_copy (toplevel, selected);
      break;

    case(OBJ_CIRCLE):
      new_obj = geda_circle_object_copy (toplevel, selected);
      break;

    case(OBJ_COMPLEX):
    case(OBJ_PLACEHOLDER):
      new_obj = o_complex_copy (toplevel, selected);
      break;

    case(OBJ_TEXT):
      new_obj = geda_text_object_copy (toplevel, selected);
      break;

    case(OBJ_PATH):
      new_obj = geda_path_object_copy (toplevel, selected);
      break;

    case(OBJ_PIN):
      new_obj = geda_pin_object_copy (toplevel, selected);
      break;

    case(OBJ_ARC):
      new_obj = geda_arc_object_copy (toplevel, selected);
      break;

    default:
      g_critical ("o_list_copy_to: object %p has bad type '%c'\n",
                  selected, selected->type);
      return NULL;
  }

  /* Store a reference in the copied object to where it was copied.
   * Used to retain associations when copying attributes */
  selected->copied_to = new_obj;

  /* make sure sid is the same! */
  if (selected) {
    new_obj->sid = selected->sid;
  }

  return new_obj;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
s_delete_object(TOPLEVEL *toplevel, OBJECT *o_current)
{
  if (o_current != NULL) {
    /* If currently attached to a page, remove it from the page */
    if (o_current->page != NULL) {
      s_page_remove (toplevel, o_current->page, o_current);
    }

    s_conn_remove_object_connections (toplevel, o_current);

    if (o_current->attached_to != NULL) {
      /* do the actual remove */
      o_attrib_remove(toplevel, &o_current->attached_to->attribs, o_current);
    }

    /* printf("sdeleting line\n"); */
    geda_line_free (o_current->line);
    o_current->line = NULL;

    geda_path_free (o_current->path);
    o_current->path = NULL;

    /*	printf("sdeleting circle\n");*/
    geda_circle_free (o_current->circle);
    o_current->circle = NULL;

    /*	printf("sdeleting arc\n");*/
    geda_arc_free (o_current->arc);
    o_current->arc = NULL;

    /*	printf("sdeleting box\n");*/
    geda_box_free (o_current->box);
    o_current->box = NULL;

    geda_picture_free (o_current->picture);
    o_current->picture = NULL;

    if (o_current->text) {
      /*printf("sdeleting text->string\n");*/
      g_free(o_current->text->string);
      o_current->text->string = NULL;
      g_free(o_current->text->disp_string);
      /*	printf("sdeleting text\n");*/
      g_free(o_current->text);
    }
    o_current->text = NULL;

    /*	printf("sdeleting name\n");*/
    g_free(o_current->name);
    o_current->name = NULL;


    /*	printf("sdeleting complex_basename\n");*/
    g_free(o_current->complex_basename);
    o_current->complex_basename = NULL;

    if (o_current->complex) {

      if (o_current->complex->prim_objs) {
        /* printf("sdeleting complex->primitive_objects\n");*/
        geda_object_list_delete (toplevel, o_current->complex->prim_objs);
        o_current->complex->prim_objs = NULL;
      }

      g_free(o_current->complex);
      o_current->complex = NULL;
    }

    o_attrib_detach_all (toplevel, o_current);

    s_weakref_notify (o_current, o_current->weak_refs);

    g_free(o_current);	/* assuming it is not null */

    o_current=NULL;		/* misc clean up */
  }
}

/*! \brief Add a weak reference watcher to an OBJECT.
 * \par Function Description
 * Adds the weak reference callback \a notify_func to \a object.  When
 * \a object is destroyed, \a notify_func will be called with two
 * arguments: the \a object, and the \a user_data.
 *
 * \sa s_object_weak_unref
 *
 * \param [in,out] object     Object to weak-reference.
 * \param [in] notify_func    Weak reference notify function.
 * \param [in] user_data      Data to be passed to \a notify_func.
 */
void
s_object_weak_ref (OBJECT *object,
                   void (*notify_func)(void *, void *),
                   void *user_data)
{
  g_return_if_fail (object != NULL);
  object->weak_refs = s_weakref_add (object->weak_refs, notify_func, user_data);
}

/*! \brief Remove a weak reference watcher from an OBJECT.
 * \par Function Description
 * Removes the weak reference callback \a notify_func from \a object.
 *
 * \sa s_object_weak_ref()
 *
 * \param [in,out] object     Object to weak-reference.
 * \param [in] notify_func    Notify function to search for.
 * \param [in] user_data      Data to to search for.
 */
void
s_object_weak_unref (OBJECT *object,
                     void (*notify_func)(void *, void *),
                     void *user_data)
{
  g_return_if_fail (object != NULL);
  object->weak_refs = s_weakref_remove (object->weak_refs,
                                        notify_func, user_data);
}

/*! \brief Add a weak pointer to an OBJECT.
 * \par Function Description
 * Adds the weak pointer at \a weak_pointer_loc to \a object. The
 * value of \a weak_pointer_loc will be set to NULL when \a object is
 * destroyed.
 *
 * \sa s_object_remove_weak_ptr
 *
 * \param [in,out] object        Object to weak-reference.
 * \param [in] weak_pointer_loc  Memory address of a pointer.
 */
void
s_object_add_weak_ptr (OBJECT *object,
                       void *weak_pointer_loc)
{
  g_return_if_fail (object != NULL);
  object->weak_refs = s_weakref_add_ptr (object->weak_refs, weak_pointer_loc);
}

/*! \brief Remove a weak pointer from an OBJECT.
 * \par Function Description
 * Removes the weak pointer at \a weak_pointer_loc from \a object.
 *
 * \sa s_object_add_weak_ptr()
 *
 * \param [in,out] object        Object to weak-reference.
 * \param [in] weak_pointer_loc  Memory address of a pointer.
 */
void
s_object_remove_weak_ptr (OBJECT *object,
                          void *weak_pointer_loc)
{
  g_return_if_fail (object != NULL);
  object->weak_refs = s_weakref_remove_ptr (object->weak_refs,
                                            weak_pointer_loc);
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
  g_return_if_fail (o_current != NULL);

  /* do some error checking / correcting */
  switch(type) {
    case(TYPE_SOLID):
      length = -1;
      space = -1;
      break;
    case(TYPE_DOTTED):
      length = -1;
      if (space < 1) {
        space = 100;
      }
    break;
    case(TYPE_DASHED):
    case(TYPE_CENTER):
    case(TYPE_PHANTOM):
      if (length < 1) {
        length = 100;
      }
      if (space < 1) {
        space = 100;
      }
    break;
    default:

    break;
  }

  o_emit_pre_change_notify (toplevel, o_current);

  o_current->line_width = width;
  o_current->line_end   = end;
  o_current->line_type  = type;

  o_current->line_length = length;
  o_current->line_space  = space;

  /* Recalculate the object's bounding box */
  o_current->w_bounds_valid_for = NULL;
  o_emit_change_notify (toplevel, o_current);

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

  /* do some error checking / correcting */
  if (geda_fill_type_draw_first_hatch (type)) {
    if (width < 0) {
      width = 1;
    }
    if (angle1 < 0) {
      angle1 = 45;
    }
    if (pitch1 < 0) {
      pitch1 = 100;
    }
  } else {
    width = -1;
    angle1 = -1;
    pitch1 = -1;
  }

  if (geda_fill_type_draw_second_hatch (type)) {
    if (angle2 < 0) {
      angle2 = 135;
    }
    if (pitch2 < 0) {
      pitch2 = 100;
    }
  } else {
    angle2 = -1;
    pitch2 = -1;
  }

  o_emit_pre_change_notify (toplevel, o_current);

  o_current->fill_type = type;
  o_current->fill_width = width;

  o_current->fill_pitch1 = pitch1;
  o_current->fill_angle1 = angle1;

  o_current->fill_pitch2 = pitch2;
  o_current->fill_angle2 = angle2;

  o_emit_change_notify (toplevel, o_current);
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
gboolean
geda_object_get_position (const GedaObject *object, gint *x, gint *y)
{
  gboolean (*func) (const GedaObject*, int*, int*) = NULL;

  g_return_val_if_fail (object != NULL, FALSE);

  switch (object->type) {
      case OBJ_LINE:    func = geda_line_object_get_position;    break;
      case OBJ_NET:     func = geda_net_object_get_position;     break;
      case OBJ_BUS:     func = geda_bus_object_get_position;     break;
      case OBJ_BOX:     func = geda_box_object_get_position;     break;
      case OBJ_PICTURE: func = geda_picture_object_get_position; break;
      case OBJ_CIRCLE:  func = geda_circle_object_get_position;  break;
      case OBJ_PLACEHOLDER:
      case OBJ_COMPLEX: func = geda_complex_object_get_position; break;
      case OBJ_TEXT:    func = geda_text_object_get_position;    break;
      case OBJ_PATH:    func = geda_path_object_get_position;    break;
      case OBJ_PIN:     func = geda_pin_object_get_position;     break;
      case OBJ_ARC:     func = geda_arc_object_get_position;     break;
      default:
        g_critical ("geda_object_get_position: object %p has bad type '%c'\n",
                    object, object->type);
  }

  if (func != NULL) {
    return (*func) (object, x, y);
  }
  return FALSE;
}


/*! \brief Translates an object in world coordinates
 *  \par Function Description
 *  This function translates the object <B>object</B> by
 *  <B>dx</B> and <B>dy</B>.
 *
 *  \param [in] object   The object to translate.
 *  \param [in] dx       Amount to horizontally translate object
 *  \param [in] dy       Amount to vertically translate object
 */
void
geda_object_translate (GedaObject *object, gint dx, gint dy)
{
  void (*func) (GedaObject*, int, int) = NULL;

  switch (object->type) {
      case OBJ_LINE:    func = geda_line_object_translate;    break;
      case OBJ_NET:     func = geda_net_object_translate;     break;
      case OBJ_BUS:     func = geda_bus_object_translate;     break;
      case OBJ_BOX:     func = geda_box_object_translate;     break;
      case OBJ_PICTURE: func = geda_picture_object_translate; break;
      case OBJ_CIRCLE:  func = geda_circle_object_translate;  break;
      case OBJ_PLACEHOLDER:
      case OBJ_COMPLEX: func = geda_complex_object_translate; break;
      case OBJ_TEXT:    func = geda_text_object_translate;    break;
      case OBJ_PATH:    func = geda_path_object_translate;    break;
      case OBJ_PIN:     func = geda_pin_object_translate;     break;
      case OBJ_ARC:     func = geda_arc_object_translate;     break;
      default:
        g_critical ("geda_object_translate: object %p has bad type '%c'\n",
                    object, object->type);
  }

  if (func != NULL) {
    (*func) (object, dx, dy);
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
void geda_object_rotate (TOPLEVEL *toplevel, int world_centerx, int world_centery, int angle, OBJECT *object)
{
  void (*func) (TOPLEVEL*, int, int, int, OBJECT*) = NULL;

  switch (object->type) {
      case OBJ_LINE:    func = geda_line_object_rotate;    break;
      case OBJ_NET:     func = geda_net_object_rotate;     break;
      case OBJ_BUS:     func = geda_bus_object_rotate;     break;
      case OBJ_BOX:     func = geda_box_object_rotate;     break;
      case OBJ_PICTURE: func = geda_picture_object_rotate; break;
      case OBJ_CIRCLE:  func = geda_circle_object_rotate;  break;
      case OBJ_PLACEHOLDER:
      case OBJ_COMPLEX: func = geda_complex_object_rotate; break;
      case OBJ_TEXT:    func = geda_text_object_rotate;    break;
      case OBJ_PATH:    func = geda_path_object_rotate;    break;
      case OBJ_PIN:     func = geda_pin_object_rotate;     break;
      case OBJ_ARC:     func = geda_arc_object_rotate;     break;
      default:
        g_critical ("geda_object_rotate: object %p has bad type '%c'\n",
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
void geda_object_mirror (TOPLEVEL *toplevel, int world_centerx, int world_centery, OBJECT *object)
{
  void (*func) (TOPLEVEL*, int, int, OBJECT*) = NULL;

  switch (object->type) {
      case OBJ_LINE:    func = geda_line_object_mirror;    break;
      case OBJ_NET:     func = geda_net_object_mirror;     break;
      case OBJ_BUS:     func = geda_bus_object_mirror;     break;
      case OBJ_BOX:     func = geda_box_object_mirror;     break;
      case OBJ_PICTURE: func = geda_picture_object_mirror; break;
      case OBJ_CIRCLE:  func = geda_circle_object_mirror;  break;
      case OBJ_PLACEHOLDER:
      case OBJ_COMPLEX: func = geda_complex_object_mirror; break;
      case OBJ_TEXT:    func = geda_text_object_mirror;    break;
      case OBJ_PATH:    func = geda_path_object_mirror;    break;
      case OBJ_PIN:     func = geda_pin_object_mirror;     break;
      case OBJ_ARC:     func = geda_arc_object_mirror;     break;
      default:
        g_critical ("geda_object_mirror: object %p has bad type '%c'\n",
                    object, object->type);
  }

  if (func != NULL) {
    (*func) (toplevel, world_centerx, world_centery, object);
  }
}


/*! \brief Calculates the distance between the given point and the closest
 * point on the given object.
 *
 *  \param [in] toplevel     The TOPLEVEL object.
 *  \param [in] object       The given object.
 *  \param [in] x            The x coordinate of the given point.
 *  \param [in] y            The y coordinate of the given point.
 *  \return The shortest distance from the object to the point. If the
 *  distance cannot be calculated, this function returns a really large
 *  number (G_MAXDOUBLE).  If an error occurs, this function returns
 *  G_MAXDOUBLE.
 */
double
geda_object_shortest_distance (TOPLEVEL *toplevel, OBJECT *object, int x, int y)
{
  return geda_object_shortest_distance_full (toplevel, object, x, y, FALSE);
}

/*! \brief Calculates the distance between the given point and the closest
 * point on the given object. Allows forcing objects to solid.
 *
 *  \param [in] toplevel     The TOPLEVEL object.
 *  \param [in] object       The given object.
 *  \param [in] x            The x coordinate of the given point.
 *  \param [in] y            The y coordinate of the given point.
 *  \param [in] force_solid  If true, force treating the object as solid.
 *  \return The shortest distance from the object to the point. If the
 *  distance cannot be calculated, this function returns a really large
 *  number (G_MAXDOUBLE).  If an error occurs, this function returns
 *  G_MAXDOUBLE.
 */
double
geda_object_shortest_distance_full (TOPLEVEL *toplevel, OBJECT *object,
                                    int x, int y, int force_solid)
{
  double shortest_distance = G_MAXDOUBLE;
  double (*func) (TOPLEVEL *, OBJECT *, int, int, int) = NULL;

  g_return_val_if_fail (object != NULL, G_MAXDOUBLE);

  switch(object->type) {
    case OBJ_BUS:
    case OBJ_NET:
    case OBJ_PIN:
    case OBJ_LINE:        func = geda_line_object_shortest_distance;     break;
    case OBJ_BOX:         func = geda_box_object_shortest_distance;      break;
    case OBJ_PICTURE:     func = geda_picture_object_shortest_distance;  break;
    case OBJ_CIRCLE:      func = geda_circle_object_shortest_distance;   break;
    case OBJ_PLACEHOLDER:
    case OBJ_COMPLEX:     func = geda_complex_object_shortest_distance;  break;
    case OBJ_TEXT:        func = geda_text_object_shortest_distance;     break;
    case OBJ_PATH:        func = geda_path_object_shortest_distance;     break;
    case OBJ_ARC:         func = geda_arc_object_shortest_distance;      break;
    default:
      g_critical ("geda_object_shortest_distance: object %p has bad type '%c'\n",
                  object, object->type);
  }

  if (func != NULL) {
    shortest_distance = (*func) (toplevel, object, x, y, force_solid);
  }

  return shortest_distance;
}

/*! \brief Mark an OBJECT's cached bounds as invalid
 *  \par Function Description
 *  Recursively marks the cached bounds of the given OBJECT and its
 *  parents as having been invalidated and in need of an update. They
 *  will be recalculated next time the OBJECT's bounds are requested
 *  (e.g. via geda_object_calculate_visible_bounds() ).
 *  \param [in] toplevel
 *  \param [in] object
 *
 *  \todo Turn this into a macro?
 */
void
o_bounds_invalidate (TOPLEVEL *toplevel, GedaObject *object)
{
  GedaObject *iter = object;

  while (iter != NULL) {
    iter->w_bounds_valid_for = NULL;
    iter = iter->parent;
  }
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
    geda_object_list_set_color (object->complex->prim_objs, color, toplevel);
}


/*! \brief Get an object's parent PAGE.
 *
 * \par Function Description
 * Returns the PAGE structure which owns \a object. If \a object is
 * not currently associated with a PAGE, returns NULL. If \a object is
 * part of a compound object, recurses upward.
 *
 * \param [in] toplevel  The TOPLEVEL structure.
 * \param [in] object    The OBJECT for which to retrieve the parent PAGE.
 * \return The PAGE which owns \a object or NULL.
 *
 * \sa s_page_append_object() s_page_append() s_page_remove()
 */
PAGE *
o_get_page (TOPLEVEL *toplevel, OBJECT *object)
{
  if (object->parent != NULL) {
    return o_get_page (toplevel, object->parent);
  }
  return object->page;
}

/*! \brief Get an object's containing complex object.
 *
 * \par Function Description
 * If \a object is part of a complex #OBJECT, returns that
 * #OBJECT. Otherwise, returns NULL.
 *
 * \param [in] toplevel  The TOPLEVEL structure.
 * \param [in] object    The OBJECT for which to get the containing OBJECT.
 * \return The complex OBJECT which owns \a object, or NULL.
 */
OBJECT *
o_get_parent (TOPLEVEL *toplevel, OBJECT *object)
{
  g_return_val_if_fail ((object != NULL), NULL);

  if (object->parent != NULL) {
    return object->parent;
  }
  return NULL;
}

/* Structure for each entry in a TOPLEVEL's list of registered change
 * notification handlers */
struct change_notify_entry {
  ChangeNotifyFunc pre_change_func;
  ChangeNotifyFunc change_func;
  void *user_data;
};

/*! \brief Add change notification handlers to a TOPLEVEL.
 * \par Function Description
 * Adds a set of change notification handlers to a #TOPLEVEL instance.
 * \a pre_change_func will be called just before an object is
 * modified, and \a change_func will be called just after an object is
 * modified, with the affected object and the given \a user_data.
 *
 * \param toplevel #TOPLEVEL structure to add handlers to.
 * \param pre_change_func Function to be called just before changes.
 * \param change_func Function to be called just after changes.
 * \param user_data User data to be passed to callback functions.
 */
void
o_add_change_notify (TOPLEVEL *toplevel,
                     ChangeNotifyFunc pre_change_func,
                     ChangeNotifyFunc change_func,
                     void *user_data)
{
  struct change_notify_entry *entry = g_new0 (struct change_notify_entry, 1);
  entry->pre_change_func = pre_change_func;
  entry->change_func = change_func;
  entry->user_data = user_data;
  toplevel->change_notify_funcs =
    g_list_prepend (toplevel->change_notify_funcs, entry);
}

/*! \brief Remove change notification handlers from a TOPLEVEL.
 * \par Function Description
 * Removes a set of change notification handlers and their associated
 * \a user_data from \a toplevel.  If no registered set of handlers
 * matches the given \a pre_change_func, \a change_func and \a
 * user_data, does nothing.
 *
 * \see o_add_change_notify()
 *
 * \param toplevel #TOPLEVEL structure to remove handlers from.
 * \param pre_change_func Function called just before changes.
 * \param change_func Function called just after changes.
 * \param user_data User data passed to callback functions.
 */
void
o_remove_change_notify (TOPLEVEL *toplevel,
                        ChangeNotifyFunc pre_change_func,
                        ChangeNotifyFunc change_func,
                        void *user_data)
{
  GList *iter;
  for (iter = toplevel->change_notify_funcs;
       iter != NULL; iter = g_list_next (iter)) {

    struct change_notify_entry *entry =
      (struct change_notify_entry *) iter->data;

    if ((entry != NULL)
        && (entry->pre_change_func == pre_change_func)
        && (entry->change_func == change_func)
        && (entry->user_data == user_data)) {
      g_free (entry);
      iter->data = NULL;
    }
  }
  toplevel->change_notify_funcs =
    g_list_remove_all (toplevel->change_notify_funcs, NULL);
}

/*! \brief Emit an object pre-change notification.
 * \par Function Description
 * Calls each pre-change callback function registered with #TOPLEVEL
 * to notify listeners that \a object is about to be modified.  All
 * libgeda functions that modify #OBJECT structures should call this
 * just before making a change to an #OBJECT.
 *
 * \param toplevel #TOPLEVEL structure to emit notifications from.
 * \param object   #OBJECT structure to emit notifications for.
 */
void
o_emit_pre_change_notify (TOPLEVEL *toplevel, OBJECT *object)
{
  GList *iter;
  for (iter = toplevel->change_notify_funcs;
       iter != NULL; iter = g_list_next (iter)) {

    struct change_notify_entry *entry =
      (struct change_notify_entry *) iter->data;

    if ((entry != NULL) && (entry->pre_change_func != NULL)) {
      entry->pre_change_func (entry->user_data, object);
    }
  }
}

/*! \brief Emit an object change notification.
 * \par Function Description
 * Calls each change callback function registered with #TOPLEVEL to
 * notify listeners that \a object has just been modified.  All
 * libgeda functions that modify #OBJECT structures should call this
 * just after making a change to an #OBJECT.
 *
 * \param toplevel #TOPLEVEL structure to emit notifications from.
 * \param object   #OBJECT structure to emit notifications for.
 */
void
o_emit_change_notify (TOPLEVEL *toplevel, OBJECT *object)
{
  GList *iter;
  for (iter = toplevel->change_notify_funcs;
       iter != NULL; iter = g_list_next (iter)) {

    struct change_notify_entry *entry =
      (struct change_notify_entry *) iter->data;

    if ((entry != NULL) && (entry->change_func != NULL)) {
      entry->change_func (entry->user_data, object);
    }
  }
}

/*! \brief Query visibility of the object.
 *  \par Function Description
 *  Attribute getter for the visible field within the object.
 *
 *  \param toplevel The TOPLEVEL structure
 *  \param object   The OBJECT structure to be queried
 *  \return TRUE when VISIBLE, FALSE otherwise
 */
gboolean
o_is_visible (const TOPLEVEL *toplevel, const OBJECT *object)
{
  g_return_val_if_fail (object != NULL, FALSE);
  return object->visibility == VISIBLE;
}

/*! \brief Get the visibility of an object
 *
 *  \param [in] object The OBJECT structure to be queried
 *  \return VISIBLE or INVISIBLE
 */
gint
geda_object_get_visible (const GedaObject *object)
{
  g_return_val_if_fail (object != NULL, VISIBLE);

  return object->visibility;
}

/*! \brief Set visibility of the object.
 *  \par Function Description
 *  Set value of visibility field within the object.
 *  If resulting visibility value is changed,
 *  invalidate the bounds of the object and parent objects.
 *
 *  \param toplevel The #TOPLEVEL structure
 *  \param object   The #OBJECT structure to be modified
 */
void
o_set_visibility (TOPLEVEL *toplevel, OBJECT *object, int visibility)
{
  g_return_if_fail (object != NULL);
  if (object->visibility != visibility) {
    object->visibility = visibility;
    o_bounds_invalidate (toplevel, object);
  }
}

/*! \brief Return the bounds of the given object.
 *  \par Given an object, calculate the bounds coordinates.
 *  \param [in] toplevel The toplevel structure.
 *  \param [in] o_current The object to look the bounds for.
 *  \param [out] rleft   pointer to the left coordinate of the object.
 *  \param [out] rtop    pointer to the top coordinate of the object.
 *  \param [out] rright  pointer to the right coordinate of the object.
 *  \param [out] rbottom pointer to the bottom coordinate of the object.
 *  \return If any bounds were found for the object
 *  \retval 0 No bound was found
 *  \retval 1 Bound was found
 */
gboolean
geda_object_calculate_visible_bounds (TOPLEVEL *toplevel,
                                      OBJECT *o_current,
                                      gint *rleft,
                                      gint *rtop,
                                      gint *rright,
                                      gint *rbottom)
{
  if (o_current == NULL) {
    return 0;
  }

  /* only do bounding boxes for visible or doing show_hidden_text*/
  /* you might lose some attrs though */
  if (o_current->type == OBJ_TEXT &&
      ! (o_is_visible (toplevel, o_current) || toplevel->show_hidden_text)) {
    return 0;
  }

  if (o_current->w_bounds_valid_for != toplevel) {
    GedaBounds bounds;

    switch(o_current->type) {

      case(OBJ_LINE):
        if (o_current->line == NULL) {
          return 0;
        }
        geda_line_object_calculate_bounds (toplevel, o_current, &bounds);
        break;

      case(OBJ_NET):
        if (o_current->line == NULL) {
          return 0;
        }
        geda_net_object_calculate_bounds (toplevel, o_current, &bounds);
        break;

      case(OBJ_BUS):
        if (o_current->line == NULL) {
          return 0;
        }
        geda_bus_object_calculate_bounds(toplevel, o_current, &bounds);
        break;

      case(OBJ_BOX):
        if (o_current->box == NULL) {
          return 0;
        }
        geda_box_object_calculate_bounds (toplevel, o_current, &bounds);
        break;

      case(OBJ_PATH):
        g_return_val_if_fail (o_current->path != NULL, 0);
        if (o_current->path->num_sections <= 0) {
          return 0;
        }
        geda_path_object_calculate_bounds (toplevel, o_current, &bounds);
        break;

      case(OBJ_PICTURE):
        if (o_current->picture == NULL) {
          return 0;
        }
        geda_picture_object_calculate_bounds (toplevel, o_current, &bounds);
        break;

      case(OBJ_CIRCLE):
        if (o_current->circle == NULL) {
          return 0;
        }
        geda_circle_object_calculate_bounds (toplevel, o_current, &bounds);
        break;

      case(OBJ_COMPLEX):
      case(OBJ_PLACEHOLDER):
        /* realc routine Add this somewhere */
        /* libhack */
        /* o_recalc(toplevel, o_current->complex);*/

        if (o_current->complex->prim_objs == NULL)
          return 0;

        geda_complex_object_calculate_bounds(toplevel, o_current, &bounds);
        break;

      case(OBJ_PIN):
        if (o_current->line == NULL) {
          return 0;
        }
        geda_pin_object_calculate_bounds (toplevel, o_current, &bounds);
        break;

      case(OBJ_ARC):
        if (o_current->arc == NULL) {
          return 0;
        }
        geda_arc_object_calculate_bounds (toplevel, o_current,
                                          &bounds.min_x,
                                          &bounds.min_y,
                                          &bounds.max_x,
                                          &bounds.max_y);
        break;

      case(OBJ_TEXT):
        if (!geda_text_object_calculate_bounds(toplevel, o_current, &bounds)) {
          return 0;
        }
        break;

      default:
        return 0;
    }

    o_current->bounds = bounds;
    o_current->w_bounds_valid_for = toplevel;
  }

  if (rleft != NULL) {
    *rleft = o_current->bounds.min_x;
  }

  if (rtop != NULL) {
    *rtop = o_current->bounds.min_y;
  }

  if (rright != NULL) {
    *rright = o_current->bounds.max_x;
  }

  if (rbottom != NULL) {
    *rbottom = o_current->bounds.max_y;
  }
  return 1;
}

/*! \private
 *  \brief Initialize an already-allocated object.
 *  \par Function Description
 *  Initializes the members of the OBJECT structure.
 *
 *  \param [in] new_node  A pointer to an allocated OBJECT
 *  \param [in] type      The object type; one of the OBJ_* constants.
 *  \param [in] name      A prefix for the object's session-unique name.
 *  \return A pointer to the initialized object.
 */
static OBJECT*
s_basic_init_object (OBJECT *new_node, int type, char const *name)
{
  /* setup sid */
  new_node->sid = global_sid++;
  new_node->type = type;

  /* Setup the name */
  new_node->name = g_strdup_printf("%s.%d", name, new_node->sid);

  /* Don't associate with a page, initially */
  new_node->page = NULL;

  /* Setup the bounding box */
  geda_bounds_init (&(new_node->bounds));
  new_node->w_bounds_valid_for = NULL;

  /* Setup line/circle structs */
  new_node->line = NULL;
  new_node->path = NULL;
  new_node->circle = NULL;
  new_node->arc = NULL;
  new_node->box = NULL;
  new_node->picture = NULL;
  new_node->text = NULL;
  new_node->complex = NULL;

  new_node->conn_list = NULL;

  new_node->complex_basename = NULL;
  new_node->parent = NULL;

  /* Setup the color */
  new_node->color = DEFAULT_COLOR;
  new_node->dont_redraw = FALSE;
  new_node->selectable = TRUE;
  new_node->selected = FALSE;

  new_node->bus_ripper_direction = 0;

  new_node->line_end = END_NONE;
  new_node->line_type = TYPE_SOLID;
  new_node->line_width = 0;
  new_node->line_space = 0;
  new_node->line_length = 0;
  new_node->fill_width = 0;
  new_node->fill_angle1 = 0;
  new_node->fill_angle2 = 0;
  new_node->fill_pitch1 = 0;
  new_node->fill_pitch2 = 0;

  new_node->attribs = NULL;
  new_node->attached_to = NULL;
  new_node->copied_to = NULL;
  new_node->show_name_value = SHOW_NAME_VALUE;
  new_node->visibility = VISIBLE;

  new_node->pin_type = PIN_TYPE_NET;
  new_node->whichend = -1;

  new_node->weak_refs = NULL;

  return(new_node);
}
