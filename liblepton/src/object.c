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

/*! \file object.c
 *  \brief functions for the basic object type
 *
 *  This file contains the code used to handle LeptonObjects (st_object).
 *  The object is the basic type of all elements stored in schematic
 *  and symbol files.
 *
 *  The object be extended to become concrete objects like a line,
 *  a pin, text, a circle or a picture. These extentions are substructures
 *  in the object struct.
 *  The subobjects are picture (st_picture), path (st_path), arcs (st_arc),
 *  a line (st_line), box (st_box), circle (st_circle), text (st_text) and
 *  a component type (st_component).
 *
 *  Pins, nets and busses are just a kind of a line.
 *
 *  The component object can carry many primary objects. If the
 *  component object is a symbol, then the component symbol contains
 *  all the pins, the text and the graphics.
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

#include "liblepton_priv.h"

/*! this is modified here and in o_list.c */
int global_sid=0;

/* Deprecated variables for Scheme code. */
char _OBJ_LINE = OBJ_LINE;
char _OBJ_PATH = OBJ_PATH;
char _OBJ_BOX = OBJ_BOX;
char _OBJ_PICTURE = OBJ_PICTURE;
char _OBJ_CIRCLE = OBJ_CIRCLE;
char _OBJ_NET = OBJ_NET;
char _OBJ_BUS = OBJ_BUS;
char _OBJ_COMPONENT = OBJ_COMPONENT;
char _OBJ_TEXT = OBJ_TEXT;
char _OBJ_PIN = OBJ_PIN;
char _OBJ_ARC = OBJ_ARC;
char _OBJ_PLACEHOLDER = OBJ_PLACEHOLDER;



/*! \brief Get object's unique id
 *
 *  If this function fails, it returns -1.
 *
 *  \param [in] object The object to obtain the id of.
 *  \return The object id.
 */
int
lepton_object_get_id (LeptonObject *object)
{
  g_return_val_if_fail (object != NULL, -1);
  return object->sid;
}


/*! \brief Set object's id
 *
 *  \param [in] object The object to set the id.
 *  \param [in] id     The new object id.
 */
void
lepton_object_set_id (LeptonObject *object, int id)
{
  g_return_if_fail (object != NULL);
  object->sid = id;
}


/*! \brief Get object's type
 *
 *  If this function fails, it returns -1.
 *
 *  \param [in] object The object to obtain the type of.
 *  \return The object type as int.
 */
int
lepton_object_get_type (const LeptonObject *object)
{
  g_return_val_if_fail (object != NULL, -1);
  return object->type;
}


/*! \brief Set object's type
 *
 *  \param [in] object The object to obtain the type of.
 *  \return The object type as int.
 */
void
lepton_object_set_type (LeptonObject *object, int type)
{
  g_return_if_fail (object != NULL);
  object->type = type;
}


/*! \brief Test if object is an arc object.
 *
 *  \param [in] object The object to test.
 *  \return TRUE if the object is arc, otherwise FALSE.
 */
gboolean
lepton_object_is_arc (const LeptonObject *object)
{
  return (object != NULL &&
          lepton_object_get_type (object) == OBJ_ARC);
}


/*! \brief Test if object is a box object.
 *
 *  \param [in] object The object to test.
 *  \return TRUE, if the object is box, otherwise FALSE.
 */
gboolean
lepton_object_is_box (const LeptonObject *object)
{
  return (object != NULL &&
          lepton_object_get_type (object) == OBJ_BOX);
}


/*! \brief Test if object is a bus object.
 *
 *  \param [in] object The object to test.
 *  \return TRUE, if the object is bus, otherwise FALSE.
 */
gboolean
lepton_object_is_bus (const LeptonObject *object)
{
  return (object != NULL &&
          lepton_object_get_type (object) == OBJ_BUS);
}


/*! \brief Test if object is a circle object.
 *
 *  \param [in] object The object to test.
 *  \return TRUE, if the object is circle, otherwise FALSE.
 */
gboolean
lepton_object_is_circle (const LeptonObject *object)
{
  return (object != NULL &&
          lepton_object_get_type (object) == OBJ_CIRCLE);
}


/*! \brief Test if object is a component object.
 *
 *  \param [in] object The object to test.
 *  \return TRUE, if the object is component, otherwise FALSE.
 */
gboolean
lepton_object_is_component (const LeptonObject *object)
{
  return (object != NULL &&
          lepton_object_get_type (object) == OBJ_COMPONENT);
}


/*! \brief Test if object is a line object.
 *
 *  \param [in] object The object to test.
 *  \return TRUE, if the object is line, otherwise FALSE.
 */
gboolean
lepton_object_is_line (const LeptonObject *object)
{
  return (object != NULL &&
          lepton_object_get_type (object) == OBJ_LINE);
}


/*! \brief Test if object is a net object.
 *
 *  \param [in] object The object to test.
 *  \return TRUE, if the object is net, otherwise FALSE.
 */
gboolean
lepton_object_is_net (const LeptonObject *object)
{
  return (object != NULL &&
          lepton_object_get_type (object) == OBJ_NET);
}


/*! \brief Test if object is a path object.
 *
 *  \param [in] object The object to test.
 *  \return TRUE, if the object is path, otherwise FALSE.
 */
gboolean
lepton_object_is_path (const LeptonObject *object)
{
  return (object != NULL &&
          lepton_object_get_type (object) == OBJ_PATH);
}


/*! \brief Test if object is a picture object.
 *
 *  \param [in] object The object to test.
 *  \return TRUE, if the object is picture, otherwise FALSE.
 */
gboolean
lepton_object_is_picture (const LeptonObject *object)
{
  return (object != NULL &&
          lepton_object_get_type (object) == OBJ_PICTURE);
}


/*! \brief Test if object is a pin object.
 *
 *  \param [in] object The object to test.
 *  \return TRUE, if the object is pin, otherwise FALSE.
 */
gboolean
lepton_object_is_pin (const LeptonObject *object)
{
  return (object != NULL &&
          lepton_object_get_type (object) == OBJ_PIN);
}


/*! \brief Test if object is a text object.
 *
 *  \param [in] object The object to test.
 *  \return TRUE, if the object is text, otherwise FALSE.
 */
gboolean
lepton_object_is_text (const LeptonObject *object)
{
  return (object != NULL &&
          lepton_object_get_type (object) == OBJ_TEXT);
}


/*! \brief Query whether an object is an attribute.
 *
 *  \return  TRUE if \a obj is an attribute, FALSE otherwise.
 */
gboolean
lepton_object_is_attrib (const LeptonObject *obj)
{
  return (lepton_object_is_text (obj) &&
          (lepton_text_object_get_name (obj) != NULL));
}


/*! \brief Get the color index of the object
 *
 *  If this function fails, it returns the default color ID.
 *
 *  \param [in] object the object to obtain the color of
 *  \return the color index of the object
 */
gint
lepton_object_get_color (const LeptonObject *object)
{
  g_return_val_if_fail (object != NULL, default_color_id());
  g_return_val_if_fail (color_id_valid (object->color), default_color_id());

  return object->color;
}

/*! \brief Get the color for drawing the object
 *
 *  If this function fails, it returns the default color ID.
 *
 *  The output of this funtion is dependent on other variables than just the
 *  object color. If the object is locked, it will return the LOCK_COLOR.
 *  This value should not be used for saving or editing the object.
 *
 *  \param [in] object the object to obtain the color of
 *  \return the color index the draw the object
 */
gint
lepton_object_get_drawing_color (const LeptonObject *object)
{
  gint color;

  g_return_val_if_fail (object != NULL, default_color_id());

  color = object->selectable ? lepton_object_get_color (object) : LOCK_COLOR;

  g_return_val_if_fail (color_id_valid (color), default_color_id());

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
lepton_object_get_selectable (const LeptonObject *object)
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
lepton_object_set_selectable (LeptonObject *object,
                              gboolean selectable)
{
  g_return_if_fail (object != NULL);

  object->selectable = selectable;
}


/*! \brief Get the stroke of an object.
 *
 *  \param [in] object    The object.
 *  \return Object's #LeptonStroke pointer.
 */
LeptonStroke*
lepton_object_get_stroke (const LeptonObject *object)
{
  g_return_val_if_fail (object != NULL, NULL);

  return object->stroke;
}


/*! \brief Set the stroke of an object.
 *
 *  \param [in] object The object.
 *  \param [in] stroke The #LeptonStroke pointer.
 */
void
lepton_object_set_stroke (LeptonObject *object,
                          LeptonStroke *stroke)
{
  g_return_if_fail (object != NULL);
  g_return_if_fail (stroke != NULL);

  object->stroke = stroke;
}


/*! \brief Get the line stroke type of an object.
 *
 *  \param [in] object The object.
 *  \return The line stroke type of the object.
 */
LeptonStrokeType
lepton_object_get_stroke_type (const LeptonObject *object)
{
  g_return_val_if_fail (object != NULL, TYPE_SOLID);

  LeptonStroke *stroke = lepton_object_get_stroke (object);

  return lepton_stroke_get_type (stroke);
}

/*! \brief Sets the line stroke type of an object.
 *
 *  \param [in] object The object.
 *  \param [in] type   The new stroke type.
 */
void
lepton_object_set_stroke_type (LeptonObject *object,
                               LeptonStrokeType type)
{
  g_return_if_fail (object != NULL);

  LeptonStroke *stroke = lepton_object_get_stroke (object);

  lepton_stroke_set_type (stroke, type);
}


/*! \brief Get the line stroke cap type of an object.
 *
 *  \param [in] object   The object.
 *  \return The line cap type of the object.
 */
LeptonStrokeCapType
lepton_object_get_stroke_cap_type (const LeptonObject *object)
{
  g_return_val_if_fail (object != NULL, END_NONE);

  LeptonStroke *stroke = lepton_object_get_stroke (object);

  return lepton_stroke_get_cap_type (stroke);
}

/*! \brief Sets the line cap type of an object.
 *
 *  \param [in] object   The object.
 *  \param [in] cap_type The new line cap type.
 */
void
lepton_object_set_stroke_cap_type (LeptonObject *object,
                                   LeptonStrokeCapType cap_type)
{
  g_return_if_fail (object != NULL);

  LeptonStroke *stroke = lepton_object_get_stroke (object);

  lepton_stroke_set_cap_type (stroke, cap_type);
}


/*! \brief Get the line stroke width of an object.
 *
 *  \param [in] object The object.
 *  \return The line stroke width of the object.
 */
int
lepton_object_get_stroke_width (const LeptonObject *object)
{
  g_return_val_if_fail (object != NULL, 0);

  LeptonStroke *stroke = lepton_object_get_stroke (object);

  return lepton_stroke_get_width (stroke);
}

/*! \brief Sets the line stroke width of an object.
 *
 *  \param [in] object The object.
 *  \param [in] width  The new line stroke width.
 */
void
lepton_object_set_stroke_width (LeptonObject *object,
                                int width)
{
  g_return_if_fail (object != NULL);

  LeptonStroke *stroke = lepton_object_get_stroke (object);

  lepton_stroke_set_width (stroke, width);
}


/*! \brief Get the line stroke dash length of an object.
 *
 *  \param [in] object The object.
 *  \return The line stroke dash length of the object.
 */
int
lepton_object_get_stroke_dash_length (const LeptonObject *object)
{
  g_return_val_if_fail (object != NULL, 0);

  LeptonStroke *stroke = lepton_object_get_stroke (object);

  return lepton_stroke_get_dash_length (stroke);
}

/*! \brief Sets the line stroke dash length of an object.
 *
 *  \param [in] object The object.
 *  \param [in] length The new line stroke dash length.
 */
void
lepton_object_set_stroke_dash_length (LeptonObject *object,
                                      int length)
{
  g_return_if_fail (object != NULL);

  LeptonStroke *stroke = lepton_object_get_stroke (object);

  lepton_stroke_set_dash_length (stroke, length);
}


/*! \brief Get the line stroke dash space of an object.
 *
 *  \param [in] object The object.
 *  \return The line stroke dash space of the object.
 */
int
lepton_object_get_stroke_space_length (const LeptonObject *object)
{
  g_return_val_if_fail (object != NULL, 0);

  LeptonStroke *stroke = lepton_object_get_stroke (object);

  return lepton_stroke_get_space_length (stroke);
}

/*! \brief Sets the line stroke dash space of an object.
 *
 *  \param [in] object The object.
 *  \param [in] space  The new line stroke dash space.
 */
void
lepton_object_set_stroke_space_length (LeptonObject *object,
                                       int space)
{
  g_return_if_fail (object != NULL);

  LeptonStroke *stroke = lepton_object_get_stroke (object);

  lepton_stroke_set_space_length (stroke, space);
}


/*! \brief Get the fill of an object.
 *
 *  \param [in] object The object.
 *  \return Object's #LeptonFill pointer.
 */
LeptonFill*
lepton_object_get_fill (const LeptonObject *object)
{
  g_return_val_if_fail (object != NULL, NULL);

  return object->fill;
}


/*! \brief Set the fill of an object.
 *
 *  \param [in] object The object.
 *  \param [in] fill The #LeptonFill pointer.
 */
void
lepton_object_set_fill (LeptonObject *object,
                        LeptonFill *fill)
{
  g_return_if_fail (object != NULL);
  g_return_if_fail (fill != NULL);

  object->fill = fill;
}


/*! \brief Get the fill type of an object.
 *
 *  \param [in] object The object.
 *  \return The fill type of the object.
 */
LeptonFillType
lepton_object_get_fill_type (const LeptonObject *object)
{
  g_return_val_if_fail (object != NULL, FILLING_HOLLOW);

  LeptonFill *fill = lepton_object_get_fill (object);

  return lepton_fill_get_type (fill);
}

/*! \brief Get the fill width of an object.
 *
 * \note Fill width is meaningful only for some types of fill.
 *
 *  \param [in] object The object.
 *  \return The fill width of the object.
 */
int
lepton_object_get_fill_width (const LeptonObject *object)
{
  g_return_val_if_fail (object != NULL, 0);

  LeptonFill *fill = lepton_object_get_fill (object);

  return lepton_fill_get_width (fill);
}

/*! \brief Get the first pitch of the fill of an object.
 *
 *  \param [in] object The object.
 *  \return The first pitch of the fill of the object.
 */
int
lepton_object_get_fill_pitch1 (const LeptonObject *object)
{
  g_return_val_if_fail (object != NULL, 0);

  LeptonFill *fill = lepton_object_get_fill (object);

  return lepton_fill_get_pitch1 (fill);
}

/*! \brief Get the first pitch angle of the fill of an object.
 *
 *  \param [in] object The object.
 *  \return The first pitch angle of the fill of the object.
 */
int
lepton_object_get_fill_angle1 (const LeptonObject *object)
{
  g_return_val_if_fail (object != NULL, 0);

  LeptonFill *fill = lepton_object_get_fill (object);

  return lepton_fill_get_angle1 (fill);
}

/*! \brief Get the second pitch of the fill of an object.
 *
 *  \param [in] object The object.
 *  \return The second pitch of the fill of the object.
 */
int
lepton_object_get_fill_pitch2 (const LeptonObject *object)
{
  g_return_val_if_fail (object != NULL, 0);

  LeptonFill *fill = lepton_object_get_fill (object);

  return lepton_fill_get_pitch2 (fill);
}

/*! \brief Get the second pitch angle of the fill of an object.
 *
 *  \param [in] object The object.
 *  \return The second pitch angle of the fill of the object.
 */
int
lepton_object_get_fill_angle2 (const LeptonObject *object)
{
  g_return_val_if_fail (object != NULL, 0);

  LeptonFill *fill = lepton_object_get_fill (object);

  return lepton_fill_get_angle2 (fill);
}


void
lepton_object_set_fill_type (LeptonObject *object,
                             LeptonFillType fill_type)
{
  g_return_if_fail (object != NULL);

  LeptonFill *fill = lepton_object_get_fill (object);

  g_return_if_fail (fill != NULL);

  lepton_fill_set_type (fill, fill_type);
}

void
lepton_object_set_fill_width (LeptonObject *object,
                              int width)
{
  g_return_if_fail (object != NULL);

  LeptonFill *fill = lepton_object_get_fill (object);

  g_return_if_fail (fill != NULL);

  lepton_fill_set_width (fill, width);
}

void
lepton_object_set_fill_pitch1 (LeptonObject *object,
                               int pitch)
{
  g_return_if_fail (object != NULL);

  LeptonFill *fill = lepton_object_get_fill (object);

  g_return_if_fail (fill != NULL);

  lepton_fill_set_pitch1 (fill, pitch);
}

void
lepton_object_set_fill_angle1 (LeptonObject *object,
                               int angle)
{
  g_return_if_fail (object != NULL);

  LeptonFill *fill = lepton_object_get_fill (object);

  g_return_if_fail (fill != NULL);

  lepton_fill_set_angle1 (fill, angle);
}

void
lepton_object_set_fill_pitch2 (LeptonObject *object,
                               int pitch)
{
  g_return_if_fail (object != NULL);

  LeptonFill *fill = lepton_object_get_fill (object);

  g_return_if_fail (fill != NULL);

  lepton_fill_set_pitch2 (fill, pitch);
}

void
lepton_object_set_fill_angle2 (LeptonObject *object,
                               int angle)
{
  g_return_if_fail (object != NULL);

  LeptonFill *fill = lepton_object_get_fill (object);

  g_return_if_fail (fill != NULL);

  lepton_fill_set_angle2 (fill, angle);
}


/*! \brief Get object's 'whichend'.
 *
 *  \par Function Description
 *  Obtains 'whichend' field of the #LeptonObject structure.  For
 *  pins it defines which end of the pin is connectible.
 *  \param [in] object The object to obtain the 'whichend' of.
 *  \return The object 'whichend'.
 */
int
lepton_object_get_whichend (LeptonObject *object)
{
  g_return_val_if_fail (object != NULL, -1);
  return object->whichend;
}


/*! \brief Get object's 'attached_to' field.
 *
 *  \par Function Description
 *  Obtains the 'attached_to' field of the #LeptonObject
 *  structure.  It defines to what another object the object in
 *  question is attached to.  \a object is usually an attribute
 *  while the other object may be component, net, bus, etc.
 *  \param [in] object The object to obtain the 'attached_to' of.
 *  \return The value of the 'attached_to' field of the object.
 */
LeptonObject*
lepton_object_get_attached_to (const LeptonObject *object)
{
  g_return_val_if_fail (object != NULL, NULL);
  return object->attached_to;
}

/*! \brief Set object's 'attached_to' field.
 *
 *  \par Function Description
 *  Set's the 'attached_to' field of the #LeptonObject structure.
 *  It defines another object to what the object should be
 *  attached to.  \a object is usually an attribute while the
 *  other object may be component, net, bus, etc.
 *  \param [in] object The object to attach.
 *  \param [in] attached_to The other object.
 */
void
lepton_object_set_attached_to (LeptonObject *object,
                               LeptonObject *attached_to)
{
  g_return_if_fail (object != NULL);
  object->attached_to = attached_to;
}


/*! \brief Get the list of object's attributes.
 *
 *  \par Function Description
 *  Obtains the list of object's attributes as a GList value.
 *
 *  \param [in] object The object to obtain the attribs of.
 *  \return The GList of attribs of the object.
 */
GList*
lepton_object_get_attribs (const LeptonObject *object)
{
  g_return_val_if_fail (object != NULL, NULL);
  return object->attribs;
}


/*! \brief Set the list of object's attributes.
 *
 *  \par Function Description
 *  Sets the list of object's attributes to the specified GList value.
 *
 *  \param [in] object The object to set the attribs for.
 *  \param [in] attribs The new attrib GList value.
 */
void
lepton_object_set_attribs (LeptonObject *object,
                           GList *attribs)
{
  g_return_if_fail (object != NULL);
  object->attribs = attribs;
}


/*! \brief Make and return a copy of an object.
 *
 *  \par Function Description
 *  Creates a copy of a LeptonObject with the same id storing a
 *  reference in the copied object to the new object in order to
 *  retain associations when copying attributes.
 *
 *  \param [in] object The #LeptonObject to copy.
 *  \return #LeptonObject pointer to the newly created object.
 */
LeptonObject*
lepton_object_copy (LeptonObject *object)
{
  LeptonObject *new_object;

  g_return_val_if_fail (object != NULL, NULL);

  switch (lepton_object_get_type (object)) {

    case(OBJ_LINE):
      new_object = lepton_line_object_copy (object);
      break;

    case(OBJ_NET):
      new_object = lepton_net_object_copy (object);
      break;

    case(OBJ_BUS):
      new_object = lepton_bus_object_copy (object);
      break;

    case(OBJ_BOX):
      new_object = lepton_box_object_copy (object);
      break;

    case(OBJ_PICTURE):
      new_object = lepton_picture_object_copy (object);
      break;

    case(OBJ_CIRCLE):
      new_object = lepton_circle_object_copy (object);
      break;

    case(OBJ_COMPONENT):
      new_object = o_component_copy (object);
      break;

    case(OBJ_TEXT):
      new_object = lepton_text_object_copy (object);
      break;

    case(OBJ_PATH):
      new_object = lepton_path_object_copy (object);
      break;

    case(OBJ_PIN):
      new_object = lepton_pin_object_copy (object);
      break;

    case(OBJ_ARC):
      new_object = lepton_arc_object_copy (object);
      break;

    default:
      g_critical ("o_list_copy_to: object %1$p has bad type '%2$c'\n",
                  object, lepton_object_get_type (object));
      return NULL;
  }

  /* Store a reference in the copied object to where it was copied.
   * Used to retain associations when copying attributes */
  object->copied_to = new_object;

  /* make sure sid is the same! */
  if (object) {
    lepton_object_set_id (new_object, lepton_object_get_id (object));
  }

  return new_object;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
lepton_object_delete (LeptonObject *o_current)
{
  GList *primitives = NULL;

  if (o_current != NULL) {
    /* If currently attached to a page, remove it from the page */
    if (o_current->page != NULL) {
      s_page_remove (o_current->page, o_current);
    }

    s_conn_remove_object_connections (o_current);

    if (o_current->attached_to != NULL) {
      /* do the actual remove */
      o_attrib_remove (&o_current->attached_to->attribs, o_current);
    }

    /* printf("sdeleting line\n"); */
    lepton_line_free (o_current->line);
    o_current->line = NULL;

    lepton_path_free (o_current->path);
    o_current->path = NULL;

    /* printf("sdeleting circle\n");*/
    lepton_circle_free (o_current->circle);
    o_current->circle = NULL;

    /* printf("sdeleting arc\n");*/
    lepton_arc_free (o_current->arc);
    o_current->arc = NULL;

    /* printf("sdeleting box\n");*/
    lepton_box_free (o_current->box);
    o_current->box = NULL;

    lepton_picture_free (o_current->picture);
    o_current->picture = NULL;

    lepton_text_free (o_current->text);
    o_current->text = NULL;

    /* printf("sdeleting name\n");*/
    g_free(o_current->name);
    o_current->name = NULL;

    lepton_stroke_free (o_current->stroke);
    o_current->stroke = NULL;

    lepton_fill_free (o_current->fill);
    o_current->fill = NULL;

    /* printf("sdeleting component_basename\n");*/
    g_free(o_current->component_basename);
    o_current->component_basename = NULL;

    if (o_current->component) {

      primitives = lepton_component_object_get_contents (o_current);
      if (primitives != NULL)
      {
        /* printf("sdeleting component's primitive objects\n");*/
        lepton_object_list_delete (primitives);
        lepton_component_object_set_contents (o_current, NULL);
      }

      g_free(o_current->component);
      o_current->component = NULL;
    }

    o_attrib_detach_all (o_current);

    o_current->weak_refs = s_weakref_notify (o_current, o_current->weak_refs);

    g_free(o_current); /* assuming it is not null */

    o_current=NULL;    /* misc clean up */
  }
}

/*! \brief Add a weak reference watcher to an LeptonObject.
 * \par Function Description
 * Adds the weak reference callback \a notify_func to \a object.  When
 * \a object is destroyed, \a notify_func will be called with two
 * arguments: the \a object, and the \a user_data.
 *
 * \sa lepton_object_weak_unref
 *
 * \param [in,out] object     Object to weak-reference.
 * \param [in] notify_func    Weak reference notify function.
 * \param [in] user_data      Data to be passed to \a notify_func.
 */
void
lepton_object_weak_ref (LeptonObject *object,
                        void (*notify_func)(void *, void *),
                        void *user_data)
{
  g_return_if_fail (object != NULL);
  object->weak_refs = s_weakref_add (object->weak_refs, notify_func, user_data);
}

/*! \brief Remove a weak reference watcher from an LeptonObject.
 * \par Function Description
 * Removes the weak reference callback \a notify_func from \a object.
 *
 * \sa lepton_object_weak_ref()
 *
 * \param [in,out] object     Object to weak-reference.
 * \param [in] notify_func    Notify function to search for.
 * \param [in] user_data      Data to to search for.
 */
void
lepton_object_weak_unref (LeptonObject *object,
                          void (*notify_func)(void *, void *),
                          void *user_data)
{
  g_return_if_fail (object != NULL);
  object->weak_refs = s_weakref_remove (object->weak_refs,
                                        notify_func, user_data);
}

/*! \brief Add a weak pointer to an LeptonObject.
 * \par Function Description
 * Adds the weak pointer at \a weak_pointer_loc to \a object. The
 * value of \a weak_pointer_loc will be set to NULL when \a object is
 * destroyed.
 *
 * \sa lepton_object_remove_weak_ptr
 *
 * \param [in,out] object        Object to weak-reference.
 * \param [in] weak_pointer_loc  Memory address of a pointer.
 */
void
lepton_object_add_weak_ptr (LeptonObject *object,
                            void *weak_pointer_loc)
{
  g_return_if_fail (object != NULL);
  object->weak_refs = s_weakref_add_ptr (object->weak_refs,
                                         (void**) weak_pointer_loc);
}

/*! \brief Remove a weak pointer from an LeptonObject.
 * \par Function Description
 * Removes the weak pointer at \a weak_pointer_loc from \a object.
 *
 * \sa lepton_object_add_weak_ptr()
 *
 * \param [in,out] object        Object to weak-reference.
 * \param [in] weak_pointer_loc  Memory address of a pointer.
 */
void
lepton_object_remove_weak_ptr (LeptonObject *object,
                               void *weak_pointer_loc)
{
  g_return_if_fail (object != NULL);
  object->weak_refs = s_weakref_remove_ptr (object->weak_refs,
                                            (void**) weak_pointer_loc);
}

/*! \brief Set an #LeptonObject's line options.
 *  \par Function Description
 *  This function allows a line's end, type, width, length and
 *  space to be set.  See #LeptonStrokeCapType and
 *  #LeptonStrokeType for information on valid object end and
 *  type values.
 *
 *  \param [in,out] o_current  LeptonObject to set line options on.
 *  \param [in]     end        An LeptonStrokeCapType.
 *  \param [in]     type       An LeptonStrokeType.
 *  \param [in]     width      Line width.
 *  \param [in]     length     Line length.
 *  \param [in]     space      Spacing between dashes/dots. Cannot be negative.
 *
 *  \todo Make space an unsigned int and check for a max value instead.
 *        If a max value is not required, then it would simplify the code.
 */
void
lepton_object_set_line_options (LeptonObject *o_current,
                                LeptonStrokeCapType end,
                                LeptonStrokeType type,
                                int width,
                                int length,
                                int space)
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

  lepton_object_emit_pre_change_notify (o_current);

  lepton_object_set_stroke_width (o_current, width);
  lepton_object_set_stroke_cap_type (o_current, end);
  lepton_object_set_stroke_type (o_current, type);
  lepton_object_set_stroke_dash_length (o_current, length);
  lepton_object_set_stroke_space_length (o_current, space);

  lepton_object_emit_change_notify (o_current);

}

/*! \brief get #LeptonObject's line properties.
 *  \par Function Description
 *  This function get's the #LeptonObject's line options.
 *  See #LeptonStrokeCapType and #LeptonStrokeType for
 *  information on valid object end and type values.
 *
 *  \param [in]   object    LeptonObject to read the properties
 *  \param [out]  end       An LeptonStrokeCapType.
 *  \param [out]  type      An LeptonStrokeType.
 *  \param [out]  width     Line width.
 *  \param [out]  length    Line length.
 *  \param [out]  space     Spacing between dashes/dots.
 *  \return TRUE on succes, FALSE otherwise
 *
 */
gboolean
lepton_object_get_line_options (LeptonObject *object,
                                LeptonStrokeCapType *end,
                                LeptonStrokeType *type,
                                int *width,
                                int *length,
                                int *space)
{
  if (!lepton_object_is_line (object)
      && !lepton_object_is_arc (object)
      && !lepton_object_is_box (object)
      && !lepton_object_is_circle (object)
      && !lepton_object_is_path (object))
    return FALSE;

  *end = lepton_object_get_stroke_cap_type (object);
  *type = lepton_object_get_stroke_type (object);
  *width = lepton_object_get_stroke_width (object);
  *length = lepton_object_get_stroke_dash_length (object);
  *space = lepton_object_get_stroke_space_length (object);

  return TRUE;
}

/*! \brief Set #LeptonObject's fill options.
 *  \par Function Description
 *  This function allows an #LeptonObject's fill options to be configured.
 *  See #LeptonFillType for information on valid fill types.
 *
 *  \param [in,out]  o_current  LeptonObject to be updated.
 *  \param [in]      type       LeptonFillType type.
 *  \param [in]      width      fill width.
 *  \param [in]      pitch1     cross hatch line distance
 *  \param [in]      angle1     cross hatch angle
 *  \param [in]      pitch2     cross hatch line distance
 *  \param [in]      angle2     cross hatch angle
 *
 */
void
lepton_object_set_fill_options (LeptonObject *o_current,
                                LeptonFillType type,
                                int width,
                                int pitch1,
                                int angle1,
                                int pitch2,
                                int angle2)
{
  if(o_current == NULL) {
    return;
  }

  /* do some error checking / correcting */
  if (lepton_fill_type_draw_first_hatch (type))
  {
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

  if (lepton_fill_type_draw_second_hatch (type))
  {
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

  lepton_object_emit_pre_change_notify (o_current);

  lepton_object_set_fill_type (o_current, type);
  lepton_object_set_fill_width (o_current, width);

  lepton_object_set_fill_pitch1 (o_current, pitch1);
  lepton_object_set_fill_angle1 (o_current, angle1);

  lepton_object_set_fill_pitch2 (o_current, pitch2);
  lepton_object_set_fill_angle2 (o_current, angle2);

  lepton_object_emit_change_notify (o_current);
}

/*! \brief get #LeptonObject's fill properties.
 *  \par Function Description
 *  This function get's the #LeptonObject's fill options.
 *  See #LeptonFillType for information on valid fill types.
 *
 *  \param [in]   object    LeptonObject to read the properties
 *  \param [out]  type      LeptonFillType type
 *  \param [out]  width     fill width.
 *  \param [out]  pitch1    cross hatch line distance
 *  \param [out]  angle1    cross hatch angle
 *  \param [out]  pitch2    cross hatch line distance
 *  \param [out]  angle2    cross hatch angle
 *  \return TRUE on succes, FALSE otherwise
 *
 */
gboolean
lepton_object_get_fill_options (LeptonObject *object,
                                LeptonFillType *type,
                                int *width,
                                int *pitch1,
                                int *angle1,
                                int *pitch2,
                                int *angle2)
{
  if (!lepton_object_is_box (object)
      && !lepton_object_is_circle (object)
      && !lepton_object_is_path (object))
    return FALSE;

  *type = lepton_object_get_fill_type (object);
  *width = lepton_object_get_fill_width (object);
  *pitch1 = lepton_object_get_fill_pitch1 (object);
  *angle1 = lepton_object_get_fill_angle1 (object);
  *pitch2 = lepton_object_get_fill_pitch2 (object);
  *angle2 = lepton_object_get_fill_angle2 (object);

  return TRUE;
}

/*! \brief get the base position of an object
 *  \par Function Description
 *  This function gets the position of an object in world coordinates.
 *
 *  \param [out] x       pointer to the x-position
 *  \param [out] y       pointer to the y-position
 *  \param [in] object   The object to get the position.
 *  \return TRUE if successfully determined the position, FALSE otherwise
 */
gboolean
lepton_object_get_position (const LeptonObject *object,
                            gint *x,
                            gint *y)
{
  gboolean (*func) (const LeptonObject*, int*, int*) = NULL;

  g_return_val_if_fail (object != NULL, FALSE);

  switch (lepton_object_get_type (object)) {
    case OBJ_LINE:      func = lepton_line_object_get_position;      break;
    case OBJ_NET:       func = lepton_net_object_get_position;       break;
    case OBJ_BUS:       func = lepton_bus_object_get_position;       break;
    case OBJ_BOX:       func = lepton_box_object_get_position;       break;
    case OBJ_PICTURE:   func = lepton_picture_object_get_position;   break;
    case OBJ_CIRCLE:    func = lepton_circle_object_get_position;    break;
    case OBJ_TEXT:      func = lepton_text_object_get_position;      break;
    case OBJ_PATH:      func = lepton_path_object_get_position;      break;
    case OBJ_PIN:       func = lepton_pin_object_get_position;       break;
    case OBJ_ARC:       func = lepton_arc_object_get_position;       break;
    case OBJ_COMPONENT: func = lepton_component_object_get_position; break;
    default:
      g_critical ("lepton_object_get_position: object %1$p has bad type '%2$c'\n",
                  object, lepton_object_get_type (object));
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
lepton_object_translate (LeptonObject *object,
                         gint dx,
                         gint dy)
{
  void (*func) (LeptonObject*, int, int) = NULL;

  switch (lepton_object_get_type (object)) {
    case OBJ_LINE:      func = lepton_line_object_translate;      break;
    case OBJ_NET:       func = lepton_net_object_translate;       break;
    case OBJ_BUS:       func = lepton_bus_object_translate;       break;
    case OBJ_BOX:       func = lepton_box_object_translate;       break;
    case OBJ_PICTURE:   func = lepton_picture_object_translate;   break;
    case OBJ_CIRCLE:    func = lepton_circle_object_translate;    break;
    case OBJ_TEXT:      func = lepton_text_object_translate;      break;
    case OBJ_PATH:      func = lepton_path_object_translate;      break;
    case OBJ_PIN:       func = lepton_pin_object_translate;       break;
    case OBJ_ARC:       func = lepton_arc_object_translate;       break;
    case OBJ_COMPONENT: func = lepton_component_object_translate; break;
    default:
      g_critical ("lepton_object_translate: object %1$p has bad type '%2$c'\n",
                  object, lepton_object_get_type (object));
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
 *  \param [in] world_centerx  X coordinate of rotation center (world coords)
 *  \param [in] world_centery  Y coordinate of rotation center (world coords)
 *  \param [in] angle          Angle of rotation (degrees)
 *  \param [in] object         The object to rotate.
 */
void
lepton_object_rotate (int world_centerx,
                      int world_centery,
                      int angle,
                      LeptonObject *object)
{
  void (*func) (int, int, int, LeptonObject*) = NULL;

  switch (lepton_object_get_type (object)) {
    case OBJ_LINE:      func = lepton_line_object_rotate;      break;
    case OBJ_NET:       func = lepton_net_object_rotate;       break;
    case OBJ_BUS:       func = lepton_bus_object_rotate;       break;
    case OBJ_BOX:       func = lepton_box_object_rotate;       break;
    case OBJ_PICTURE:   func = lepton_picture_object_rotate;   break;
    case OBJ_CIRCLE:    func = lepton_circle_object_rotate;    break;
    case OBJ_TEXT:      func = lepton_text_object_rotate;      break;
    case OBJ_PATH:      func = lepton_path_object_rotate;      break;
    case OBJ_PIN:       func = lepton_pin_object_rotate;       break;
    case OBJ_ARC:       func = lepton_arc_object_rotate;       break;
    case OBJ_COMPONENT: func = lepton_component_object_rotate; break;
    default:
      g_critical ("lepton_object_rotate: object %1$p has bad type '%2$c'\n",
                  object, lepton_object_get_type (object));
  }

  if (func != NULL) {
    (*func) (world_centerx, world_centery, angle, object);
  }
}


/*! \brief Mirrors an object in world coordinates
 *  \par Function Description
 *  This function mirrors an object about the point
 *  (<B>world_centerx</B>,<B>world_centery</B>) in world units.
 *
 *  \param [in]     world_centerx  Origin x coordinate in WORLD units.
 *  \param [in]     world_centery  Origin y coordinate in WORLD units.
 *  \param [in,out] object         The LeptonObject to mirror.
 */
void
lepton_object_mirror (int world_centerx,
                      int world_centery,
                      LeptonObject *object)
{
  void (*func) (int, int, LeptonObject*) = NULL;

  switch (lepton_object_get_type (object)) {
    case OBJ_LINE:      func = lepton_line_object_mirror;      break;
    case OBJ_NET:       func = lepton_net_object_mirror;       break;
    case OBJ_BUS:       func = lepton_bus_object_mirror;       break;
    case OBJ_BOX:       func = lepton_box_object_mirror;       break;
    case OBJ_PICTURE:   func = lepton_picture_object_mirror;   break;
    case OBJ_CIRCLE:    func = lepton_circle_object_mirror;    break;
    case OBJ_TEXT:      func = lepton_text_object_mirror;      break;
    case OBJ_PATH:      func = lepton_path_object_mirror;      break;
    case OBJ_PIN:       func = lepton_pin_object_mirror;       break;
    case OBJ_ARC:       func = lepton_arc_object_mirror;       break;
    case OBJ_COMPONENT: func = lepton_component_object_mirror; break;
    default:
      g_critical ("lepton_object_mirror: object %1$p has bad type '%2$c'\n",
                  object, lepton_object_get_type (object));
  }

  if (func != NULL) {
    (*func) (world_centerx, world_centery, object);
  }
}


/*! \brief Calculates the distance between the given point and the closest
 * point on the given object.
 *
 *  \param [in] object         The given object.
 *  \param [in] x              The x coordinate of the given point.
 *  \param [in] y              The y coordinate of the given point.
 *  \param [in] include_hidden Take hidden text into account.
 *  \return The shortest distance from the object to the point. If the
 *  distance cannot be calculated, this function returns a really large
 *  number (G_MAXDOUBLE).  If an error occurs, this function returns
 *  G_MAXDOUBLE.
 */
double
lepton_object_shortest_distance (LeptonObject *object,
                                 int x,
                                 int y,
                                 gboolean include_hidden)
{
  return lepton_object_shortest_distance_full (object, x, y, FALSE, include_hidden);
}

/*! \brief Calculates the distance between the given point and the closest
 * point on the given object. Allows forcing objects to solid.
 *
 *  \param [in] object         The given object.
 *  \param [in] x              The x coordinate of the given point.
 *  \param [in] y              The y coordinate of the given point.
 *  \param [in] force_solid    If true, force treating the object as solid.
 *  \param [in] include_hidden Take hidden text into account.
 *  \return The shortest distance from the object to the point. If the
 *  distance cannot be calculated, this function returns a really large
 *  number (G_MAXDOUBLE).  If an error occurs, this function returns
 *  G_MAXDOUBLE.
 */
double
lepton_object_shortest_distance_full (LeptonObject *object,
                                      int x,
                                      int y,
                                      int force_solid,
                                      gboolean include_hidden)
{
  double shortest_distance = G_MAXDOUBLE;
  double (*func) (LeptonObject *, int, int, int, gboolean) = NULL;

  g_return_val_if_fail (object != NULL, G_MAXDOUBLE);

  switch(lepton_object_get_type (object)) {
    case OBJ_BUS:
    case OBJ_NET:
    case OBJ_PIN:
    case OBJ_LINE:      func = lepton_line_object_shortest_distance;      break;
    case OBJ_BOX:       func = lepton_box_object_shortest_distance;       break;
    case OBJ_PICTURE:   func = lepton_picture_object_shortest_distance;   break;
    case OBJ_CIRCLE:    func = lepton_circle_object_shortest_distance;    break;
    case OBJ_COMPONENT: func = lepton_component_object_shortest_distance; break;
    case OBJ_TEXT:      func = lepton_text_object_shortest_distance;      break;
    case OBJ_PATH:      func = lepton_path_object_shortest_distance;      break;
    case OBJ_ARC:       func = lepton_arc_object_shortest_distance;       break;
    default:
      g_critical ("lepton_object_shortest_distance: object %1$p has bad type '%2$c'\n",
                  object, lepton_object_get_type (object));
  }

  if (func != NULL) {
    shortest_distance = (*func) (object, x, y, force_solid, include_hidden);
  }

  return shortest_distance;
}


/*! \brief Change the color of an object
 *
 *  \param [in] object    The LeptonObject to change color.
 *  \param [in] color     The new color.
 */
void
lepton_object_set_color (LeptonObject *object,
                         int color)
{
  GList *primitives = NULL;

  g_return_if_fail (object != NULL);

  object->color = color;

  if (lepton_object_is_component (object) && object->component != NULL)
  {
    primitives = lepton_component_object_get_contents (object);
    lepton_object_list_set_color (primitives, color);
  }
}


/*! \brief Get an object's parent LeptonPage.
 *
 * \par Function Description
 * Returns the LeptonPage structure which owns \a object. If \a
 * object is not currently associated with a LeptonPage, returns
 * NULL. If \a object is part of a compound object, recurses
 * upward.
 *
 * \param [in] object The LeptonObject for which to retrieve the
 *                    parent LeptonPage.
 * \return The LeptonPage which owns \a object or NULL.
 *
 * \sa s_page_append_object() s_page_append() s_page_remove()
 */
LeptonPage *
lepton_object_get_page (LeptonObject *object)
{
  if (object->parent != NULL) {
    return lepton_object_get_page (object->parent);
  }
  return object->page;
}


/*! \brief Flag an object's page as having been changed.
 *
 * \par Function Description
 * Sets the page of \a object as having been changed by setting
 * its CHANGED flag to TRUE.  This is primarily used in cases when
 * there is no another way to do it yet, e.g. in Scheme core
 * modules.
 *
 * \param [in] object The #LeptonObject which page should be changed.
 */
void
lepton_object_page_set_changed (LeptonObject *object)
{
  LeptonPage *page = lepton_object_get_page (object);
  if (page != NULL) page->CHANGED = TRUE;
}


/*! \brief Get an object's containing component object.
 *
 * \par Function Description
 * If \a object is part of a component #LeptonObject, returns that
 * #LeptonObject. Otherwise, returns NULL.
 *
 * \param [in] object    The LeptonObject for which to get the containing LeptonObject.
 * \return The component LeptonObject which owns \a object, or NULL.
 */
LeptonObject *
lepton_object_get_parent (const LeptonObject *object)
{
  g_return_val_if_fail ((object != NULL), NULL);

  return object->parent;
}

/*! \brief Set a parent object for an object.
 *
 * \param [in] object  The object.
 * \param [in] object  The parent object.
 */
void
lepton_object_set_parent (LeptonObject *object,
                          LeptonObject *parent)
{
  g_return_if_fail (object != NULL);

  object->parent = parent;
}


/* Structure for each entry in a LeptonToplevel's list of
 * registered change notification handlers */
struct change_notify_entry {
  ChangeNotifyFunc pre_change_func;
  ChangeNotifyFunc change_func;
  void *user_data;
};

/*! \brief Add change notification handlers to a LeptonToplevel.
 * \par Function Description
 * Adds a set of change notification handlers to a #LeptonToplevel
 * instance.
 *
 * \a pre_change_func will be called just before an object is
 * modified, and \a change_func will be called just after an object is
 * modified, with the affected object and the given \a user_data.
 *
 * \param toplevel #LeptonToplevel structure to add handlers to.
 * \param pre_change_func Function to be called just before changes.
 * \param change_func Function to be called just after changes.
 * \param user_data User data to be passed to callback functions.
 */
void
lepton_object_add_change_notify (LeptonToplevel *toplevel,
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

/*! \brief Remove change notification handlers from a LeptonToplevel.
 * \par Function Description
 * Removes a set of change notification handlers and their associated
 * \a user_data from \a toplevel.  If no registered set of handlers
 * matches the given \a pre_change_func, \a change_func and \a
 * user_data, does nothing.
 *
 * \see lepton_object_add_change_notify()
 *
 * \param toplevel #LeptonToplevel structure to remove handlers from.
 * \param pre_change_func Function called just before changes.
 * \param change_func Function called just after changes.
 * \param user_data User data passed to callback functions.
 */
void
lepton_object_remove_change_notify (LeptonToplevel *toplevel,
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
 *
 * Calls each pre-change callback function registered with \a
 * object's #LeptonToplevel to notify listeners that \a object is about
 * to be modified.  All liblepton functions that modify #LeptonObject
 * structures should call this just before making a change to an
 * #LeptonObject.
 *
 * \param object   #LeptonObject structure to emit notifications for.
 */
void
lepton_object_emit_pre_change_notify (LeptonObject *object)
{
  GList *iter;

  if (object->page == NULL) {
    return;
  }

  LeptonToplevel *toplevel = object->page->toplevel;

  if (toplevel == NULL) {
    return;
  }

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
 *
 * Calls each change callback function registered with \a object's
 * #LeptonToplevel to notify listeners that \a object has just been
 * modified.  All liblepton functions that modify #LeptonObject
 * structures should call this just after making a change to an
 * #LeptonObject.
 *
 * \param object   #LeptonObject structure to emit notifications for.
 */
void
lepton_object_emit_change_notify (LeptonObject *object)
{
  GList *iter;

  if (object->page == NULL) {
    return;
  }

  LeptonToplevel *toplevel = object->page->toplevel;

  if (toplevel == NULL) {
    return;
  }

  for (iter = toplevel->change_notify_funcs;
       iter != NULL; iter = g_list_next (iter)) {

    struct change_notify_entry *entry =
      (struct change_notify_entry *) iter->data;

    if ((entry != NULL) && (entry->change_func != NULL)) {
      entry->change_func (entry->user_data, object);
    }
  }
}

/*! \brief Return the bounds of the given object.
 *  \par Given an object, calculate the bounds coordinates.
 *
 *  \param [in] o_current The object to look the bounds for.
 *  \param [in] include_hidden If bounds of hidden objects should
 *                             be calculated.
 *  \param [out] rleft   pointer to the left coordinate of the object.
 *  \param [out] rtop    pointer to the top coordinate of the object.
 *  \param [out] rright  pointer to the right coordinate of the object.
 *  \param [out] rbottom pointer to the bottom coordinate of the object.
 *  \return If any bounds were found for the object
 *  \retval 0 No bound was found
 *  \retval 1 Bound was found
 */
gboolean
lepton_object_calculate_visible_bounds (LeptonObject *o_current,
                                        gboolean include_hidden,
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
  if (lepton_object_is_text (o_current) &&
      ! (lepton_text_object_is_visible (o_current) || include_hidden)) {
    return 0;
  }

  LeptonBounds bounds;

  switch (lepton_object_get_type (o_current)) {

  case(OBJ_LINE):
    if (o_current->line == NULL) {
      return 0;
    }
    lepton_line_object_calculate_bounds (o_current, &bounds);
    break;

  case(OBJ_NET):
    if (o_current->line == NULL) {
      return 0;
    }
    lepton_net_object_calculate_bounds (o_current, &bounds);
    break;

  case(OBJ_BUS):
    if (o_current->line == NULL) {
      return 0;
    }
    lepton_bus_object_calculate_bounds (o_current, &bounds);
    break;

  case(OBJ_BOX):
    if (o_current->box == NULL) {
      return 0;
    }
    lepton_box_object_calculate_bounds (o_current, &bounds);
    break;

  case(OBJ_PATH):
    g_return_val_if_fail (o_current->path != NULL, 0);
    if (lepton_path_object_get_num_sections (o_current) <= 0)
    {
      return 0;
    }
    lepton_path_object_calculate_bounds (o_current, &bounds);
    break;

  case(OBJ_PICTURE):
    if (o_current->picture == NULL) {
      return 0;
    }
    lepton_picture_object_calculate_bounds (o_current, &bounds);
    break;

  case(OBJ_CIRCLE):
    if (o_current->circle == NULL) {
      return 0;
    }
    lepton_circle_object_calculate_bounds (o_current, &bounds);
    break;

  case(OBJ_COMPONENT):
    if (lepton_component_object_get_contents (o_current) == NULL)
      return 0;

    lepton_component_object_calculate_bounds (o_current,
                                              include_hidden,
                                              &bounds);
    break;

  case(OBJ_PIN):
    if (o_current->line == NULL) {
      return 0;
    }
    lepton_pin_object_calculate_bounds (o_current, &bounds);
    break;

  case(OBJ_ARC):
    if (o_current->arc == NULL) {
      return 0;
    }
    lepton_arc_object_calculate_bounds (o_current,
                                        &bounds.min_x,
                                        &bounds.min_y,
                                        &bounds.max_x,
                                        &bounds.max_y);
    break;

  case(OBJ_TEXT):
    if (!lepton_text_object_calculate_bounds (o_current,
                                              include_hidden,
                                              &bounds))
    {
      return 0;
    }
    break;

  default:
    return 0;
  }

  o_current->bounds = bounds;

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


/*! \brief Allocate and initialise an object.
 *  \par Function Description
 *  Allocates memory for an #LeptonObject and then initializes the
 *  members of its structure.
 *
 *  \param [in] type      The object type; one of the OBJ_* constants.
 *  \param [in] name      A prefix for the object's session-unique name.
 *  \return A pointer to the fully constructed #LeptonObject.
 */
LeptonObject*
lepton_object_new (int type,
                   char const *name)
{
  LeptonObject* new_node = g_new0 (LeptonObject, 1);

  /* setup sid */
  lepton_object_set_id (new_node, global_sid++);
  lepton_object_set_type (new_node, type);

  /* Setup the name */
  new_node->name = g_strdup_printf("%s.%d", name, lepton_object_get_id (new_node));

  /* Don't associate with a page, initially */
  new_node->page = NULL;

  /* Setup the bounding box */
  lepton_bounds_init (&(new_node->bounds));

  /* Setup line/circle structs */
  new_node->line = NULL;
  new_node->path = NULL;
  new_node->circle = NULL;
  new_node->arc = NULL;
  new_node->box = NULL;
  new_node->picture = NULL;
  new_node->text = NULL;
  new_node->component = NULL;

  new_node->conn_list = NULL;

  new_node->stroke = lepton_stroke_new ();
  new_node->fill = lepton_fill_new ();

  new_node->component_basename = NULL;
  new_node->parent = NULL;

  /* Setup the color */
  lepton_object_set_color (new_node, default_color_id());
  new_node->dont_redraw = FALSE;
  new_node->selectable = TRUE;
  new_node->selected = FALSE;

  new_node->bus_ripper_direction = 0;

  new_node->attribs = NULL;
  new_node->attached_to = NULL;
  new_node->copied_to = NULL;

  new_node->pin_type = PIN_TYPE_NET;
  new_node->whichend = -1;

  new_node->weak_refs = NULL;

  return(new_node);
}


const char*
lepton_object_visibility_to_string (gint visibility)
{
  const char *result = NULL;

  switch (visibility)
  {
  case VISIBLE: result = "visible"; break;
  case INVISIBLE: result = "invisible"; break;
  default: break;
  }

  return result;
}


gint
lepton_object_visibility_from_string (char *s)
{
  gint result = VISIBLE;

  if      (strcmp (s, "visible") == 0) { result = VISIBLE; }
  else if (strcmp (s, "invisible") == 0) { result = INVISIBLE; }

  return result;
}
