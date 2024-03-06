/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2024 Lepton EDA Contributors
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
/*!
 * \file gschem_selection_adapter.c
 *
 * \brief
 */

#include <config.h>
#include "gschem.h"

/*! \private
 */
enum
{
  PROP_0,
  PROP_CAP_STYLE,
  PROP_DASH_LENGTH,
  PROP_DASH_SPACE,
  PROP_FILL_ANGLE1,
  PROP_FILL_ANGLE2,
  PROP_FILL_PITCH1,
  PROP_FILL_PITCH2,
  PROP_FILL_TYPE,
  PROP_FILL_WIDTH,
  PROP_LINE_TYPE,
  PROP_LINE_WIDTH,
  PROP_OBJECT_COLOR,
  PROP_PIN_TYPE,
  PROP_TEXT_ALIGNMENT,
  PROP_TEXT_COLOR,
  PROP_TEXT_ROTATION,
  PROP_TEXT_SIZE,
  PROP_TEXT_STRING
};



G_DEFINE_TYPE (GschemSelectionAdapter,
               gschem_selection_adapter,
               G_TYPE_OBJECT);


static void
gschem_selection_adapter_class_init (GschemSelectionAdapterClass *klass);

static GList*
get_selection_iter (GschemSelectionAdapter *adapter);

static void
get_property (GObject *object, guint param_id, GValue *value, GParamSpec *pspec);

static void
gschem_selection_adapter_init (GschemSelectionAdapter *adapter);

static void
selection_changed (LeptonList *selection,
                   GschemSelectionAdapter *adapter);
static void
set_property (GObject *object, guint param_id, const GValue *value, GParamSpec *pspec);



/*! \brief Get the cap style from the selection
 *
 *  \param [in] adapter This adapter
 *
 *  \retval NO_SELECTION    No objects are selected
 *  \retval MULTIPLE_VALUES Multiple objects with different cap styles are selected
 *  \retval others          The cap style of the selected objects
 */
int
gschem_selection_adapter_get_cap_style (GschemSelectionAdapter *adapter)
{
  gint cap_style = NO_SELECTION;
  GList *iter = get_selection_iter (adapter);

  while (iter != NULL) {
    LeptonObject *object = (LeptonObject*) iter->data;
    gboolean success;
    LeptonStrokeCapType temp_cap_style;
    gint temp_dash_length;
    gint temp_dash_space;
    LeptonStrokeType temp_line_type;
    gint temp_line_width;

    success = lepton_object_get_line_options (object,
                                              &temp_cap_style,
                                              &temp_line_type,
                                              &temp_line_width,
                                              &temp_dash_length,
                                              &temp_dash_space);

    if (success) {
      if (cap_style < 0) {
        cap_style = temp_cap_style;
      }
      else if (cap_style != temp_cap_style) {
        cap_style = MULTIPLE_VALUES;
        break;
      }
    }

    iter = g_list_next (iter);
  }

  return cap_style;
}



/*! \brief Get the dash_length from the selection
 *
 *  \param [in] adapter This adapter
 *
 *  \retval NO_SELECTION    No objects are selected
 *  \retval MULTIPLE_VALUES Multiple objects with different dash lengths are selected
 *  \retval others          The dash length of the selected objects
 */
int
gschem_selection_adapter_get_dash_length (GschemSelectionAdapter *adapter)
{
  gint dash_length = NO_SELECTION;
  GList *iter = get_selection_iter (adapter);

  while (iter != NULL) {
    LeptonObject *object = (LeptonObject*) iter->data;
    gboolean success;
    LeptonStrokeCapType temp_cap_style;
    gint temp_dash_length;
    gint temp_dash_space;
    LeptonStrokeType temp_line_type;
    gint temp_line_width;

    success = lepton_object_get_line_options (object,
                                              &temp_cap_style,
                                              &temp_line_type,
                                              &temp_line_width,
                                              &temp_dash_length,
                                              &temp_dash_space);

    if (success) {
      if (dash_length < 0) {
        dash_length = temp_dash_length;
      }
      else if (dash_length != temp_dash_length) {
        dash_length = MULTIPLE_VALUES;
        break;
      }
    }

    iter = g_list_next (iter);
  }

  return dash_length;
}



/*! \brief Get the dash space from the selection
 *
 *  \param [in] adapter This adapter
 *
 *  \retval NO_SELECTION    No objects are selected
 *  \retval MULTIPLE_VALUES Multiple objects with different dash spacings are selected
 *  \retval others          The dash spacing of the selected objects
 */
int
gschem_selection_adapter_get_dash_space (GschemSelectionAdapter *adapter)
{
  gint dash_space = NO_SELECTION;
  GList *iter = get_selection_iter (adapter);

  while (iter != NULL) {
    LeptonObject *object = (LeptonObject*) iter->data;
    gboolean success;
    LeptonStrokeCapType temp_cap_style;
    gint temp_dash_length;
    gint temp_dash_space;
    LeptonStrokeType temp_line_type;
    gint temp_line_width;

    success = lepton_object_get_line_options (object,
                                              &temp_cap_style,
                                              &temp_line_type,
                                              &temp_line_width,
                                              &temp_dash_length,
                                              &temp_dash_space);

    if (success) {
      if (dash_space < 0) {
        dash_space = temp_dash_space;
      }
      else if (dash_space != temp_dash_space) {
        dash_space = MULTIPLE_VALUES;
        break;
      }
    }

    iter = g_list_next (iter);
  }

  return dash_space;
}



/*! \brief Get the first fill line angle of the selected objects
 *
 *  \param [in] adapter This adapter
 *
 *  \retval NO_SELECTION    No objects are selected
 *  \retval MULTIPLE_VALUES Multiple objects with different fill line angles are selected
 *  \retval others          The fill line angle of the selected objects
 */
int
gschem_selection_adapter_get_fill_angle1 (GschemSelectionAdapter *adapter)
{
  gint fill_angle = NO_SELECTION;
  GList *iter = get_selection_iter (adapter);

  while (iter != NULL) {
    LeptonObject *object = (LeptonObject*) iter->data;
    gboolean success;
    gint temp_angle1;
    gint temp_angle2;
    LeptonFillType temp_fill_type;
    gint temp_pitch1;
    gint temp_pitch2;
    gint temp_width;

    success = lepton_object_get_fill_options (object,
                                              &temp_fill_type,
                                              &temp_width,
                                              &temp_pitch1,
                                              &temp_angle1,
                                              &temp_pitch2,
                                              &temp_angle2);


    if (success) {
      if (fill_angle < 0) {
        fill_angle = temp_angle1;
      }
      else if (fill_angle != temp_angle1) {
        fill_angle = MULTIPLE_VALUES;
        break;
      }
    }

    iter = g_list_next (iter);
  }

  return fill_angle;
}



/*! \brief Get the second fill line angle of the selected objects
 *
 *  \param [in] adapter This adapter
 *
 *  \retval NO_SELECTION    No objects are selected
 *  \retval MULTIPLE_VALUES Multiple objects with different fill line angles are selected
 *  \retval others          The fill line angle of the selected objects
 */
int
gschem_selection_adapter_get_fill_angle2 (GschemSelectionAdapter *adapter)
{
  gint fill_angle = NO_SELECTION;
  GList *iter = get_selection_iter (adapter);

  while (iter != NULL) {
    LeptonObject *object = (LeptonObject*) iter->data;
    gboolean success;
    gint temp_angle1;
    gint temp_angle2;
    LeptonFillType temp_fill_type;
    gint temp_pitch1;
    gint temp_pitch2;
    gint temp_width;

    success = lepton_object_get_fill_options (object,
                                              &temp_fill_type,
                                              &temp_width,
                                              &temp_pitch1,
                                              &temp_angle1,
                                              &temp_pitch2,
                                              &temp_angle2);


    if (success) {
      if (fill_angle < 0) {
        fill_angle = temp_angle2;
      }
      else if (fill_angle != temp_angle2) {
        fill_angle = MULTIPLE_VALUES;
        break;
      }
    }

    iter = g_list_next (iter);
  }

  return fill_angle;
}



/*! \brief Get the first fill line pitch of the selected objects
 *
 *  \param [in] adapter This adapter
 *
 *  \retval NO_SELECTION    No objects are selected
 *  \retval MULTIPLE_VALUES Multiple objects with different fill line pitches are selected
 *  \retval others          The fill line pitch of the selected objects
 */
int
gschem_selection_adapter_get_fill_pitch1 (GschemSelectionAdapter *adapter)
{
  gint fill_pitch = NO_SELECTION;
  GList *iter = get_selection_iter (adapter);

  while (iter != NULL) {
    LeptonObject *object = (LeptonObject*) iter->data;
    gboolean success;
    gint temp_angle1;
    gint temp_angle2;
    LeptonFillType temp_fill_type;
    gint temp_pitch1;
    gint temp_pitch2;
    gint temp_width;

    success = lepton_object_get_fill_options (object,
                                              &temp_fill_type,
                                              &temp_width,
                                              &temp_pitch1,
                                              &temp_angle1,
                                              &temp_pitch2,
                                              &temp_angle2);


    if (success) {
      if (fill_pitch < 0) {
        fill_pitch = temp_pitch1;
      }
      else if (fill_pitch != temp_pitch1) {
        fill_pitch = MULTIPLE_VALUES;
        break;
      }
    }

    iter = g_list_next (iter);
  }

  return fill_pitch;
}



/*! \brief Get the second fill line pitch of the selected objects
 *
 *  \param [in] adapter This adapter
 *
 *  \retval NO_SELECTION    No objects are selected
 *  \retval MULTIPLE_VALUES Multiple objects with different fill line pitches are selected
 *  \retval others          The fill line pitch of the selected objects
 */
int
gschem_selection_adapter_get_fill_pitch2 (GschemSelectionAdapter *adapter)
{
  gint fill_pitch = NO_SELECTION;
  GList *iter = get_selection_iter (adapter);

  while (iter != NULL) {
    LeptonObject *object = (LeptonObject*) iter->data;
    gboolean success;
    gint temp_angle1;
    gint temp_angle2;
    LeptonFillType temp_fill_type;
    gint temp_pitch1;
    gint temp_pitch2;
    gint temp_width;

    success = lepton_object_get_fill_options (object,
                                              &temp_fill_type,
                                              &temp_width,
                                              &temp_pitch1,
                                              &temp_angle1,
                                              &temp_pitch2,
                                              &temp_angle2);


    if (success) {
      if (fill_pitch < 0) {
        fill_pitch = temp_pitch2;
      }
      else if (fill_pitch != temp_pitch2) {
        fill_pitch = MULTIPLE_VALUES;
        break;
      }
    }

    iter = g_list_next (iter);
  }

  return fill_pitch;
}



/*! \brief Get the fill type from the selection
 *
 *  \param [in] adapter This adapter
 *
 *  \retval NO_SELECTION    No objects are selected
 *  \retval MULTIPLE_VALUES Multiple objects with different fill types are selected
 *  \retval others          The fill type of the selected objects
 */
int
gschem_selection_adapter_get_fill_type (GschemSelectionAdapter *adapter)
{
  gint fill_type = NO_SELECTION;
  GList *iter = get_selection_iter (adapter);

  while (iter != NULL) {
    LeptonObject *object = (LeptonObject*) iter->data;
    gboolean success;
    gint temp_angle1;
    gint temp_angle2;
    LeptonFillType temp_fill_type;
    gint temp_pitch1;
    gint temp_pitch2;
    gint temp_width;

    success = lepton_object_get_fill_options (object,
                                              &temp_fill_type,
                                              &temp_width,
                                              &temp_pitch1,
                                              &temp_angle1,
                                              &temp_pitch2,
                                              &temp_angle2);


    if (success) {
      if (fill_type < 0) {
        fill_type = temp_fill_type;
      }
      else if (fill_type != temp_fill_type) {
        fill_type = MULTIPLE_VALUES;
        break;
      }
    }

    iter = g_list_next (iter);
  }

  return fill_type;
}



/*! \brief Get the width of the fill lines from the selection
 *
 *  \param [in] adapter This adapter
 *
 *  \retval NO_SELECTION    No objects are selected
 *  \retval MULTIPLE_VALUES Multiple objects with different fill line widths are selected
 *  \retval others          The fill line width of the selected objects
 */
int
gschem_selection_adapter_get_fill_width (GschemSelectionAdapter *adapter)
{
  gint fill_width = NO_SELECTION;
  GList *iter = get_selection_iter (adapter);

  while (iter != NULL) {
    LeptonObject *object = (LeptonObject*) iter->data;
    gboolean success;
    gint temp_angle1;
    gint temp_angle2;
    LeptonFillType temp_fill_type;
    gint temp_pitch1;
    gint temp_pitch2;
    gint temp_width;

    success = lepton_object_get_fill_options (object,
                                              &temp_fill_type,
                                              &temp_width,
                                              &temp_pitch1,
                                              &temp_angle1,
                                              &temp_pitch2,
                                              &temp_angle2);


    if (success) {
      if (fill_width < 0) {
        fill_width = temp_width;
      }
      else if (fill_width != temp_width) {
        fill_width = MULTIPLE_VALUES;
        break;
      }
    }

    iter = g_list_next (iter);
  }

  return fill_width;
}



/*! \brief Get the line_type from the selection
 *
 *  \param [in] adapter This adapter
 *
 *  \retval NO_SELECTION    No objects are selected
 *  \retval MULTIPLE_VALUES Multiple objects with different line types are selected
 *  \retval others          The line type of the selected objects
 */
int
gschem_selection_adapter_get_line_type (GschemSelectionAdapter *adapter)
{
  gint line_type = NO_SELECTION;
  GList *iter = get_selection_iter (adapter);

  while (iter != NULL) {
    LeptonObject *object = (LeptonObject*) iter->data;
    gboolean success;
    LeptonStrokeCapType temp_cap_style;
    gint temp_dash_length;
    gint temp_dash_space;
    LeptonStrokeType temp_line_type;
    gint temp_line_width;

    success = lepton_object_get_line_options (object,
                                              &temp_cap_style,
                                              &temp_line_type,
                                              &temp_line_width,
                                              &temp_dash_length,
                                              &temp_dash_space);

    if (success) {
      if (line_type < 0) {
        line_type = temp_line_type;
      }
      else if (line_type != temp_line_type) {
        line_type = MULTIPLE_VALUES;
        break;
      }
    }

    iter = g_list_next (iter);
  }

  return line_type;
}



/*! \brief Get the line width from the selection
 *
 *  \param [in] adapter This adapter
 *
 *  \retval NO_SELECTION    No objects are selected
 *  \retval MULTIPLE_VALUES Multiple objects with different widths are selected
 *  \retval others          The width of the selected objects
 */
int
gschem_selection_adapter_get_line_width (GschemSelectionAdapter *adapter)
{
  gint line_width = NO_SELECTION;
  GList *iter = get_selection_iter (adapter);

  while (iter != NULL) {
    LeptonObject *object = (LeptonObject*) iter->data;
    gboolean success;
    LeptonStrokeCapType temp_cap_style;
    gint temp_dash_length;
    gint temp_dash_space;
    LeptonStrokeType temp_line_type;
    gint temp_line_width;

    success = lepton_object_get_line_options (object,
                                              &temp_cap_style,
                                              &temp_line_type,
                                              &temp_line_width,
                                              &temp_dash_length,
                                              &temp_dash_space);

    if (success) {
      if (line_width < 0) {
        line_width = temp_line_width;
      }
      else if (line_width != temp_line_width) {
        line_width = MULTIPLE_VALUES;
        break;
      }
    }

    iter = g_list_next (iter);
  }

  return line_width;
}



/*! \brief Get the color of selected objects
 *
 *  \param [in] adapter This adapter
 *
 *  \retval NO_SELECTION    No objects are selected
 *  \retval MULTIPLE_VALUES Multiple objects with different colors are selected
 *  \retval others          The color of the selected objects
 */
int
gschem_selection_adapter_get_object_color (GschemSelectionAdapter *adapter)
{
  int color = NO_SELECTION;
  GList *iter = get_selection_iter (adapter);

  while (iter != NULL) {
    LeptonObject* object = (LeptonObject *) iter->data;
    iter = g_list_next (iter);
    if ((object != NULL) &&
        (lepton_object_is_arc (object)  ||
         lepton_object_is_box (object)  ||
         lepton_object_is_bus (object)  ||
         lepton_object_is_net (object)  ||
         lepton_object_is_line (object) ||
         lepton_object_is_path (object) ||
         lepton_object_is_text (object) ||
         lepton_object_is_circle (object)))
    {
      color = lepton_object_get_color (object);
      break;
    }
  }

  /* Check if all other objects have the same properties */

  while (iter != NULL) {
    LeptonObject* object = (LeptonObject *) iter->data;
    if ((object != NULL) &&
        (lepton_object_is_arc (object)  ||
         lepton_object_is_box (object)  ||
         lepton_object_is_line (object) ||
         lepton_object_is_path (object) ||
         lepton_object_is_text (object) ||
         lepton_object_is_circle (object)))
    {
      if (color != lepton_object_get_color (object)) {
        color = MULTIPLE_VALUES;
        break;
      }
    }
    iter = g_list_next (iter);
  }

  return color;
}



/*! \brief Get the pin type of selected objects
 *
 *  \param [in] adapter This adapter
 *
 *  \retval NO_SELECTION    No objects are selected
 *  \retval MULTIPLE_VALUES Multiple objects with different pin types are selected
 *  \retval others          The pin type of the selected objects
 */
int
gschem_selection_adapter_get_pin_type (GschemSelectionAdapter *adapter)
{
  int type = NO_SELECTION;
  GList *iter = get_selection_iter (adapter);

  while (iter != NULL) {
    LeptonObject* object = (LeptonObject *) iter->data;
    iter = g_list_next (iter);
    if (lepton_object_is_pin (object))
    {
      type = object->pin_type;
      break;
    }
  }

  /* Check if all other objects have the same properties */

  while (iter != NULL) {
    LeptonObject* object = (LeptonObject *) iter->data;
    if (lepton_object_is_pin (object))
    {
      if (type != object->pin_type) {
        type = MULTIPLE_VALUES;
        break;
      }
    }
    iter = g_list_next (iter);
  }

  return type;
}



/*! \brief Get the selection associated with this adapter
 *
 *  \param [in] adapter This adapter
 *  \return The libgeda selection
 */
LeptonSelection*
gschem_selection_adapter_get_selection (GschemSelectionAdapter *adapter)
{
  g_return_val_if_fail (adapter != NULL, NULL);

  return adapter->selection;
}



/*! \brief Get the text alignment from the selection
 *
 *  \param [in] adapter This adapter
 *
 *  \retval NO_SELECTION    No objects are selected
 *  \retval MULTIPLE_VALUES Multiple objects with different rotations are selected
 *  \retval others          The rotation of the selected objects
 */
int
gschem_selection_adapter_get_text_alignment (GschemSelectionAdapter *adapter)
{
  gint alignment = NO_SELECTION;
  GList *iter = get_selection_iter (adapter);

  while (iter != NULL) {
    LeptonObject *object = (LeptonObject*) iter->data;

    if (lepton_object_is_text (object))
    {
      int temp_alignment = lepton_text_object_get_alignment (object);

      if (alignment < 0) {
        alignment = temp_alignment;
      }
      else if (alignment != temp_alignment) {
        alignment = MULTIPLE_VALUES;
        break;
      }
    }

    iter = g_list_next (iter);
  }

  return alignment;
}



/*! \brief Get the color of selected objects
 *
 *  \param [in] adapter This adapter
 *
 *  \retval NO_SELECTION    No objects are selected
 *  \retval MULTIPLE_VALUES Multiple objects with different colors are selected
 *  \retval others          The color of the selected objects
 */
int
gschem_selection_adapter_get_text_color (GschemSelectionAdapter *adapter)
{
  int color = NO_SELECTION;
  GList *iter = get_selection_iter (adapter);

  while (iter != NULL) {
    LeptonObject* object = (LeptonObject *) iter->data;
    iter = g_list_next (iter);
    if (lepton_object_is_text (object))
    {
      color = lepton_object_get_color (object);
      break;
    }
  }

  /* Check if all other objects have the same properties */

  while (iter != NULL) {
    LeptonObject* object = (LeptonObject *) iter->data;
    if (lepton_object_is_text (object))
    {
      if (color != lepton_object_get_color (object)) {
        color = MULTIPLE_VALUES;
        break;
      }
    }
    iter = g_list_next (iter);
  }

  return color;
}



/*! \brief Get the text rotation angle from the selection
 *
 *  \param [in] adapter This adapter
 *
 *  \retval NO_SELECTION    No objects are selected
 *  \retval MULTIPLE_VALUES Multiple objects with different rotations are selected
 *  \retval others          The rotation of the selected objects
 */
int
gschem_selection_adapter_get_text_rotation (GschemSelectionAdapter *adapter)
{
  gint angle = NO_SELECTION;
  GList *iter = get_selection_iter (adapter);

  while (iter != NULL) {
    LeptonObject *object = (LeptonObject*) iter->data;

    if (lepton_object_is_text (object))
    {
      int temp_angle = lepton_text_object_get_angle (object);

      if (angle < 0) {
        angle = temp_angle;
      }
      else if (angle != temp_angle) {
        angle = MULTIPLE_VALUES;
        break;
      }
    }

    iter = g_list_next (iter);
  }

  return angle;
}



/*! \brief Get the text size from the selection
 *
 *  \param [in] adapter This adapter
 *
 *  \retval NO_SELECTION    No objects are selected
 *  \retval MULTIPLE_VALUES Multiple objects with different rotations are selected
 *  \retval others          The rotation of the selected objects
 */
int
gschem_selection_adapter_get_text_size (GschemSelectionAdapter *adapter)
{
  gint size = NO_SELECTION;
  GList *iter = get_selection_iter (adapter);

  while (iter != NULL) {
    LeptonObject *object = (LeptonObject*) iter->data;

    if (lepton_object_is_text (object))
    {
      int temp_size = lepton_text_object_get_size (object);

      if (size < 0) {
        size = temp_size;
      }
      else if (size != temp_size) {
        size = MULTIPLE_VALUES;
        break;
      }
    }

    iter = g_list_next (iter);
  }

  return size;
}


/*! \brief Get the text string from the selection
 *
 *  \param [in] adapter This adapter
 *
 *  \retval NULL      No objects or multiple values are selected
 *  \retval non-NULL  The content string of the selected text object [transfer none]
 */
const char*
gschem_selection_adapter_get_text_string (GschemSelectionAdapter *adapter)
{
  const char *string = NULL;
  GList *iter = get_selection_iter (adapter);

  while (iter != NULL) {
    LeptonObject *object = (LeptonObject*) iter->data;

    if (lepton_object_is_text (object))
    {
      if (string == NULL) {
        string = lepton_text_object_get_string (object);
      } else {
        string = NULL;
        break;
      }
    }

    iter = g_list_next (iter);
  }

  return string;
}




/*! \brief Get the libgeda toplevel associated with this adapter
 *
 *  \param [in] adapter This adapter
 *  \return The libgeda toplevel
 */
LeptonToplevel*
gschem_selection_adapter_get_toplevel (GschemSelectionAdapter *adapter)
{
  g_return_val_if_fail (adapter != NULL, NULL);

  return adapter->toplevel;
}



/*! \brief Create a new instance of the GschemSelectionAdapter
 *
 *  \return A new instance of the GschemSelectionAdapter
 */
GschemSelectionAdapter*
gschem_selection_adapter_new ()
{
  return GSCHEM_SELECTION_ADAPTER (g_object_new (GSCHEM_TYPE_SELECTION_ADAPTER, NULL));
}



/*! \brief Set the first fill angle in the selection
 *
 *  \param [in] adapter The #GschemSelectionAdapter structure.
 *  \param [in] angle The new angle.
 */
void
gschem_selection_adapter_set_fill_angle1 (GschemSelectionAdapter *adapter, int angle)
{
  GList *iter;

  g_return_if_fail (adapter != NULL);

  if ((adapter->selection == NULL) || (adapter->toplevel == NULL)) {
    return;
  }

  g_return_if_fail (adapter->toplevel->page_current != NULL);
  g_return_if_fail (adapter->toplevel->page_current->selection_list == adapter->selection);
  g_return_if_fail (angle >= 0);

  iter = lepton_list_get_glist (adapter->selection);

  while (iter != NULL) {
    LeptonObject *object = (LeptonObject*) iter->data;
    gboolean success;
    gint temp_angle1;
    gint temp_angle2;
    LeptonFillType temp_fill_type;
    gint temp_pitch1;
    gint temp_pitch2;
    gint temp_width;

    success = lepton_object_get_fill_options (object,
                                              &temp_fill_type,
                                              &temp_width,
                                              &temp_pitch1,
                                              &temp_angle1,
                                              &temp_pitch2,
                                              &temp_angle2);

    if (success) {
      lepton_object_set_fill_options (object,
                                      temp_fill_type,
                                      temp_width,
                                      temp_pitch1,
                                      angle,
                                      temp_pitch2,
                                      temp_angle2);
    }

    iter = g_list_next (iter);
  }

  g_object_notify (G_OBJECT (adapter), "fill-angle1");

  g_signal_emit_by_name (adapter, "handle-undo");
}


/*! \brief Set the fill angle 2 in the selection
 *
 *  \param [in] adapter The #GschemSelectionAdapter structure.
 *  \param [in] angle The new angle.
 */
void
gschem_selection_adapter_set_fill_angle2 (GschemSelectionAdapter *adapter, int angle)
{
  GList *iter;

  g_return_if_fail (adapter != NULL);

  if ((adapter->selection == NULL) || (adapter->toplevel == NULL)) {
    return;
  }

  g_return_if_fail (adapter->toplevel->page_current != NULL);
  g_return_if_fail (adapter->toplevel->page_current->selection_list == adapter->selection);
  g_return_if_fail (angle >= 0);

  iter = lepton_list_get_glist (adapter->selection);

  while (iter != NULL) {
    LeptonObject *object = (LeptonObject*) iter->data;
    gboolean success;
    gint temp_angle1;
    gint temp_angle2;
    LeptonFillType temp_fill_type;
    gint temp_pitch1;
    gint temp_pitch2;
    gint temp_width;

    success = lepton_object_get_fill_options (object,
                                              &temp_fill_type,
                                              &temp_width,
                                              &temp_pitch1,
                                              &temp_angle1,
                                              &temp_pitch2,
                                              &temp_angle2);

    if (success) {
      lepton_object_set_fill_options (object,
                                      temp_fill_type,
                                      temp_width,
                                      temp_pitch1,
                                      temp_angle1,
                                      temp_pitch2,
                                      angle);
    }

    iter = g_list_next (iter);
  }

  g_object_notify (G_OBJECT (adapter), "fill-angle2");

  g_signal_emit_by_name (adapter, "handle-undo");
}




/*! \brief Set the fill pitch 1 in the selection
 *
 *  \param [in] adapter The #GschemSelectionAdapter structure.
 *  \param [in] pitch The new fill pitch.
 */
void
gschem_selection_adapter_set_fill_pitch1 (GschemSelectionAdapter *adapter, int pitch)
{
  GList *iter;

  g_return_if_fail (adapter != NULL);

  if ((adapter->selection == NULL) || (adapter->toplevel == NULL)) {
    return;
  }

  g_return_if_fail (adapter->toplevel->page_current != NULL);
  g_return_if_fail (adapter->toplevel->page_current->selection_list == adapter->selection);
  g_return_if_fail (pitch >= 0);

  iter = lepton_list_get_glist (adapter->selection);

  while (iter != NULL) {
    LeptonObject *object = (LeptonObject*) iter->data;
    gboolean success;
    gint temp_angle1;
    gint temp_angle2;
    LeptonFillType temp_fill_type;
    gint temp_pitch1;
    gint temp_pitch2;
    gint temp_width;

    success = lepton_object_get_fill_options (object,
                                              &temp_fill_type,
                                              &temp_width,
                                              &temp_pitch1,
                                              &temp_angle1,
                                              &temp_pitch2,
                                              &temp_angle2);

    if (success) {
      lepton_object_set_fill_options (object,
                                      temp_fill_type,
                                      temp_width,
                                      pitch,
                                      temp_angle1,
                                      temp_pitch2,
                                      temp_angle2);
    }

    iter = g_list_next (iter);
  }

  g_object_notify (G_OBJECT (adapter), "fill-pitch1");

  g_signal_emit_by_name (adapter, "handle-undo");
}


/*! \brief Set the fill pitch 2 in the selection
 *
 *  \param [in] adapter The #GschemSelectionAdapter structure.
 *  \param [in] pitch The new fill pitch.
 */
void
gschem_selection_adapter_set_fill_pitch2 (GschemSelectionAdapter *adapter, int pitch)
{
  GList *iter;

  g_return_if_fail (adapter != NULL);

  if ((adapter->selection == NULL) || (adapter->toplevel == NULL)) {
    return;
  }

  g_return_if_fail (adapter->toplevel->page_current != NULL);
  g_return_if_fail (adapter->toplevel->page_current->selection_list == adapter->selection);
  g_return_if_fail (pitch >= 0);

  iter = lepton_list_get_glist (adapter->selection);

  while (iter != NULL) {
    LeptonObject *object = (LeptonObject*) iter->data;
    gboolean success;
    gint temp_angle1;
    gint temp_angle2;
    LeptonFillType temp_fill_type;
    gint temp_pitch1;
    gint temp_pitch2;
    gint temp_width;

    success = lepton_object_get_fill_options (object,
                                              &temp_fill_type,
                                              &temp_width,
                                              &temp_pitch1,
                                              &temp_angle1,
                                              &temp_pitch2,
                                              &temp_angle2);

    if (success) {
      lepton_object_set_fill_options (object,
                                      temp_fill_type,
                                      temp_width,
                                      temp_pitch1,
                                      temp_angle1,
                                      pitch,
                                      temp_angle2);
    }

    iter = g_list_next (iter);
  }

  g_object_notify (G_OBJECT (adapter), "fill-pitch2");

  g_signal_emit_by_name (adapter, "handle-undo");
}




/*! \brief Set the fill type in the selection
 *
 *  \param [in] adapter The #GschemSelectionAdapter structure.
 *  \param [in] fill_type The new fill type.
 */
void
gschem_selection_adapter_set_fill_type (GschemSelectionAdapter *adapter, int fill_type)
{
  GList *iter;

  g_return_if_fail (adapter != NULL);

  if ((adapter->selection == NULL) || (adapter->toplevel == NULL)) {
    return;
  }

  g_return_if_fail (adapter->toplevel->page_current != NULL);
  g_return_if_fail (adapter->toplevel->page_current->selection_list == adapter->selection);
  g_return_if_fail (fill_type >= 0);

  iter = lepton_list_get_glist (adapter->selection);

  while (iter != NULL) {
    LeptonObject *object = (LeptonObject*) iter->data;
    gboolean success;
    gint temp_angle1;
    gint temp_angle2;
    LeptonFillType temp_fill_type;
    gint temp_pitch1;
    gint temp_pitch2;
    gint temp_width;

    success = lepton_object_get_fill_options (object,
                                              &temp_fill_type,
                                              &temp_width,
                                              &temp_pitch1,
                                              &temp_angle1,
                                              &temp_pitch2,
                                              &temp_angle2);

    if (success) {
      lepton_object_set_fill_options (object,
                                      (LeptonFillType) fill_type,
                                      temp_width,
                                      temp_pitch1,
                                      temp_angle1,
                                      temp_pitch2,
                                      temp_angle2);
    }

    iter = g_list_next (iter);
  }

  g_object_notify (G_OBJECT (adapter), "fill-angle1");
  g_object_notify (G_OBJECT (adapter), "fill-angle2");
  g_object_notify (G_OBJECT (adapter), "fill-pitch1");
  g_object_notify (G_OBJECT (adapter), "fill-pitch2");
  g_object_notify (G_OBJECT (adapter), "fill-type");
  g_object_notify (G_OBJECT (adapter), "fill-width");

  g_signal_emit_by_name (adapter, "handle-undo");
}



/*! \brief Set the fill width in the selection
 *
 *  \param [in] adapter The #GschemSelectionAdapter structure.
 *  \param [in] fill_width The new fill width.
 */
void
gschem_selection_adapter_set_fill_width (GschemSelectionAdapter *adapter, int fill_width)
{
  GList *iter;

  g_return_if_fail (adapter != NULL);

  if ((adapter->selection == NULL) || (adapter->toplevel == NULL)) {
    return;
  }

  g_return_if_fail (adapter->toplevel->page_current != NULL);
  g_return_if_fail (adapter->toplevel->page_current->selection_list == adapter->selection);
  g_return_if_fail (fill_width >= 0);

  iter = lepton_list_get_glist (adapter->selection);

  while (iter != NULL) {
    LeptonObject *object = (LeptonObject*) iter->data;
    gboolean success;
    gint temp_angle1;
    gint temp_angle2;
    LeptonFillType temp_fill_type;
    gint temp_pitch1;
    gint temp_pitch2;
    gint temp_width;

    success = lepton_object_get_fill_options (object,
                                              &temp_fill_type,
                                              &temp_width,
                                              &temp_pitch1,
                                              &temp_angle1,
                                              &temp_pitch2,
                                              &temp_angle2);

    if (success) {
      lepton_object_set_fill_options (object,
                                      temp_fill_type,
                                      fill_width,
                                      temp_pitch1,
                                      temp_angle1,
                                      temp_pitch2,
                                      temp_angle2);
    }

    iter = g_list_next (iter);
  }

  g_object_notify (G_OBJECT (adapter), "fill-width");

  g_signal_emit_by_name (adapter, "handle-undo");
}



/*! \brief Set the line type in the selection
 *
 *  \param [in] adapter The #GschemSelectionAdapter structure.
 *  \param [in] line_type The new line type.
 */
void
gschem_selection_adapter_set_line_type (GschemSelectionAdapter *adapter, int line_type)
{
  GList *iter;

  g_return_if_fail (adapter != NULL);

  if ((adapter->selection == NULL) || (adapter->toplevel == NULL)) {
    return;
  }

  g_return_if_fail (adapter->toplevel->page_current != NULL);
  g_return_if_fail (adapter->toplevel->page_current->selection_list == adapter->selection);
  g_return_if_fail (line_type >= 0);

  iter = lepton_list_get_glist (adapter->selection);

  while (iter != NULL) {
    LeptonObject *object = (LeptonObject*) iter->data;
    gboolean success;
    LeptonStrokeCapType temp_cap_style;
    int temp_dash_length;
    int temp_dash_space;
    LeptonStrokeType temp_line_type;
    int temp_line_width;

    success = lepton_object_get_line_options (object,
                                              &temp_cap_style,
                                              &temp_line_type,
                                              &temp_line_width,
                                              &temp_dash_length,
                                              &temp_dash_space);

    if (success) {
      lepton_object_set_line_options (object,
                                      temp_cap_style,
                                      (LeptonStrokeType) line_type,
                                      temp_line_width,
                                      temp_dash_length,
                                      temp_dash_space);
    }

    iter = g_list_next (iter);
  }

  g_object_notify (G_OBJECT (adapter), "line-type");
  g_object_notify (G_OBJECT (adapter), "dash-length");
  g_object_notify (G_OBJECT (adapter), "dash-space");

  g_signal_emit_by_name (adapter, "handle-undo");
}



/*! \brief Set the line width in the selection
 *
 *  \param [in] adapter The #GschemSelectionAdapter structure.
 *  \param [in] line_width The new line width.
 */
void
gschem_selection_adapter_set_line_width (GschemSelectionAdapter *adapter, int line_width)
{
  GList *iter;

  g_return_if_fail (adapter != NULL);

  if ((adapter->selection == NULL) || (adapter->toplevel == NULL)) {
    return;
  }

  g_return_if_fail (adapter->toplevel->page_current != NULL);
  g_return_if_fail (adapter->toplevel->page_current->selection_list == adapter->selection);
  g_return_if_fail (line_width >= 0);

  iter = lepton_list_get_glist (adapter->selection);

  while (iter != NULL) {
    LeptonObject *object = (LeptonObject*) iter->data;
    gboolean success;
    LeptonStrokeCapType temp_cap_style;
    int temp_dash_length;
    int temp_dash_space;
    LeptonStrokeType temp_line_type;
    int temp_line_width;

    success = lepton_object_get_line_options (object,
                                              &temp_cap_style,
                                              &temp_line_type,
                                              &temp_line_width,
                                              &temp_dash_length,
                                              &temp_dash_space);

    if (success) {
      lepton_object_set_line_options (object,
                                      temp_cap_style,
                                      temp_line_type,
                                      line_width,
                                      temp_dash_length,
                                      temp_dash_space);
    }

    iter = g_list_next (iter);
  }

  g_object_notify (G_OBJECT (adapter), "line-width");

  g_signal_emit_by_name (adapter, "handle-undo");
}



/*! \brief Set the dash length in the selection
 *
 *  \param [in] adapter The #GschemSelectionAdapter structure.
 *  \param [in] dash_length The new dash length value.
 */
void
gschem_selection_adapter_set_dash_length (GschemSelectionAdapter *adapter, int dash_length)
{
  GList *iter;

  g_return_if_fail (adapter != NULL);

  if ((adapter->selection == NULL) || (adapter->toplevel == NULL)) {
    return;
  }

  g_return_if_fail (adapter->toplevel->page_current != NULL);
  g_return_if_fail (adapter->toplevel->page_current->selection_list == adapter->selection);
  g_return_if_fail (dash_length >= 0);

  iter = lepton_list_get_glist (adapter->selection);

  while (iter != NULL) {
    LeptonObject *object = (LeptonObject*) iter->data;
    gboolean success;
    LeptonStrokeCapType temp_cap_style;
    int temp_dash_length;
    int temp_dash_space;
    LeptonStrokeType temp_line_type;
    int temp_line_width;

    success = lepton_object_get_line_options (object,
                                              &temp_cap_style,
                                              &temp_line_type,
                                              &temp_line_width,
                                              &temp_dash_length,
                                              &temp_dash_space);

    if (success) {
      lepton_object_set_line_options (object,
                                      temp_cap_style,
                                      temp_line_type,
                                      temp_line_width,
                                      dash_length,
                                      temp_dash_space);
    }

    iter = g_list_next (iter);
  }

  g_object_notify (G_OBJECT (adapter), "dash-length");

  g_signal_emit_by_name (adapter, "handle-undo");
}



/*! \brief Set the dash spacing in the selection
 *
 *  \param [in] adapter The #GschemSelectionAdapter structure.
 *  \param [in] dash_space The new dash spacing value.
 */
void
gschem_selection_adapter_set_dash_space (GschemSelectionAdapter *adapter, int dash_space)
{
  GList *iter;

  g_return_if_fail (adapter != NULL);

  if ((adapter->selection == NULL) || (adapter->toplevel == NULL)) {
    return;
  }

  g_return_if_fail (adapter->toplevel->page_current != NULL);
  g_return_if_fail (adapter->toplevel->page_current->selection_list == adapter->selection);
  g_return_if_fail (dash_space >= 0);

  iter = lepton_list_get_glist (adapter->selection);

  while (iter != NULL) {
    LeptonObject *object = (LeptonObject*) iter->data;
    gboolean success;
    LeptonStrokeCapType temp_cap_style;
    int temp_dash_length;
    int temp_dash_space;
    LeptonStrokeType temp_line_type;
    int temp_line_width;

    success = lepton_object_get_line_options (object,
                                              &temp_cap_style,
                                              &temp_line_type,
                                              &temp_line_width,
                                              &temp_dash_length,
                                              &temp_dash_space);

    if (success) {
      lepton_object_set_line_options (object,
                                      temp_cap_style,
                                      temp_line_type,
                                      temp_line_width,
                                      temp_dash_length,
                                      dash_space);
    }

    iter = g_list_next (iter);
  }

  g_object_notify (G_OBJECT (adapter), "dash-space");

  g_signal_emit_by_name (adapter, "handle-undo");
}



/*! \brief Set the cap styles in the selection
 *
 *  \param [in] adapter
 *  \param [in] cap_style
 */
void
gschem_selection_adapter_set_cap_style (GschemSelectionAdapter *adapter, int cap_style)
{
  GList *iter;

  g_return_if_fail (adapter != NULL);

  if ((adapter->selection == NULL) || (adapter->toplevel == NULL)) {
    return;
  }

  g_return_if_fail (adapter->toplevel->page_current != NULL);
  g_return_if_fail (adapter->toplevel->page_current->selection_list == adapter->selection);
  g_return_if_fail (cap_style >= 0);

  iter = lepton_list_get_glist (adapter->selection);

  while (iter != NULL) {
    LeptonObject *object = (LeptonObject*) iter->data;
    gboolean success;
    LeptonStrokeCapType temp_cap_style;
    int temp_dash_length;
    int temp_dash_space;
    LeptonStrokeType temp_line_type;
    int temp_line_width;

    success = lepton_object_get_line_options (object,
                                              &temp_cap_style,
                                              &temp_line_type,
                                              &temp_line_width,
                                              &temp_dash_length,
                                              &temp_dash_space);

    if (success) {
      lepton_object_set_line_options (object,
                                      (LeptonStrokeCapType) cap_style,
                                      temp_line_type,
                                      temp_line_width,
                                      temp_dash_length,
                                      temp_dash_space);
    }

    iter = g_list_next (iter);
  }

  g_object_notify (G_OBJECT (adapter), "cap-style");

  g_signal_emit_by_name (adapter, "handle-undo");
}



/*! \brief Set the object color in the selection
 *
 *  \param [in] adapter
 *  \param [in] color
 */
void
gschem_selection_adapter_set_object_color (GschemSelectionAdapter *adapter, int color)
{
  g_return_if_fail (adapter != NULL);
  g_return_if_fail (color_id_valid (color));

  lepton_object_list_set_color (lepton_list_get_glist (adapter->selection),
                                color);

  g_object_notify (G_OBJECT (adapter), "object-color");
  g_object_notify (G_OBJECT (adapter), "text-color");

  g_signal_emit_by_name (adapter, "handle-undo");
}



/*! \brief Set the pin type in the selection
 *
 *  \param [in] adapter The #GschemSelectionAdapter structure.
 *  \param [in] type The new pin type.
 */
void
gschem_selection_adapter_set_pin_type (GschemSelectionAdapter *adapter, int type)
{
  GList *iter;

  g_return_if_fail (adapter != NULL);
  g_return_if_fail ((type == PIN_TYPE_NET) || (type == PIN_TYPE_BUS));

  iter = lepton_list_get_glist (adapter->selection);

  while (iter != NULL) {
    LeptonObject *object = (LeptonObject*) iter->data;

    if (lepton_object_is_pin (object) && object->pin_type != type)
    {
      s_conn_remove_object_connections (object);
      lepton_pin_object_set_type (object, type);
      s_conn_update_object (object->page, object);
    }

    iter = g_list_next (iter);
  }

  g_object_notify (G_OBJECT (adapter), "pin-type");

  g_signal_emit_by_name (adapter, "handle-undo");
}



/*! \brief Set the selection associated with this adapter
 *
 *  \param [in] adapter
 *  \param [in] selection
 */
void
gschem_selection_adapter_set_selection (GschemSelectionAdapter *adapter,
                                        LeptonSelection *selection)
{
  g_return_if_fail (adapter != NULL);

   if (adapter->selection != NULL) {
    g_signal_handlers_disconnect_by_func (adapter->selection,
                                          (gpointer) selection_changed,
                                          adapter);

    g_object_unref (adapter->selection);
  }

  adapter->selection = selection;

  if (adapter->selection != NULL) {
    g_object_ref (adapter->selection);

    g_signal_connect (adapter->selection,
                      "changed",
                      G_CALLBACK (selection_changed),
                      adapter);
  }

  g_object_notify (G_OBJECT (adapter), "cap-style");
  g_object_notify (G_OBJECT (adapter), "dash-length");
  g_object_notify (G_OBJECT (adapter), "dash-space");
  g_object_notify (G_OBJECT (adapter), "fill-angle1");
  g_object_notify (G_OBJECT (adapter), "fill-angle2");
  g_object_notify (G_OBJECT (adapter), "fill-pitch1");
  g_object_notify (G_OBJECT (adapter), "fill-pitch2");
  g_object_notify (G_OBJECT (adapter), "fill-type");
  g_object_notify (G_OBJECT (adapter), "fill-width");
  g_object_notify (G_OBJECT (adapter), "line-type");
  g_object_notify (G_OBJECT (adapter), "line-width");
  g_object_notify (G_OBJECT (adapter), "object-color");
  g_object_notify (G_OBJECT (adapter), "pin-type");
}



/*! \brief Set the text alignment in the selection
 *
 *  \param [in] adapter The #GschemSelectionAdapter structure.
 *  \param [in] alignment The new text alignment.
 */
void
gschem_selection_adapter_set_text_alignment (GschemSelectionAdapter *adapter, int alignment)
{
  GList *iter;

  g_return_if_fail (adapter != NULL);
  g_return_if_fail (adapter->toplevel != NULL);
  g_return_if_fail (alignment >= 0);

  iter = lepton_list_get_glist (adapter->selection);

  while (iter != NULL) {
    LeptonObject *object = (LeptonObject*) iter->data;

    if (lepton_object_is_text (object))
    {
      lepton_text_object_set_alignment (object, alignment);
      lepton_text_object_recreate (object);
    }

    iter = g_list_next (iter);
  }

  g_object_notify (G_OBJECT (adapter), "text-alignment");

  g_signal_emit_by_name (adapter, "handle-undo");
}



/*! \brief Set the text color in the selection
 *
 *  \param [in] adapter
 *  \param [in] color
 */
void
gschem_selection_adapter_set_text_color (GschemSelectionAdapter *adapter, int color)
{
  GList *iter;

  g_return_if_fail (adapter != NULL);
  g_return_if_fail (color_id_valid (color));

  iter = lepton_list_get_glist (adapter->selection);

  while (iter != NULL) {
    LeptonObject *object = (LeptonObject*) iter->data;

    if (lepton_object_is_text (object))
    {
      lepton_object_set_color (object, color);
    }

    iter = g_list_next (iter);
  }

  g_object_notify (G_OBJECT (adapter), "object-color");
  g_object_notify (G_OBJECT (adapter), "text-color");

  g_signal_emit_by_name (adapter, "handle-undo");
}



/*! \brief Set the text rotation in the selection
 *
 *  \param [in] adapter
 *  \param [in] angle
 */
void
gschem_selection_adapter_set_text_rotation (GschemSelectionAdapter *adapter, int angle)
{
  GList *iter;

  g_return_if_fail (adapter != NULL);
  g_return_if_fail (adapter->toplevel != NULL);
  g_return_if_fail (angle >= 0);

  iter = lepton_list_get_glist (adapter->selection);

  while (iter != NULL) {
    LeptonObject *object = (LeptonObject*) iter->data;

    if (lepton_object_is_text (object))
    {
      lepton_text_object_set_angle (object, angle);
      lepton_text_object_recreate (object);
    }

    iter = g_list_next (iter);
  }

  g_object_notify (G_OBJECT (adapter), "text-rotation");

  g_signal_emit_by_name (adapter, "handle-undo");
}



/*! \brief Set the text size in the selection
 *
 *  \param [in] adapter
 *  \param [in] size
 */
void
gschem_selection_adapter_set_text_size (GschemSelectionAdapter *adapter, int size)
{
  GList *iter;

  g_return_if_fail (adapter != NULL);
  g_return_if_fail (adapter->toplevel != NULL);
  g_return_if_fail (size >= MINIMUM_TEXT_SIZE);

  iter = lepton_list_get_glist (adapter->selection);

  while (iter != NULL) {
    LeptonObject *object = (LeptonObject*) iter->data;

    if (lepton_object_is_text (object))
    {
      lepton_text_object_set_size (object, size);
      lepton_text_object_recreate (object);
    }

    iter = g_list_next (iter);
  }

  g_object_notify (G_OBJECT (adapter), "text-size");

  g_signal_emit_by_name (adapter, "handle-undo");
}



/*! \brief Set the text string in the selection
 *
 *  \param [in] adapter The #GschemSelectionAdapter structure.
 *  \param [in] string The new text string.
 *  \param [in] w_current The current #GschemToplevel object.
 */
void
gschem_selection_adapter_set_text_string (GschemSelectionAdapter *adapter, const char *string, GschemToplevel *w_current)
{
  GList *iter;

  g_return_if_fail (w_current != NULL);
  g_return_if_fail (adapter != NULL);
  g_return_if_fail (adapter->toplevel != NULL);
  g_return_if_fail (string != NULL);

  iter = lepton_list_get_glist (adapter->selection);

  while (iter != NULL) {
    LeptonObject *object = (LeptonObject*) iter->data;

    if (lepton_object_is_text (object))
    {
      lepton_text_object_set_string (object, string);

      /* handle slot= attribute, it's a special case */
      LeptonObject *attachment = lepton_object_get_attached_to (object);
      if (attachment != NULL &&
          g_ascii_strncasecmp (string, "slot=", 5) == 0)
      {
        o_slot_end (w_current, attachment, string);
      }

      lepton_text_object_recreate (object);
    }

    iter = g_list_next (iter);
  }

  g_object_notify (G_OBJECT (adapter), "text-string");

  g_signal_emit_by_name (adapter, "handle-undo");
}



/*! \brief Set the toplevel associated with this adapter
 *
 *  \param [in] adapter
 *  \param [in] toplevel
 */
void
gschem_selection_adapter_set_toplevel (GschemSelectionAdapter *adapter,
                                       LeptonToplevel *toplevel)
{
  g_return_if_fail (adapter != NULL);

  adapter->toplevel = toplevel;
}



/*! \private
 *  \brief Initialize GschemSelectionAdapter class
 *
 *  \param [in] klass The class for the GschemSelectionAdapter
 */
static void
gschem_selection_adapter_class_init (GschemSelectionAdapterClass *klass)
{
  G_OBJECT_CLASS (klass)->get_property = get_property;
  G_OBJECT_CLASS (klass)->set_property = set_property;

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_CAP_STYLE,
                                   g_param_spec_int ("cap-style",
                                                     "Cap Style",
                                                     "Cap Style",
                                                     G_MININT,
                                                     G_MAXINT,
                                                     NO_SELECTION,
                                                     (GParamFlags) (G_PARAM_READWRITE
                                                                    | G_PARAM_STATIC_STRINGS)));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_DASH_LENGTH,
                                   g_param_spec_int ("dash-length",
                                                     "Dash Length",
                                                     "Dash Length",
                                                     G_MININT,
                                                     G_MAXINT,
                                                     NO_SELECTION,
                                                     (GParamFlags) (G_PARAM_READWRITE
                                                                    | G_PARAM_STATIC_STRINGS)));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_DASH_SPACE,
                                   g_param_spec_int ("dash-space",
                                                     "Dash Space",
                                                     "Dash Space",
                                                     G_MININT,
                                                     G_MAXINT,
                                                     NO_SELECTION,
                                                     (GParamFlags) (G_PARAM_READWRITE
                                                                    | G_PARAM_STATIC_STRINGS)));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_FILL_ANGLE1,
                                   g_param_spec_int ("fill-angle1",
                                                     "Fill Angle 1",
                                                     "Fill Angle 1",
                                                     G_MININT,
                                                     G_MAXINT,
                                                     NO_SELECTION,
                                                     (GParamFlags) (G_PARAM_READWRITE
                                                                    | G_PARAM_STATIC_STRINGS)));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_FILL_ANGLE2,
                                   g_param_spec_int ("fill-angle2",
                                                     "Fill Angle 2",
                                                     "Fill Angle 2",
                                                     G_MININT,
                                                     G_MAXINT,
                                                     NO_SELECTION,
                                                     (GParamFlags) (G_PARAM_READWRITE
                                                                    | G_PARAM_STATIC_STRINGS)));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_FILL_PITCH1,
                                   g_param_spec_int ("fill-pitch1",
                                                     "Fill Pitch 1",
                                                     "Fill Pitch 1",
                                                     G_MININT,
                                                     G_MAXINT,
                                                     NO_SELECTION,
                                                     (GParamFlags) (G_PARAM_READWRITE
                                                                    | G_PARAM_STATIC_STRINGS)));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_FILL_PITCH2,
                                   g_param_spec_int ("fill-pitch2",
                                                     "Fill Pitch 2",
                                                     "Fill Pitch 2",
                                                     G_MININT,
                                                     G_MAXINT,
                                                     NO_SELECTION,
                                                     (GParamFlags) (G_PARAM_READWRITE
                                                                    | G_PARAM_STATIC_STRINGS)));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_FILL_TYPE,
                                   g_param_spec_int ("fill-type",
                                                     "Fill Type",
                                                     "Fill Type",
                                                     G_MININT,
                                                     G_MAXINT,
                                                     NO_SELECTION,
                                                     (GParamFlags) (G_PARAM_READWRITE
                                                                    | G_PARAM_STATIC_STRINGS)));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_FILL_WIDTH,
                                   g_param_spec_int ("fill-width",
                                                     "Fill Width",
                                                     "Fill Width",
                                                     G_MININT,
                                                     G_MAXINT,
                                                     NO_SELECTION,
                                                     (GParamFlags) (G_PARAM_READWRITE
                                                                    | G_PARAM_STATIC_STRINGS)));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_LINE_TYPE,
                                   g_param_spec_int ("line-type",
                                                     "Line Type",
                                                     "Line Type",
                                                     G_MININT,
                                                     G_MAXINT,
                                                     NO_SELECTION,
                                                     (GParamFlags) (G_PARAM_READWRITE
                                                                    | G_PARAM_STATIC_STRINGS)));


  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_LINE_WIDTH,
                                   g_param_spec_int ("line-width",
                                                     "Line Width",
                                                     "Line Width",
                                                     G_MININT,
                                                     G_MAXINT,
                                                     NO_SELECTION,
                                                     (GParamFlags) (G_PARAM_READWRITE
                                                                    | G_PARAM_STATIC_STRINGS)));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_OBJECT_COLOR,
                                   g_param_spec_int ("object-color",
                                                     "Object Color",
                                                     "Object Color",
                                                     G_MININT,
                                                     G_MAXINT,
                                                     NO_SELECTION,
                                                     (GParamFlags) (G_PARAM_READWRITE
                                                                    | G_PARAM_STATIC_STRINGS)));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_PIN_TYPE,
                                   g_param_spec_int ("pin-type",
                                                     "Pin Type",
                                                     "Pin Type",
                                                     G_MININT,
                                                     G_MAXINT,
                                                     NO_SELECTION,
                                                     (GParamFlags) (G_PARAM_READWRITE
                                                                    | G_PARAM_STATIC_STRINGS)));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_TEXT_ALIGNMENT,
                                   g_param_spec_int ("text-alignment",
                                                     "Text Alignment",
                                                     "Text Alignment",
                                                     G_MININT,
                                                     G_MAXINT,
                                                     NO_SELECTION,
                                                     (GParamFlags) (G_PARAM_READWRITE
                                                                    | G_PARAM_STATIC_STRINGS)));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_TEXT_COLOR,
                                   g_param_spec_int ("text-color",
                                                     "Text Color",
                                                     "Text Color",
                                                     G_MININT,
                                                     G_MAXINT,
                                                     NO_SELECTION,
                                                     (GParamFlags) (G_PARAM_READWRITE
                                                                    | G_PARAM_STATIC_STRINGS)));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_TEXT_ROTATION,
                                   g_param_spec_int ("text-rotation",
                                                     "Text Rotation",
                                                     "Text Rotation",
                                                     G_MININT,
                                                     G_MAXINT,
                                                     NO_SELECTION,
                                                     (GParamFlags) (G_PARAM_READWRITE
                                                                    | G_PARAM_STATIC_STRINGS)));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_TEXT_SIZE,
                                   g_param_spec_int ("text-size",
                                                     "Text Size",
                                                     "Text Size",
                                                     G_MININT,
                                                     G_MAXINT,
                                                     NO_SELECTION,
                                                     (GParamFlags) (G_PARAM_READWRITE
                                                                    | G_PARAM_STATIC_STRINGS)));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_TEXT_STRING,
                                   g_param_spec_string ("text-string",
                                                        "Text String",
                                                        "Text String",
                                                        NULL,
                                                        (GParamFlags) (G_PARAM_READWRITE
                                                                       | G_PARAM_STATIC_STRINGS)));

  /* This signal indicates changes to the selection that requires the undo
   * manager save the state.
   */

  g_signal_new ("handle-undo",                    /* signal_name  */
                G_OBJECT_CLASS_TYPE (klass),      /* itype        */
                (GSignalFlags) 0,                 /* signal_flags */
                0,                                /* class_offset */
                NULL,                             /* accumulator  */
                NULL,                             /* accu_data    */
                g_cclosure_marshal_VOID__VOID,    /* c_marshaller */
                G_TYPE_NONE,                      /* return_type  */
                0                                 /* n_params     */
                );
}


/*! \private
 *  \brief Get an iterator for objects in the selection
 *
 *  \param [in] adapter this adapter
 *  \return an iterator for the selection or NULL in none
 */
static GList*
get_selection_iter (GschemSelectionAdapter *adapter)
{
  GList *iter = NULL;
  LeptonSelection *selection = gschem_selection_adapter_get_selection (adapter);

  if (selection != NULL) {
    iter = lepton_list_get_glist (selection);
  }

  return iter;
}


/*! \private
 *  \brief Get a property
 *
 *  \param [in]     object
 *  \param [in]     param_id
 *  \param [in,out] value
 *  \param [in]     pspec
 */
static void
get_property (GObject *object, guint param_id, GValue *value, GParamSpec *pspec)
{
  GschemSelectionAdapter *adapter = GSCHEM_SELECTION_ADAPTER (object);

  switch (param_id) {
    case PROP_CAP_STYLE:
      g_value_set_int (value, gschem_selection_adapter_get_cap_style (adapter));
      break;

    case PROP_DASH_LENGTH:
      g_value_set_int (value, gschem_selection_adapter_get_dash_length (adapter));
      break;

    case PROP_DASH_SPACE:
      g_value_set_int (value, gschem_selection_adapter_get_dash_space (adapter));
      break;

    case PROP_FILL_ANGLE1:
      g_value_set_int (value, gschem_selection_adapter_get_fill_angle1 (adapter));
      break;

    case PROP_FILL_ANGLE2:
      g_value_set_int (value, gschem_selection_adapter_get_fill_angle2 (adapter));
      break;

    case PROP_FILL_PITCH1:
      g_value_set_int (value, gschem_selection_adapter_get_fill_pitch1 (adapter));
      break;

    case PROP_FILL_PITCH2:
      g_value_set_int (value, gschem_selection_adapter_get_fill_pitch2 (adapter));
      break;

    case PROP_FILL_TYPE:
      g_value_set_int (value, gschem_selection_adapter_get_fill_type (adapter));
      break;

    case PROP_FILL_WIDTH:
      g_value_set_int (value, gschem_selection_adapter_get_fill_width (adapter));
      break;

    case PROP_LINE_TYPE:
      g_value_set_int (value, gschem_selection_adapter_get_line_type (adapter));
      break;

    case PROP_LINE_WIDTH:
      g_value_set_int (value, gschem_selection_adapter_get_line_width (adapter));
      break;

    case PROP_OBJECT_COLOR:
      g_value_set_int (value, gschem_selection_adapter_get_object_color (adapter));
      break;

    case PROP_PIN_TYPE:
      g_value_set_int (value, gschem_selection_adapter_get_pin_type (adapter));
      break;

    case PROP_TEXT_ALIGNMENT:
      g_value_set_int (value, gschem_selection_adapter_get_text_alignment (adapter));
      break;

    case PROP_TEXT_COLOR:
      g_value_set_int (value, gschem_selection_adapter_get_text_color (adapter));
      break;

    case PROP_TEXT_ROTATION:
      g_value_set_int (value, gschem_selection_adapter_get_text_rotation (adapter));
      break;

    case PROP_TEXT_SIZE:
      g_value_set_int (value, gschem_selection_adapter_get_text_size (adapter));
      break;

    case PROP_TEXT_STRING:
      g_value_set_string (value, gschem_selection_adapter_get_text_string (adapter));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
  }
}



/*! \brief Initialize #GschemSelectionAdapter instance
 *
 *  \param [in,out] adapter The #GschemSelectionAdapter instance.
 */
static void
gschem_selection_adapter_init (GschemSelectionAdapter *adapter)
{
}



/*! \private
 *  \brief Signal handler for when the selection changes
 *
 *  \par Function Description
 *  This function gets called when items are added or removed from the
 *  selection.
 *
 *  \param [in] selection The selection that changed
 *  \param [in] adapter   This adapter
 */
static void
selection_changed (LeptonList *selection,
                   GschemSelectionAdapter *adapter)
{
  g_return_if_fail (adapter != NULL);
  g_return_if_fail (selection != NULL);
  g_return_if_fail (adapter->selection == selection);

  g_object_notify (G_OBJECT (adapter), "cap-style");
  g_object_notify (G_OBJECT (adapter), "dash-length");
  g_object_notify (G_OBJECT (adapter), "dash-space");
  g_object_notify (G_OBJECT (adapter), "fill-angle1");
  g_object_notify (G_OBJECT (adapter), "fill-angle2");
  g_object_notify (G_OBJECT (adapter), "fill-pitch1");
  g_object_notify (G_OBJECT (adapter), "fill-pitch2");
  g_object_notify (G_OBJECT (adapter), "fill-type");
  g_object_notify (G_OBJECT (adapter), "fill-width");
  g_object_notify (G_OBJECT (adapter), "line-type");
  g_object_notify (G_OBJECT (adapter), "line-width");
  g_object_notify (G_OBJECT (adapter), "object-color");
  g_object_notify (G_OBJECT (adapter), "pin-type");
  g_object_notify (G_OBJECT (adapter), "text-alignment");
  g_object_notify (G_OBJECT (adapter), "text-color");
  g_object_notify (G_OBJECT (adapter), "text-rotation");
  g_object_notify (G_OBJECT (adapter), "text-size");
  g_object_notify (G_OBJECT (adapter), "text-string");
}



/*! \brief Set a property
 *
 *  \param [in,out] object
 *  \param [in]     param_id
 *  \param [in]     value
 *  \param [in]     pspec
 */
static void
set_property (GObject *object, guint param_id, const GValue *value, GParamSpec *pspec)
{
  GschemSelectionAdapter *adapter = GSCHEM_SELECTION_ADAPTER (object);

  switch (param_id) {
    case PROP_CAP_STYLE:
      gschem_selection_adapter_set_cap_style (adapter, g_value_get_int (value));
      break;

    case PROP_DASH_LENGTH:
      gschem_selection_adapter_set_dash_length (adapter, g_value_get_int (value));
      break;

    case PROP_DASH_SPACE:
      gschem_selection_adapter_set_dash_space (adapter, g_value_get_int (value));
      break;

    case PROP_FILL_ANGLE1:
      gschem_selection_adapter_set_fill_angle1 (adapter, g_value_get_int (value));
      break;

    case PROP_FILL_ANGLE2:
      gschem_selection_adapter_set_fill_angle2 (adapter, g_value_get_int (value));
      break;

    case PROP_FILL_PITCH1:
      gschem_selection_adapter_set_fill_pitch1 (adapter, g_value_get_int (value));
      break;

    case PROP_FILL_PITCH2:
      gschem_selection_adapter_set_fill_pitch2 (adapter, g_value_get_int (value));
      break;

    case PROP_FILL_TYPE:
      gschem_selection_adapter_set_fill_type (adapter, g_value_get_int (value));
      break;

    case PROP_FILL_WIDTH:
      gschem_selection_adapter_set_fill_width (adapter, g_value_get_int (value));
      break;

    case PROP_LINE_TYPE:
      gschem_selection_adapter_set_line_type (adapter, g_value_get_int (value));
      break;

    case PROP_LINE_WIDTH:
      gschem_selection_adapter_set_line_width (adapter, g_value_get_int (value));
      break;

    case PROP_OBJECT_COLOR:
      gschem_selection_adapter_set_object_color (adapter, g_value_get_int (value));
      break;

    case PROP_PIN_TYPE:
      gschem_selection_adapter_set_pin_type (adapter, g_value_get_int (value));
      break;

    case PROP_TEXT_ALIGNMENT:
      gschem_selection_adapter_set_text_alignment (adapter, g_value_get_int (value));
      break;

    case PROP_TEXT_COLOR:
      gschem_selection_adapter_set_text_color (adapter, g_value_get_int (value));
      break;

    case PROP_TEXT_ROTATION:
      gschem_selection_adapter_set_text_rotation (adapter, g_value_get_int (value));
      break;

    case PROP_TEXT_SIZE:
      gschem_selection_adapter_set_text_size (adapter, g_value_get_int (value));
      break;

    /* Currently, the text string cannot be set using the gobject property
     * system
     *
     * case PROP_TEXT_STRING:
     *   gschem_selection_adapter_set_text_string (adapter, g_value_get_string (value));
     *   break;
     */

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
  }
}
