/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
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
/*!
 * \file gschem_selection_adapter.c
 *
 * \brief
 */

#include <config.h>

#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "gschem.h"
#include <gdk/gdkkeysyms.h>

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
  PROP_OBJECT_COLOR
};



static void
get_property (GObject *object, guint param_id, GValue *value, GParamSpec *pspec);

static void
gschem_selection_adapter_class_init (GschemSelectionAdapterClass *klass);

static void
gschem_selection_adapter_init (GschemSelectionAdapter *adapter);

static void
selection_changed (GedaList *selection, GschemSelectionAdapter *adapter);

static void
set_property (GObject *object, guint param_id, const GValue *value, GParamSpec *pspec);



/*! \brief Get a property
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

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
  }
}



/*! \brief Initialize GschemSelectionAdapter class
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
                                                     G_PARAM_READWRITE | G_PARAM_STATIC_STRINGS));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_DASH_LENGTH,
                                   g_param_spec_int ("dash-length",
                                                     "Dash Length",
                                                     "Dash Length",
                                                     G_MININT,
                                                     G_MAXINT,
                                                     NO_SELECTION,
                                                     G_PARAM_READWRITE | G_PARAM_STATIC_STRINGS));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_DASH_SPACE,
                                   g_param_spec_int ("dash-space",
                                                     "Dash Space",
                                                     "Dash Space",
                                                     G_MININT,
                                                     G_MAXINT,
                                                     NO_SELECTION,
                                                     G_PARAM_READWRITE | G_PARAM_STATIC_STRINGS));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_FILL_ANGLE1,
                                   g_param_spec_int ("fill-angle1",
                                                     "Fill Angle 1",
                                                     "Fill Angle 1",
                                                     G_MININT,
                                                     G_MAXINT,
                                                     NO_SELECTION,
                                                     G_PARAM_READWRITE | G_PARAM_STATIC_STRINGS));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_FILL_ANGLE2,
                                   g_param_spec_int ("fill-angle2",
                                                     "Fill Angle 2",
                                                     "Fill Angle 2",
                                                     G_MININT,
                                                     G_MAXINT,
                                                     NO_SELECTION,
                                                     G_PARAM_READWRITE | G_PARAM_STATIC_STRINGS));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_FILL_PITCH1,
                                   g_param_spec_int ("fill-pitch1",
                                                     "Fill Pitch 1",
                                                     "Fill Pitch 1",
                                                     G_MININT,
                                                     G_MAXINT,
                                                     NO_SELECTION,
                                                     G_PARAM_READWRITE | G_PARAM_STATIC_STRINGS));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_FILL_PITCH2,
                                   g_param_spec_int ("fill-pitch2",
                                                     "Fill Pitch 2",
                                                     "Fill Pitch 2",
                                                     G_MININT,
                                                     G_MAXINT,
                                                     NO_SELECTION,
                                                     G_PARAM_READWRITE | G_PARAM_STATIC_STRINGS));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_FILL_TYPE,
                                   g_param_spec_int ("fill-type",
                                                     "Fill Type",
                                                     "Fill Type",
                                                     G_MININT,
                                                     G_MAXINT,
                                                     NO_SELECTION,
                                                     G_PARAM_READWRITE | G_PARAM_STATIC_STRINGS));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_FILL_WIDTH,
                                   g_param_spec_int ("fill-width",
                                                     "Fill Width",
                                                     "Fill Width",
                                                     G_MININT,
                                                     G_MAXINT,
                                                     NO_SELECTION,
                                                     G_PARAM_READWRITE | G_PARAM_STATIC_STRINGS));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_LINE_TYPE,
                                   g_param_spec_int ("line-type",
                                                     "Line Type",
                                                     "Line Type",
                                                     G_MININT,
                                                     G_MAXINT,
                                                     NO_SELECTION,
                                                     G_PARAM_READWRITE | G_PARAM_STATIC_STRINGS));


  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_LINE_WIDTH,
                                   g_param_spec_int ("line-width",
                                                     "Line Width",
                                                     "Line Width",
                                                     G_MININT,
                                                     G_MAXINT,
                                                     NO_SELECTION,
                                                     G_PARAM_READWRITE | G_PARAM_STATIC_STRINGS));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_OBJECT_COLOR,
                                   g_param_spec_int ("object-color",
                                                     "Object Color",
                                                     "Object Color",
                                                     G_MININT,
                                                     G_MAXINT,
                                                     NO_SELECTION,
                                                     G_PARAM_READWRITE | G_PARAM_STATIC_STRINGS));

  /* This signal indicates changes to the selection that requires the undo
   * manager save the state.
   */

  g_signal_new ("handle-undo",                    /* signal_name  */
                G_OBJECT_CLASS_TYPE (klass),      /* itype        */
                0,                                /* signal_flags */
                0,                                /* class_offset */
                NULL,                             /* accumulator  */
                NULL,                             /* accu_data    */
                g_cclosure_marshal_VOID__VOID,    /* c_marshaller */
                G_TYPE_NONE,                      /* return_type  */
                0                                 /* n_params     */
                );
}



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
  GList *iter;

  g_return_val_if_fail (adapter != NULL, NO_SELECTION);

  iter = geda_list_get_glist (gschem_selection_adapter_get_selection (adapter));

  while (iter != NULL) {
    OBJECT *object = (OBJECT*) iter->data;
    gboolean success;
    OBJECT_END temp_cap_style;
    gint temp_dash_length;
    gint temp_dash_space;
    OBJECT_TYPE temp_line_type;
    gint temp_line_width;

    success = o_get_line_options (object,
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
  GList *iter;

  g_return_val_if_fail (adapter != NULL, NO_SELECTION);

  iter = geda_list_get_glist (gschem_selection_adapter_get_selection (adapter));

  while (iter != NULL) {
    OBJECT *object = (OBJECT*) iter->data;
    gboolean success;
    OBJECT_END temp_cap_style;
    gint temp_dash_length;
    gint temp_dash_space;
    OBJECT_TYPE temp_line_type;
    gint temp_line_width;

    success = o_get_line_options (object,
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
  GList *iter;

  g_return_val_if_fail (adapter != NULL, NO_SELECTION);

  iter = geda_list_get_glist (gschem_selection_adapter_get_selection (adapter));

  while (iter != NULL) {
    OBJECT *object = (OBJECT*) iter->data;
    gboolean success;
    OBJECT_END temp_cap_style;
    gint temp_dash_length;
    gint temp_dash_space;
    OBJECT_TYPE temp_line_type;
    gint temp_line_width;

    success = o_get_line_options (object,
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
  GList *iter;

  g_return_val_if_fail (adapter != NULL, NO_SELECTION);

  iter = geda_list_get_glist (gschem_selection_adapter_get_selection (adapter));

  while (iter != NULL) {
    OBJECT *object = (OBJECT*) iter->data;
    gboolean success;
    gint temp_angle1;
    gint temp_angle2;
    OBJECT_FILLING temp_fill_type;
    gint temp_pitch1;
    gint temp_pitch2;
    gint temp_width;

    success = o_get_fill_options(object,
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
  GList *iter;

  g_return_val_if_fail (adapter != NULL, NO_SELECTION);

  iter = geda_list_get_glist (gschem_selection_adapter_get_selection (adapter));

  while (iter != NULL) {
    OBJECT *object = (OBJECT*) iter->data;
    gboolean success;
    gint temp_angle1;
    gint temp_angle2;
    OBJECT_FILLING temp_fill_type;
    gint temp_pitch1;
    gint temp_pitch2;
    gint temp_width;

    success = o_get_fill_options(object,
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
  GList *iter;

  g_return_val_if_fail (adapter != NULL, NO_SELECTION);

  iter = geda_list_get_glist (gschem_selection_adapter_get_selection (adapter));

  while (iter != NULL) {
    OBJECT *object = (OBJECT*) iter->data;
    gboolean success;
    gint temp_angle1;
    gint temp_angle2;
    OBJECT_FILLING temp_fill_type;
    gint temp_pitch1;
    gint temp_pitch2;
    gint temp_width;

    success = o_get_fill_options(object,
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
  GList *iter;

  g_return_val_if_fail (adapter != NULL, NO_SELECTION);

  iter = geda_list_get_glist (gschem_selection_adapter_get_selection (adapter));

  while (iter != NULL) {
    OBJECT *object = (OBJECT*) iter->data;
    gboolean success;
    gint temp_angle1;
    gint temp_angle2;
    OBJECT_FILLING temp_fill_type;
    gint temp_pitch1;
    gint temp_pitch2;
    gint temp_width;

    success = o_get_fill_options(object,
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
  GList *iter;

  g_return_val_if_fail (adapter != NULL, NO_SELECTION);

  iter = geda_list_get_glist (gschem_selection_adapter_get_selection (adapter));

  while (iter != NULL) {
    OBJECT *object = (OBJECT*) iter->data;
    gboolean success;
    gint temp_angle1;
    gint temp_angle2;
    OBJECT_FILLING temp_fill_type;
    gint temp_pitch1;
    gint temp_pitch2;
    gint temp_width;

    success = o_get_fill_options(object,
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
  GList *iter;

  g_return_val_if_fail (adapter != NULL, NO_SELECTION);

  iter = geda_list_get_glist (gschem_selection_adapter_get_selection (adapter));

  while (iter != NULL) {
    OBJECT *object = (OBJECT*) iter->data;
    gboolean success;
    gint temp_angle1;
    gint temp_angle2;
    OBJECT_FILLING temp_fill_type;
    gint temp_pitch1;
    gint temp_pitch2;
    gint temp_width;

    success = o_get_fill_options(object,
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
  GList *iter;

  g_return_val_if_fail (adapter != NULL, NO_SELECTION);

  iter = geda_list_get_glist (gschem_selection_adapter_get_selection (adapter));

  while (iter != NULL) {
    OBJECT *object = (OBJECT*) iter->data;
    gboolean success;
    OBJECT_END temp_cap_style;
    gint temp_dash_length;
    gint temp_dash_space;
    OBJECT_TYPE temp_line_type;
    gint temp_line_width;

    success = o_get_line_options (object,
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
  GList *iter;

  g_return_val_if_fail (adapter != NULL, NO_SELECTION);

  iter = geda_list_get_glist (gschem_selection_adapter_get_selection (adapter));

  while (iter != NULL) {
    OBJECT *object = (OBJECT*) iter->data;
    gboolean success;
    OBJECT_END temp_cap_style;
    gint temp_dash_length;
    gint temp_dash_space;
    OBJECT_TYPE temp_line_type;
    gint temp_line_width;

    success = o_get_line_options (object,
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
  GList *iter;

  g_return_val_if_fail (adapter != NULL, NO_SELECTION);

  iter = geda_list_get_glist (gschem_selection_adapter_get_selection (adapter));

  while (iter != NULL) {
    OBJECT* object = (OBJECT *) iter->data;
    iter = g_list_next (iter);
    if ((object != NULL) && (
        (object->type == OBJ_ARC)    ||
        (object->type == OBJ_BOX)    ||
        (object->type == OBJ_CIRCLE) ||
        (object->type == OBJ_LINE)   ||
        (object->type == OBJ_PATH)   ||
        (object->type == OBJ_TEXT))) {
      color = object->color;
      break;
    }
  }

  /* Check if all other objects have the same properties */

  while (iter != NULL) {
    OBJECT* object = (OBJECT *) iter->data;
    if ((object != NULL) && (
        (object->type == OBJ_ARC)    ||
        (object->type == OBJ_BOX)    ||
        (object->type == OBJ_CIRCLE) ||
        (object->type == OBJ_LINE)   ||
        (object->type == OBJ_PATH)   ||
        (object->type == OBJ_TEXT))) {
      if (color != object->color) {
        color = MULTIPLE_VALUES;
      }
    }
    iter = g_list_next (iter);
  }

  return color;
}



/*! \brief Get the selection associated with this adapter
 *
 *  \param [in] adapter This adapter
 *  \return The libgeda selection
 */
SELECTION*
gschem_selection_adapter_get_selection (GschemSelectionAdapter *adapter)
{
  g_return_val_if_fail (adapter != NULL, NULL);

  return adapter->selection;
}



/*! \brief Get the libgeda toplevel associated with this adapter
 *
 *  \param [in] adapter This adapter
 *  \return The libgeda toplevel
 */
TOPLEVEL*
gschem_selection_adapter_get_toplevel (GschemSelectionAdapter *adapter)
{
  g_return_val_if_fail (adapter != NULL, NULL);

  return adapter->toplevel;
}



/*! \brief Get/register GschemSelection type.
 */
GType
gschem_selection_adapter_get_type ()
{
  static GType type = 0;

  if (type == 0) {
    static const GTypeInfo info = {
      sizeof(GschemSelectionAdapterClass),
      NULL,                                                    /* base_init */
      NULL,                                                    /* base_finalize */
      (GClassInitFunc) gschem_selection_adapter_class_init,
      NULL,                                                    /* class_finalize */
      NULL,                                                    /* class_data */
      sizeof(GschemSelectionAdapter),
      0,                                                       /* n_preallocs */
      (GInstanceInitFunc) gschem_selection_adapter_init,
    };

    type = g_type_register_static (G_TYPE_OBJECT, "GschemSelectionAdapter", &info, 0);
  }

  return type;
}



/*! \brief Initialize GschemSelection instance
 *
 *  \param [in,out] selection
 */
static void
gschem_selection_adapter_init (GschemSelectionAdapter *adapter)
{
}



/*! \brief Create a new instanceof the GschemSelectionAdapter
 *
 *  \return A new instanceof the GschemSelectionAdapter
 */
GschemSelectionAdapter*
gschem_selection_adapter_new ()
{
  return GSCHEM_SELECTION_ADAPTER (g_object_new (GSCHEM_TYPE_SELECTION_ADAPTER, NULL));
}



/*! \brief Set the fill angle 1 in the selection
 *
 *  \param [in] selection
 *  \param [in] angle
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

  iter = geda_list_get_glist (adapter->selection);

  while (iter != NULL) {
    OBJECT *object = (OBJECT*) iter->data;
    gboolean success;
    gint temp_angle1;
    gint temp_angle2;
    OBJECT_FILLING temp_fill_type;
    gint temp_pitch1;
    gint temp_pitch2;
    gint temp_width;

    success = o_get_fill_options(object,
                                 &temp_fill_type,
                                 &temp_width,
                                 &temp_pitch1,
                                 &temp_angle1,
                                 &temp_pitch2,
                                 &temp_angle2);

    if (success) {
      o_set_fill_options (adapter->toplevel,
                          object,
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
 *  \param [in] selection
 *  \param [in] angle
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

  iter = geda_list_get_glist (adapter->selection);

  while (iter != NULL) {
    OBJECT *object = (OBJECT*) iter->data;
    gboolean success;
    gint temp_angle1;
    gint temp_angle2;
    OBJECT_FILLING temp_fill_type;
    gint temp_pitch1;
    gint temp_pitch2;
    gint temp_width;

    success = o_get_fill_options(object,
                                 &temp_fill_type,
                                 &temp_width,
                                 &temp_pitch1,
                                 &temp_angle1,
                                 &temp_pitch2,
                                 &temp_angle2);

    if (success) {
      o_set_fill_options (adapter->toplevel,
                          object,
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
 *  \param [in] selection
 *  \param [in] angle
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

  iter = geda_list_get_glist (adapter->selection);

  while (iter != NULL) {
    OBJECT *object = (OBJECT*) iter->data;
    gboolean success;
    gint temp_angle1;
    gint temp_angle2;
    OBJECT_FILLING temp_fill_type;
    gint temp_pitch1;
    gint temp_pitch2;
    gint temp_width;

    success = o_get_fill_options(object,
                                 &temp_fill_type,
                                 &temp_width,
                                 &temp_pitch1,
                                 &temp_angle1,
                                 &temp_pitch2,
                                 &temp_angle2);

    if (success) {
      o_set_fill_options (adapter->toplevel,
                          object,
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
 *  \param [in] selection
 *  \param [in] pitch
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

  iter = geda_list_get_glist (adapter->selection);

  while (iter != NULL) {
    OBJECT *object = (OBJECT*) iter->data;
    gboolean success;
    gint temp_angle1;
    gint temp_angle2;
    OBJECT_FILLING temp_fill_type;
    gint temp_pitch1;
    gint temp_pitch2;
    gint temp_width;

    success = o_get_fill_options(object,
                                 &temp_fill_type,
                                 &temp_width,
                                 &temp_pitch1,
                                 &temp_angle1,
                                 &temp_pitch2,
                                 &temp_angle2);

    if (success) {
      o_set_fill_options (adapter->toplevel,
                          object,
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
 *  \param [in] selection
 *  \param [in] line_type
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

  iter = geda_list_get_glist (adapter->selection);

  while (iter != NULL) {
    OBJECT *object = (OBJECT*) iter->data;
    gboolean success;
    gint temp_angle1;
    gint temp_angle2;
    OBJECT_FILLING temp_fill_type;
    gint temp_pitch1;
    gint temp_pitch2;
    gint temp_width;

    success = o_get_fill_options(object,
                                 &temp_fill_type,
                                 &temp_width,
                                 &temp_pitch1,
                                 &temp_angle1,
                                 &temp_pitch2,
                                 &temp_angle2);

    if (success) {
      o_set_fill_options (adapter->toplevel,
                          object,
                          fill_type,
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
 *  \param [in] selection
 *  \param [in] fill_width
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

  iter = geda_list_get_glist (adapter->selection);

  while (iter != NULL) {
    OBJECT *object = (OBJECT*) iter->data;
    gboolean success;
    gint temp_angle1;
    gint temp_angle2;
    OBJECT_FILLING temp_fill_type;
    gint temp_pitch1;
    gint temp_pitch2;
    gint temp_width;

    success = o_get_fill_options(object,
                                 &temp_fill_type,
                                 &temp_width,
                                 &temp_pitch1,
                                 &temp_angle1,
                                 &temp_pitch2,
                                 &temp_angle2);

    if (success) {
      o_set_fill_options (adapter->toplevel,
                          object,
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
 *  \param [in] selection
 *  \param [in] line_type
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

  iter = geda_list_get_glist (adapter->selection);

  while (iter != NULL) {
    OBJECT *object = (OBJECT*) iter->data;
    gboolean success;
    OBJECT_END temp_cap_style;
    int temp_dash_length;
    int temp_dash_space;
    OBJECT_TYPE temp_line_type;
    int temp_line_width;

    success = o_get_line_options (object,
                                  &temp_cap_style,
                                  &temp_line_type,
                                  &temp_line_width,
                                  &temp_dash_length,
                                  &temp_dash_space);

    if (success) {
      o_set_line_options (adapter->toplevel,
                          object,
                          temp_cap_style,
                          line_type,
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
 *  \param [in] selection
 *  \param [in] line_width
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

  iter = geda_list_get_glist (adapter->selection);

  while (iter != NULL) {
    OBJECT *object = (OBJECT*) iter->data;
    gboolean success;
    OBJECT_END temp_cap_style;
    int temp_dash_length;
    int temp_dash_space;
    OBJECT_TYPE temp_line_type;
    int temp_line_width;

    success = o_get_line_options (object,
                                  &temp_cap_style,
                                  &temp_line_type,
                                  &temp_line_width,
                                  &temp_dash_length,
                                  &temp_dash_space);

    if (success) {
      o_set_line_options (adapter->toplevel,
                          object,
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
 *  \param [in] selection
 *  \param [in] dash_length
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

  iter = geda_list_get_glist (adapter->selection);

  while (iter != NULL) {
    OBJECT *object = (OBJECT*) iter->data;
    gboolean success;
    OBJECT_END temp_cap_style;
    int temp_dash_length;
    int temp_dash_space;
    OBJECT_TYPE temp_line_type;
    int temp_line_width;

    success = o_get_line_options (object,
                                  &temp_cap_style,
                                  &temp_line_type,
                                  &temp_line_width,
                                  &temp_dash_length,
                                  &temp_dash_space);

    if (success) {
      o_set_line_options (adapter->toplevel,
                          object,
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
 *  \param [in] selection
 *  \param [in] dash_space
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

  iter = geda_list_get_glist (adapter->selection);

  while (iter != NULL) {
    OBJECT *object = (OBJECT*) iter->data;
    gboolean success;
    OBJECT_END temp_cap_style;
    int temp_dash_length;
    int temp_dash_space;
    OBJECT_TYPE temp_line_type;
    int temp_line_width;

    success = o_get_line_options (object,
                                  &temp_cap_style,
                                  &temp_line_type,
                                  &temp_line_width,
                                  &temp_dash_length,
                                  &temp_dash_space);

    if (success) {
      o_set_line_options (adapter->toplevel,
                          object,
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

  iter = geda_list_get_glist (adapter->selection);

  while (iter != NULL) {
    OBJECT *object = (OBJECT*) iter->data;
    gboolean success;
    OBJECT_END temp_cap_style;
    int temp_dash_length;
    int temp_dash_space;
    OBJECT_TYPE temp_line_type;
    int temp_line_width;

    success = o_get_line_options (object,
                                  &temp_cap_style,
                                  &temp_line_type,
                                  &temp_line_width,
                                  &temp_dash_length,
                                  &temp_dash_space);

    if (success) {
      o_set_line_options (adapter->toplevel,
                          object,
                          cap_style,
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
  GList *iter;

  g_return_if_fail (adapter != NULL);
  g_return_if_fail (color >= 0);
  g_return_if_fail (color < MAX_COLORS);

  iter = geda_list_get_glist (adapter->selection);

  while (iter != NULL) {
    OBJECT *object = (OBJECT*) iter->data;

    o_set_color (adapter->toplevel, object, color);

    iter = g_list_next (iter);
  }

  g_object_notify (G_OBJECT (adapter), "object-color");

  g_signal_emit_by_name (adapter, "handle-undo");
}



/*! \brief Set the selection associated with this adapter
 *
 *  \param [in] adapter
 *  \param [in] selection
 */
void
gschem_selection_adapter_set_selection (GschemSelectionAdapter *adapter, SELECTION *selection)
{
  g_return_if_fail (adapter != NULL);

   if (adapter->selection != NULL) {
    g_signal_handlers_disconnect_by_func (adapter->selection,
                                          G_CALLBACK (selection_changed),
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
}



/*! \brief Set the toplevel associated with this adapter
 *
 *  \param [in] adapter
 *  \param [in] toplevel
 */
void
gschem_selection_adapter_set_toplevel (GschemSelectionAdapter *adapter, TOPLEVEL *toplevel)
{
  g_return_if_fail (adapter != NULL);

  adapter->toplevel = toplevel;
}



/*! \brief Signal handler for when the selection changes
 *
 *  \par Function Description
 *  This function gets called when items are added or removed from the
 *  selection.
 *
 *  \param [in] selection The selection that changed
 *  \param [in] adapter   This adapter
 */
static void
selection_changed (GedaList *selection, GschemSelectionAdapter *adapter)
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

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
  }
}
