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

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif



static void
gschem_selection_adapter_class_init (GschemSelectionAdapterClass *klasse);

static void
gschem_selection_adapter_init (GschemSelectionAdapter *adapter);

static void
selection_changed (GedaList *selection, GschemSelectionAdapter *adapter);



/*! \brief Initialize GschemSelectionAdapter class
 *
 *  \param [in] klasse The class for the GschemSelectionAdapter
 */
static void
gschem_selection_adapter_class_init (GschemSelectionAdapterClass *klasse)
{
  g_signal_new ("changed",                        /* signal_name  */
                G_OBJECT_CLASS_TYPE (klasse),     /* itype        */
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
 *  \return The cap style. If there are not objects with a cap style, or
 *  multiple objects with a different cap style, this function returns -1.
 */
int
gschem_selection_adapter_get_cap_style (GschemSelectionAdapter *adapter)
{
  gint cap_style = -1;
  GList *iter;

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
        cap_style = -2;
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
 *  \return The dash_length. If there are not objects with a dash_length, or
 *  multiple objects with a different dash_length, this function returns -1.
 */
int
gschem_selection_adapter_get_dash_length (GschemSelectionAdapter *adapter)
{
  gint dash_length = -1;
  GList *iter;

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
        dash_length = -2;
        break;
      }
    }

    iter = g_list_next (iter);
  }

  return dash_length;
}



/*! \brief Get the dash_space from the selection
 *
 *  \param [in] adapter This adapter
 *  \return The dash_space. If there are not objects with a dash_space, or
 *  multiple objects with a different dash_space, this function returns -1.
 */
int
gschem_selection_adapter_get_dash_space (GschemSelectionAdapter *adapter)
{
  gint dash_space = -1;
  GList *iter;

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
        dash_space = -2;
        break;
      }
    }

    iter = g_list_next (iter);
  }

  return dash_space;
}



/*! \brief Get the line_type from the selection
 *
 *  \param [in] adapter This adapter
 *  \return The line_type. If there are not objects with a line_type, or
 *  multiple objects with a different line_type, this function returns -1.
 */
int
gschem_selection_adapter_get_line_type (GschemSelectionAdapter *adapter)
{
  gint line_type = -1;
  GList *iter;

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
        line_type = -2;
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
 *  \return The line width. If there are not objects with a line width, or
 *  multiple objects with a different line width, this function returns -1.
 */
int
gschem_selection_adapter_get_line_width (GschemSelectionAdapter *adapter)
{
  gint line_width = -1;
  GList *iter;

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
        line_width = -2;
        break;
      }
    }

    iter = g_list_next (iter);
  }

  return line_width;
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

  g_signal_emit_by_name (adapter, "changed");
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

  g_signal_emit_by_name (adapter, "changed");
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

  g_signal_emit_by_name (adapter, "changed");
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

  g_signal_emit_by_name (adapter, "changed");
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

  g_signal_emit_by_name (adapter, "changed");
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

  g_signal_emit_by_name (adapter, "changed");
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

  g_signal_emit_by_name (adapter, "changed");
}
