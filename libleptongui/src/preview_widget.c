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
 *  \file preview_widget.c
 *
 *  \brief A widget for viewing a symbol or schematic
 */

#include <config.h>

#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "gschem.h"


struct _SchematicPreviewClass
{
  SchematicCanvasClass parent_class;
};

struct _SchematicPreview
{
  SchematicCanvas parent_instance;

  SchematicWindow *window;

  gchar *filename;
  gchar *buffer;

  gboolean active;
};


enum {
  PROP_FILENAME=1,
  PROP_BUFFER,
  PROP_ACTIVE
};

G_DEFINE_TYPE (SchematicPreview, schematic_preview, SCHEMATIC_TYPE_CANVAS);

static void schematic_preview_set_property (GObject *object,
                                            guint property_id,
                                            const GValue *value,
                                            GParamSpec *pspec);
static void schematic_preview_get_property (GObject *object,
                                            guint property_id,
                                            GValue *value,
                                            GParamSpec *pspec);
static void schematic_preview_dispose (GObject *self);
static void schematic_preview_finalize (GObject *self);


/*! \brief get the filename for the current page
 */
static const char*
schematic_preview_get_page_filename (SchematicPreview *preview)
{
  LeptonPage *page = schematic_canvas_get_page (SCHEMATIC_CANVAS (preview));

  g_return_val_if_fail (page != NULL, "");

  return lepton_page_get_filename (page);
}


/*! \brief Completes initialitation of the widget after realization.
 *  \par Function Description
 *  This function terminates the initialization of the preview widget
 *  after it has been realized.
 *
 *  \param [in] widget    The preview widget.
 *  \param [in] user_data Unused user data.
 */
void
schematic_preview_callback_realize (GtkWidget *widget,
                                    gpointer user_data)
{
  g_return_if_fail (widget != NULL);

  gtk_widget_grab_focus (widget);
  schematic_canvas_zoom_extents (SCHEMATIC_CANVAS (widget), NULL);
}


/*! \brief Handles the press on a mouse button.
 *  \par Function Description
 *  It handles the user inputs.
 *
 *  Three action are available: zoom in, pan and zoom out on preview display.
 *
 *  \param [in] widget    The preview widget.
 *  \param [in] event     The event structure.
 *  \param [in] user_data Unused user data.
 *  \returns FALSE to propagate the event further.
 */
gboolean
schematic_preview_callback_button_press (GtkWidget *widget,
                                         GdkEventButton *event,
                                         gpointer user_data)
{
  SchematicPreview *preview = SCHEMATIC_PREVIEW (widget);
  SchematicWindow *preview_w_current = preview->window;
  gint wx, wy;

  if (!preview->active) {
    return TRUE;
  }

  switch (event->button) {
      case 1: /* left mouse button: zoom in */
        a_zoom (preview_w_current,
                SCHEMATIC_CANVAS (preview),
                ZOOM_IN,
                HOTKEY);
        schematic_canvas_invalidate_all (SCHEMATIC_CANVAS (widget));
        break;
      case 2: /* middle mouse button: pan */
        if (!x_event_get_pointer_position(preview_w_current, FALSE, &wx, &wy))
          return FALSE;
        schematic_canvas_pan (SCHEMATIC_CANVAS (preview), wx, wy);
        break;
      case 3: /* right mouse button: zoom out */
        a_zoom (preview_w_current,
                SCHEMATIC_CANVAS (preview),
                ZOOM_OUT,
                HOTKEY);
        schematic_canvas_invalidate_all (SCHEMATIC_CANVAS (widget));
        break;
  }

  return FALSE;
}


static void
schematic_preview_class_init (SchematicPreviewClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

  gobject_class->set_property = schematic_preview_set_property;
  gobject_class->get_property = schematic_preview_get_property;
  gobject_class->dispose      = schematic_preview_dispose;
  gobject_class->finalize     = schematic_preview_finalize;

  g_object_class_install_property (
    gobject_class, PROP_FILENAME,
    g_param_spec_string ("filename",
                         "",
                         "",
                         NULL,
                         G_PARAM_READWRITE));
  g_object_class_install_property (
    gobject_class, PROP_BUFFER,
    g_param_spec_string ("buffer",
                         "",
                         "",
                         NULL,
                         G_PARAM_WRITABLE));
  g_object_class_install_property(
    gobject_class, PROP_ACTIVE,
    g_param_spec_boolean ("active",
                          "",
                          "",
                          FALSE,
                          G_PARAM_READWRITE));

  g_signal_new ("update-preview",
                G_OBJECT_CLASS_TYPE (klass),
                (GSignalFlags) (G_SIGNAL_RUN_LAST), /*signal_flags */
                0, /*class_offset */
                NULL, /* accumulator */
                NULL, /* accu_data */
                NULL,
                G_TYPE_NONE,
                0 /* n_params */
                );
}


/*! \brief create a new preview widget
 */
GtkWidget*
schematic_preview_new ()
{
  return GTK_WIDGET (g_object_new (SCHEMATIC_TYPE_PREVIEW, NULL));
}


static void
schematic_preview_init (SchematicPreview *preview)
{
  SchematicWindow *preview_w_current;
  preview_w_current = schematic_window_new ();
  schematic_window_set_toplevel (preview_w_current, lepton_toplevel_new ());

  i_vars_set (preview_w_current);

  /* be sure to turn off scrollbars */
  preview_w_current->scrollbars_flag = FALSE;

  /* be sure to turn off the grid */
  gschem_options_set_grid_mode (preview_w_current->options, GRID_MODE_NONE);

  /* preview_w_current windows don't have toolbars */
  preview_w_current->handleboxes = FALSE;
  preview_w_current->toolbars    = FALSE;

  preview_w_current->drawing_area = GTK_WIDGET (preview);
  preview->window = preview_w_current;

  preview->active   = FALSE;
  preview->filename = NULL;
  preview->buffer   = NULL;

  schematic_canvas_set_page (SCHEMATIC_CANVAS (preview),
                             lepton_page_new (preview->window->toplevel,
                                              "preview"));

  gtk_widget_set_events (GTK_WIDGET (preview),
                         GDK_EXPOSURE_MASK |
                         GDK_POINTER_MOTION_MASK |
                         GDK_BUTTON_PRESS_MASK
#ifdef ENABLE_GTK3
                         | GDK_SCROLL_MASK
#endif
                         );
}

static void
schematic_preview_set_property (GObject *object,
                                guint property_id,
                                const GValue *value,
                                GParamSpec *pspec)
{
  SchematicPreview *preview = SCHEMATIC_PREVIEW (object);
  SchematicWindow *preview_w_current = preview->window;

  g_assert (preview_w_current != NULL);

  switch(property_id) {
      case PROP_FILENAME:
        if (preview->buffer != NULL) {
          g_free (preview->buffer);
          preview->buffer = NULL;
          g_object_notify (object, "buffer");
        }
        g_free (preview->filename);
        preview->filename = g_strdup (g_value_get_string (value));
        break;

      case PROP_BUFFER:
        if (preview->filename != NULL) {
          g_free (preview->filename);
          preview->filename = NULL;
          g_object_notify (object, "filename");
        }
        g_free (preview->buffer);
        preview->buffer = g_strdup (g_value_get_string (value));
        break;

      case PROP_ACTIVE:
        preview->active = g_value_get_boolean (value);
        g_signal_emit_by_name (preview, "update-preview");
        break;
      default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }

}

static void
schematic_preview_get_property (GObject *object,
                                guint property_id,
                                GValue *value,
                                GParamSpec *pspec)
{
  SchematicPreview *preview = SCHEMATIC_PREVIEW (object);

  switch(property_id) {
      case PROP_FILENAME:
        g_value_set_string (value, schematic_preview_get_page_filename (preview));
        break;

      case PROP_ACTIVE:
        g_value_set_boolean (value, preview->active);
        break;

      default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }
}

static void
schematic_preview_dispose (GObject *self)
{
  SchematicPreview *preview = SCHEMATIC_PREVIEW (self);
  SchematicWindow *preview_w_current = preview->window;

  if (preview_w_current != NULL) {
    preview_w_current->drawing_area = NULL;

    schematic_window_free (preview_w_current);

    preview->window = NULL;
  }

  G_OBJECT_CLASS (schematic_preview_parent_class)->dispose (self);
}

static void
schematic_preview_finalize (GObject *self)
{
  SchematicPreview *preview = SCHEMATIC_PREVIEW (self);

  g_free (preview->filename);
  g_free (preview->buffer);

  G_OBJECT_CLASS (schematic_preview_parent_class)->finalize (self);
}


/*! \brief Get the field \a active of this preview.
 *
 *  \param [in] preview The preview.
 *  \return The \a active field.
 */
gboolean
schematic_preview_get_active (GtkWidget *preview)
{
  g_return_val_if_fail (preview != NULL, FALSE);

  return SCHEMATIC_PREVIEW (preview)->active;
}


/*! \brief Get the field \a buffer of this preview.
 *
 *  \param [in] preview The preview.
 *  \return The \a buffer field.
 */
gchar*
schematic_preview_get_buffer (GtkWidget *preview)
{
  g_return_val_if_fail (preview != NULL, NULL);

  return SCHEMATIC_PREVIEW (preview)->buffer;
}


/*! \brief Get the field \a filename of this preview.
 *
 *  \param [in] preview The preview.
 *  \return The \a filename field.
 */
gchar*
schematic_preview_get_filename (GtkWidget *preview)
{
  g_return_val_if_fail (preview != NULL, NULL);

  return SCHEMATIC_PREVIEW (preview)->filename;
}


/*! \brief Get the field \a preview_w_current of this preview.
 *
 *  \param [in] preview The preview.
 *  \return The field.
 */
SchematicWindow*
schematic_preview_get_window (GtkWidget *preview)
{
  g_return_val_if_fail (preview != NULL, NULL);

  return SCHEMATIC_PREVIEW (preview)->window;
}
