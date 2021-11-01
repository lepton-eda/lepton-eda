/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2022 Lepton EDA Contributors
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
 *  \file gschem_preview.c
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

#define OVER_ZOOM_FACTOR 0.1


enum {
  PROP_FILENAME=1,
  PROP_BUFFER,
  PROP_ACTIVE
};

G_DEFINE_TYPE (GschemPreview, gschem_preview, GSCHEM_TYPE_PAGE_VIEW);

static void preview_set_property (GObject *object,
                                  guint property_id,
                                  const GValue *value,
                                  GParamSpec *pspec);
static void preview_get_property (GObject *object,
                                  guint property_id,
                                  GValue *value,
                                  GParamSpec *pspec);
static void preview_dispose (GObject *self);
static void preview_finalize (GObject *self);


/*! \brief create a new preview widget
 */
GtkWidget*
gschem_preview_new ()
{
  return GTK_WIDGET (g_object_new (GSCHEM_TYPE_PREVIEW, NULL));
}


/*! \brief get the filename for the current page
 */
static const char*
preview_get_filename (GschemPreview *preview)
{
  LeptonPage *page = gschem_page_view_get_page (GSCHEM_PAGE_VIEW (preview));

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
static void
preview_callback_realize (GtkWidget *widget, gpointer user_data)
{
  g_return_if_fail (widget != NULL);

  gtk_widget_grab_focus (widget);
  gschem_page_view_zoom_extents (GSCHEM_PAGE_VIEW (widget), NULL);
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
static gboolean
preview_callback_button_press (GtkWidget *widget,
                               GdkEventButton *event,
                               gpointer user_data)
{
  GschemPreview *preview = GSCHEM_PREVIEW (widget);
  GschemToplevel *preview_w_current = preview->preview_w_current;
  gint wx, wy;

  if (!preview->active) {
    return TRUE;
  }

  switch (event->button) {
      case 1: /* left mouse button: zoom in */
        a_zoom (preview_w_current,
                GSCHEM_PAGE_VIEW (preview),
                ZOOM_IN,
                HOTKEY);
        gschem_page_view_invalidate_all (GSCHEM_PAGE_VIEW (widget));
        break;
      case 2: /* middle mouse button: pan */
        if (!x_event_get_pointer_position(preview_w_current, FALSE, &wx, &wy))
          return FALSE;
        gschem_page_view_pan (GSCHEM_PAGE_VIEW (preview), wx, wy);
        break;
      case 3: /* right mouse button: zoom out */
        a_zoom (preview_w_current,
                GSCHEM_PAGE_VIEW (preview),
                ZOOM_OUT,
                HOTKEY);
        gschem_page_view_invalidate_all (GSCHEM_PAGE_VIEW (widget));
        break;
  }

  return FALSE;
}


/*! \brief Updates the preview widget.
 *  \par Function Description
 *  This function updates the preview: if the preview is active and a
 *  filename has been given, it opens the file and displays
 *  it. Otherwise it displays a blank page.
 *
 *  \param [in] preview The preview widget.
 */
static void
preview_update (GschemPreview *preview)
{
  int left, top, right, bottom;
  int width, height;
  GError * err = NULL;

  GschemPageView *preview_view = GSCHEM_PAGE_VIEW (preview);

  g_return_if_fail (preview_view != NULL);
  LeptonPage *preview_page = gschem_page_view_get_page (preview_view);

  if (preview_page == NULL) {
    return;
  }

  LeptonToplevel *preview_toplevel = preview_page->toplevel;

  /* delete old preview */
  lepton_page_delete_objects (preview_page);

  if (preview->active) {
    g_assert ((preview->filename == NULL) || (preview->buffer == NULL));
    if (preview->filename != NULL) {
      /* open up file in current page */
      f_open_flags (preview_toplevel, preview_page,
                    preview->filename,
                    F_OPEN_RC | F_OPEN_RESTORE_CWD, NULL);
      /* test value returned by f_open... - Fix me */
      /* we should display something if there an error occured - Fix me */
    }
    if (preview->buffer != NULL) {
      /* Load the data buffer */
      GList * objects = o_read_buffer (preview_page,
                                       NULL, preview->buffer, -1,
                                       _("Preview Buffer"), &err);

      if (err == NULL) {
        lepton_page_append_list (preview_page, objects);
      }
      else {
        lepton_page_append (preview_page,
                            lepton_text_object_new (2,
                                                    100,
                                                    100,
                                                    LOWER_MIDDLE,
                                                    0,
                                                    err->message,
                                                    10,
                                                    VISIBLE,
                                                    SHOW_NAME_VALUE));
        g_error_free(err);
      }
    }
  }

  if (world_get_object_glist_bounds (lepton_page_objects (preview_page),
                                     /* Do not include hidden text. */
                                     FALSE,
                                     &left, &top,
                                     &right, &bottom)) {
    /* Clamp the canvas size to the extents of the page being previewed */
    width = right - left;
    height = bottom - top;

    GschemPageGeometry *geometry = gschem_page_view_get_page_geometry (preview_view);
    geometry->world_left   = left   - ((double)width  * OVER_ZOOM_FACTOR);
    geometry->world_right  = right  + ((double)width  * OVER_ZOOM_FACTOR);
    geometry->world_top    = top    - ((double)height * OVER_ZOOM_FACTOR);
    geometry->world_bottom = bottom + ((double)height * OVER_ZOOM_FACTOR);
  }

  /* display current page (possibly empty) */
  gschem_page_view_zoom_extents (preview_view, NULL);
}


static void
gschem_preview_class_init (GschemPreviewClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

  gobject_class->set_property = preview_set_property;
  gobject_class->get_property = preview_get_property;
  gobject_class->dispose      = preview_dispose;
  gobject_class->finalize     = preview_finalize;

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


}


static gboolean
preview_event_scroll (GtkWidget *widget,
                      GdkEventScroll *event,
                      GschemToplevel *w_current)
{
  if (!GSCHEM_PREVIEW (widget)->active) {
    return TRUE;
  }
  return x_event_scroll (widget, event, GSCHEM_PREVIEW (widget)->preview_w_current);
}

static void
gschem_preview_init (GschemPreview *preview)
{
  struct event_reg_t {
    const gchar *detailed_signal;
    GCallback c_handler;
  } drawing_area_events[] = {
    { "realize",              G_CALLBACK (preview_callback_realize)       },
#ifdef ENABLE_GTK3
    { "draw",                 G_CALLBACK (x_event_draw)                   },
#else
    { "expose_event",         G_CALLBACK (x_event_expose)                 },
#endif
    { "button_press_event",   G_CALLBACK (preview_callback_button_press)  },
    { "configure_event",      G_CALLBACK (x_event_configure)              },
    { "scroll_event",         G_CALLBACK (preview_event_scroll)           },
    { NULL,                   NULL                                        }
  }, *tmp;

  GschemToplevel *preview_w_current;
  preview_w_current = gschem_toplevel_new ();
  gschem_toplevel_set_toplevel (preview_w_current, lepton_toplevel_new ());

  i_vars_set (preview_w_current);

  /* be sure to turn off scrollbars */
  preview_w_current->scrollbars_flag = FALSE;

  /* be sure to turn off the grid */
  gschem_options_set_grid_mode (preview_w_current->options, GRID_MODE_NONE);

  /* preview_w_current windows don't have toolbars */
  preview_w_current->handleboxes = FALSE;
  preview_w_current->toolbars    = FALSE;

  preview_w_current->drawing_area = GTK_WIDGET (preview);
  preview->preview_w_current = preview_w_current;

  preview->active   = FALSE;
  preview->filename = NULL;
  preview->buffer   = NULL;

  gschem_page_view_set_page (GSCHEM_PAGE_VIEW (preview),
                             lepton_page_new (preview->preview_w_current->toplevel,
                                              "preview"));

  gtk_widget_set_events (GTK_WIDGET (preview),
                         GDK_EXPOSURE_MASK |
                         GDK_POINTER_MOTION_MASK |
                         GDK_BUTTON_PRESS_MASK
#ifdef ENABLE_GTK3
                         | GDK_SCROLL_MASK
#endif
                         );
  for (tmp = drawing_area_events; tmp->detailed_signal != NULL; tmp++) {
    g_signal_connect (GTK_WIDGET (preview),
                      tmp->detailed_signal,
                      tmp->c_handler,
                      preview_w_current);
  }

}

static void
preview_set_property (GObject *object,
                      guint property_id,
                      const GValue *value,
                      GParamSpec *pspec)
{
  GschemPreview *preview = GSCHEM_PREVIEW (object);
  GschemToplevel *preview_w_current = preview->preview_w_current;

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
        preview_update (preview);
        break;
      default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }

}

static void
preview_get_property (GObject *object,
                      guint property_id,
                      GValue *value,
                      GParamSpec *pspec)
{
  GschemPreview *preview = GSCHEM_PREVIEW (object);

  switch(property_id) {
      case PROP_FILENAME:
        g_value_set_string (value, preview_get_filename (preview));
        break;

      case PROP_ACTIVE:
        g_value_set_boolean (value, preview->active);
        break;

      default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }
}

static void
preview_dispose (GObject *self)
{
  GschemPreview *preview = GSCHEM_PREVIEW (self);
  GschemToplevel *preview_w_current = preview->preview_w_current;

  if (preview_w_current != NULL) {
    preview_w_current->drawing_area = NULL;

    gschem_toplevel_free (preview_w_current);

    preview->preview_w_current = NULL;
  }

  G_OBJECT_CLASS (gschem_preview_parent_class)->dispose (self);
}

static void
preview_finalize (GObject *self)
{
  GschemPreview *preview = GSCHEM_PREVIEW (self);

  g_free (preview->filename);
  g_free (preview->buffer);

  G_OBJECT_CLASS (gschem_preview_parent_class)->finalize (self);
}
