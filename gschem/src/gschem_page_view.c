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
 * \file gschem_page_view.c
 *
 * \brief A widget for viewing a schematic page
 */

#include <config.h>

#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <math.h>

#include "gschem.h"
#include <gdk/gdkkeysyms.h>

#include "gtk/gtkmarshal.h"



#define INVALIDATE_MARGIN 1



enum
{
  PROP_0,
  PROP_HADJUSTMENT,
  PROP_PAGE,
  PROP_PAGE_GEOMETRY,
  PROP_TOPLEVEL,
  PROP_VADJUSTMENT
};



static void
dispose (GObject *object);

static void
finalize (GObject *object);

static void
get_property (GObject *object, guint param_id, GValue *value, GParamSpec *pspec);

static void
gschem_page_view_class_init (GschemPageViewClass *klass);

static void
gschem_page_view_init (GschemPageView *view);

static void
gschem_page_view_update_hadjustment (GschemPageView *view);

static void
gschem_page_view_update_vadjustment (GschemPageView *view);

static void
hadjustment_value_changed (GtkAdjustment *vadjustment, GschemPageView *view);

static void
remove_page_weak_reference (PAGE *page, gpointer geometry, GschemPageView *view);

static void
page_deleted (PAGE *page, GschemPageView *view);

static void
set_property (GObject *object, guint param_id, const GValue *value, GParamSpec *pspec);

static void
set_scroll_adjustments (GschemPageView *view, GtkAdjustment *hadjustment, GtkAdjustment *vadjustment);

static void
vadjustment_value_changed (GtkAdjustment *vadjustment, GschemPageView *view);



static GObjectClass *gschem_page_view_parent_class = NULL;


/*
 *  In later versions of GTK+, the GtkScrolledWindow uses an interface, instead
 *  of signals, to set the scrollbar adjustments. When Gschem uses on of these
 *  more recent version of GTK+, this function will no longer be needed.
 */
static void
cclosure_marshal_VOID__OBJECT_OBJECT (GClosure     *closure,
                                      GValue       *return_value G_GNUC_UNUSED,
                                      guint         n_param_values,
                                      const GValue *param_values,
                                      gpointer      invocation_hint G_GNUC_UNUSED,
                                      gpointer      marshal_data)
{
  typedef void (*GMarshalFunc_VOID__OBJECT_OBJECT) (gpointer     data1,
                                                    gpointer     arg_1,
                                                    gpointer     arg_2,
                                                    gpointer     data2);

  register GMarshalFunc_VOID__OBJECT_OBJECT callback;
  register GCClosure *cc = (GCClosure*) closure;
  register gpointer data1, data2;

  g_return_if_fail (n_param_values == 3);

  if (G_CCLOSURE_SWAP_DATA (closure)) {
    data1 = closure->data;
    data2 = g_value_peek_pointer (param_values + 0);
  }
  else {
    data1 = g_value_peek_pointer (param_values + 0);
    data2 = closure->data;
  }

  callback = (GMarshalFunc_VOID__OBJECT_OBJECT) (marshal_data ? marshal_data : cc->callback);

  callback (data1,
            g_value_get_object (param_values + 1),
            g_value_get_object (param_values + 2),
            data2);
}



/*! \brief Dispose of the object
 */
static void
dispose (GObject *object)
{
  GschemPageView *view;

  g_return_if_fail (object != NULL);
  view = GSCHEM_PAGE_VIEW (object);
  g_return_if_fail (view != NULL);

  gschem_page_view_set_hadjustment (view, NULL);
  gschem_page_view_set_vadjustment (view, NULL);

  g_hash_table_foreach (view->geometry_table, (GHFunc)remove_page_weak_reference, view);
  g_hash_table_remove_all (view->geometry_table);

  /* According to the GObject Manual the dispose function might be
   * called several times. We don't want to call
   * gschem_page_view_set_page twice here  */
  if (view->page != NULL) {
    gschem_page_view_set_page (view, NULL);
  }

  /* lastly, chain up to the parent dispose */

  g_return_if_fail (gschem_page_view_parent_class != NULL);
  gschem_page_view_parent_class->dispose (object);
}



/*! \brief Finalize object
 */
static void
finalize (GObject *object)
{
  GschemPageView *view = GSCHEM_PAGE_VIEW (object);

  g_return_if_fail (view != NULL);
  g_return_if_fail (view->geometry_table != NULL);

  g_hash_table_destroy (view->geometry_table);

  /* lastly, chain up to the parent finalize */

  g_return_if_fail (gschem_page_view_parent_class != NULL);
  gschem_page_view_parent_class->finalize (object);
}



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
  GschemPageView *view = GSCHEM_PAGE_VIEW (object);

  switch (param_id) {
    case PROP_HADJUSTMENT:
      g_value_set_object (value, gschem_page_view_get_hadjustment (view));
      break;

    case PROP_PAGE:
      g_value_set_pointer (value, gschem_page_view_get_page (view));
      break;

    case PROP_PAGE_GEOMETRY:
      g_value_set_boxed (value, gschem_page_view_get_page_geometry (view));
      break;

    case PROP_VADJUSTMENT:
      g_value_set_object (value, gschem_page_view_get_vadjustment (view));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
  }
}



/*! \brief Initialize GschemPageView class
 *
 *  \param [in] klass The class for the GschemPageView
 */
static void
gschem_page_view_class_init (GschemPageViewClass *klass)
{
  gschem_page_view_parent_class = G_OBJECT_CLASS (g_type_class_peek_parent (klass));

  G_OBJECT_CLASS (klass)->dispose  = dispose;
  G_OBJECT_CLASS (klass)->finalize = finalize;

  G_OBJECT_CLASS (klass)->get_property = get_property;
  G_OBJECT_CLASS (klass)->set_property = set_property;

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_HADJUSTMENT,
                                   g_param_spec_object ("hadjustment",
                                                        "Horizontal adjustment",
                                                        "Horizontal adjustment",
                                                        GTK_TYPE_ADJUSTMENT,
                                                        G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_PAGE,
                                   g_param_spec_pointer ("page",
                                                         "Page",
                                                         "Page",
                                                         G_PARAM_READWRITE | G_PARAM_STATIC_STRINGS));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_PAGE_GEOMETRY,
                                   g_param_spec_boxed ("page-geometry",
                                                       "Page Geometry",
                                                       "Page Geometry",
                                                       GSCHEM_TYPE_PAGE_GEOMETRY,
                                                       G_PARAM_READABLE | G_PARAM_STATIC_STRINGS));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_VADJUSTMENT,
                                   g_param_spec_object ("vadjustment",
                                                        "Vertical adjustment",
                                                        "Vertical adjustment",
                                                        GTK_TYPE_ADJUSTMENT,
                                                        G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

  GTK_WIDGET_CLASS (klass)->set_scroll_adjustments_signal = g_signal_new (
    "set-scroll-adjustments",
    G_OBJECT_CLASS_TYPE (klass),
    G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
    0,
    NULL,
    NULL,
    cclosure_marshal_VOID__OBJECT_OBJECT,
    G_TYPE_NONE,
    2,
    GTK_TYPE_ADJUSTMENT,
    GTK_TYPE_ADJUSTMENT);

  g_signal_new (
    "update-grid-info",
    G_OBJECT_CLASS_TYPE (klass),
    G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
    0,
    NULL,
    NULL,
    g_cclosure_marshal_VOID__VOID,
    G_TYPE_NONE,
    0);
}



/*! \brief Get the horizontal adjustment for this view
 *
 *  \param [in] view The view
 *  \return The horizontal adjustment for this view
 */
GtkAdjustment*
gschem_page_view_get_hadjustment (GschemPageView *view)
{
  g_return_val_if_fail (view != NULL, NULL);

  return view->hadjustment;
}



/*! \brief Get page for this view
 *
 *  \param [in] view The view
 *  \return The page for the view
 */
PAGE*
gschem_page_view_get_page (GschemPageView *view)
{
  g_return_val_if_fail (view != NULL, NULL);

  return view->page;
}



/*! \brief Get page geometry for this view
 *
 *  \param [in] view The view
 *  \return The page geometry for the view
 */
GschemPageGeometry*
gschem_page_view_get_page_geometry (GschemPageView *view)
{
  typedef void (*NotifyFunction) (void*,void*);

  PAGE *page = NULL;
  GschemPageGeometry *geometry = NULL;
  int screen_width;
  int screen_height;

  g_return_val_if_fail (view != NULL, NULL);

  page = gschem_page_view_get_page (view);
  if (page == NULL) {
    return NULL;
  }

  geometry = g_hash_table_lookup (view->geometry_table, page);

  /* \todo The following line is deprecated in GDK 2.24 */
  gdk_drawable_get_size (GTK_WIDGET (view)->window, &screen_width, &screen_height);

  if (geometry == NULL) {
    geometry = gschem_page_geometry_new_with_values (screen_width,
                                                     screen_height,
                                                     view->page->toplevel->init_left,
                                                     view->page->toplevel->init_top,
                                                     view->page->toplevel->init_right,
                                                     view->page->toplevel->init_bottom,
                                                     view->page->toplevel->init_left,
                                                     view->page->toplevel->init_top,
                                                     view->page->toplevel->init_right,
                                                     view->page->toplevel->init_bottom);

    g_hash_table_insert (view->geometry_table, page, geometry);
    s_page_weak_ref (page, (NotifyFunction) page_deleted, view);

    gschem_page_geometry_zoom_extents (geometry,
                                       view->page->toplevel,
                                       s_page_objects (page),
                                       A_PAN_DONT_REDRAW);

  }
  else {
    gschem_page_geometry_set_values (geometry,
                                     screen_width,
                                     screen_height,
                                     gschem_page_geometry_get_viewport_left (geometry),
                                     gschem_page_geometry_get_viewport_top (geometry),
                                     gschem_page_geometry_get_viewport_right (geometry),
                                     gschem_page_geometry_get_viewport_bottom (geometry));
  }

  return geometry;
}



/*! \brief Get/register GschemPageView type.
 */
GType
gschem_page_view_get_type ()
{
  static GType type = 0;

  if (type == 0) {
    static const GTypeInfo info = {
      sizeof(GschemPageViewClass),
      NULL,                                                    /* base_init */
      NULL,                                                    /* base_finalize */
      (GClassInitFunc) gschem_page_view_class_init,
      NULL,                                                    /* class_finalize */
      NULL,                                                    /* class_data */
      sizeof(GschemPageView),
      0,                                                       /* n_preallocs */
      (GInstanceInitFunc) gschem_page_view_init,
    };

    type = g_type_register_static (GTK_TYPE_DRAWING_AREA, "GschemPageView", &info, 0);
  }

  return type;
}



/*! \brief Get the vertical adjustment for this view
 *
 *  \param [in] view The view
 *  \return The vertical adjustment for this view
 */
GtkAdjustment*
gschem_page_view_get_vadjustment (GschemPageView *view)
{
  g_return_val_if_fail (view != NULL, NULL);

  return view->vadjustment;
}



/*! \brief Schedule redraw for the entire window
 *
 *  \param [in,out] view The Gschem page view to redraw
 */
void
gschem_page_view_invalidate_all (GschemPageView *view)
{
  GdkWindow *window;

  g_return_if_fail (view != NULL);

  window = gtk_widget_get_window (GTK_WIDGET (view));

  if (window == NULL) {
    return;
  }

  gdk_window_invalidate_rect (window, NULL, FALSE);
}



/*! \brief Schedule redraw of the given object
 *
 *  \param [in,out] view   The Gschem page view to redraw
 *  \param [in]     object The object to redraw
 */
void
gschem_page_view_invalidate_object (GschemPageView *view, OBJECT *object)
{
  g_return_if_fail (object != NULL);
  g_return_if_fail (view != NULL);

  if (!GTK_WIDGET_REALIZED (GTK_WIDGET (view))) {
    return;
  }

  if (view->page != NULL) {
    gboolean success;
    int world_bottom;
    int world_right;
    int world_left;
    int world_top;

    success = world_get_single_object_bounds (view->page->toplevel,
                                              object,
                                              &world_left,
                                              &world_top,
                                              &world_right,
                                              &world_bottom);

    if (success) {
      gschem_page_view_invalidate_world_rect (view,
                                              world_left,
                                              world_top,
                                              world_right,
                                              world_bottom);
    }
  }
}



/*! \brief Schedule redraw of the given rectange
 *
 *  \param [in,out] view   The Gschem page view to redraw
 *  \param [in]     left
 *  \param [in]     top
 *  \param [in]     right
 *  \param [in]     bottom
 */
void
gschem_page_view_invalidate_screen_rect (GschemPageView *view, int left, int top, int right, int bottom)
{
  int bloat;
  int cue_half_size;
  int grip_half_size;
  GdkRectangle rect;
  GdkWindow *window;

  g_return_if_fail (view != NULL);

  window = gtk_widget_get_window (GTK_WIDGET (view));

  if (window == NULL) {
    return;
  }

  grip_half_size = GRIP_SIZE / 2;
  cue_half_size = gschem_page_view_SCREENabs (view, CUE_BOX_SIZE);
  bloat = MAX (grip_half_size, cue_half_size) + INVALIDATE_MARGIN;

  rect.x = MIN(left, right) - bloat;
  rect.y = MIN(top, bottom) - bloat;
  rect.width = 1 + abs( left - right ) + 2 * bloat;
  rect.height = 1 + abs( top - bottom ) + 2 * bloat;

  gdk_window_invalidate_rect (window, &rect, FALSE);
}



/*! \brief Schedule redraw of the given rectange
 *
 *  \param [in,out] view   The Gschem page view to redraw
 *  \param [in]     left
 *  \param [in]     top
 *  \param [in]     right
 *  \param [in]     bottom
 */
void
gschem_page_view_invalidate_world_rect (GschemPageView *view, int left, int top, int right, int bottom)
{
  int screen_bottom;
  int screen_right;
  int screen_left;
  int screen_top;

  g_return_if_fail (view != NULL);

  gschem_page_view_WORLDtoSCREEN (view, left, top, &screen_left, &screen_top);
  gschem_page_view_WORLDtoSCREEN (view, right, bottom, &screen_right, &screen_bottom);

  gschem_page_view_invalidate_screen_rect (view,
                                           screen_left,
                                           screen_top,
                                           screen_right,
                                           screen_bottom);
}



/*! \brief Initialize GschemPageView instance
 *
 *  \param [in,out] view the gschem page view
 */
static void
gschem_page_view_init (GschemPageView *view)
{
  g_return_if_fail (view != NULL);

  view->hadjustment = NULL;
  view->vadjustment = NULL;

  view->geometry_table = g_hash_table_new_full (g_direct_hash,
                                                g_direct_equal,
                                                NULL,
                                                (GDestroyNotify) gschem_page_geometry_free);

  view->page = NULL;
  view->configured = FALSE;

  g_signal_connect (view,
                    "set-scroll-adjustments",
                    G_CALLBACK (set_scroll_adjustments),
                    NULL);
}



/*! \brief Create a new instance of the GschemPageView
 *
 *  \return A new instance of the GschemPageView
 */
GschemPageView*
gschem_page_view_new_with_page (PAGE *page)
{
  return GSCHEM_PAGE_VIEW (g_object_new (GSCHEM_TYPE_PAGE_VIEW, "page", page, NULL));
}



/*! \brief Pan the view on the given world coordinate using given zoom factor
 *
 *  \param [in,out] page_view This GschemPageView
 *  \param [in]     w_x       The world x coordinate of the new center
 *  \param [in]     w_y       The world y coordinate of the new center
 *  \param [in]     relativ_zoom_factor  The zoom factor
 */
void
gschem_page_view_pan_general (GschemPageView *view, int w_x, int w_y, double relativ_zoom_factor)
{
  PAGE *page = NULL;
  GschemPageGeometry *geometry = NULL;

  g_return_if_fail (view != NULL);

  page = gschem_page_view_get_page (view);
  g_return_if_fail (page != NULL);

  geometry = gschem_page_view_get_page_geometry (view);
  g_return_if_fail (geometry != NULL);

  /* make mouse to the new world-center;
     attention: there are information looses because of type cast in mil_x */

  gschem_page_geometry_pan_general (geometry, w_x, w_y, relativ_zoom_factor, A_PAN_DONT_REDRAW);

  /*! \bug FIXME? This call will trigger a motion event (x_event_motion()),
   * even if the user doesn't move the mouse
   * Not ready for prime time, maybe there is another way to trigger the
   * motion event without changing the cursor position (Werner)
   */
  /* x_basic_warp_cursor(w_current->drawing_area, x, y); */

  g_signal_emit_by_name (view, "update-grid-info");
  gschem_page_view_update_scroll_adjustments (view);
  gschem_page_view_invalidate_all (view);
}


/*! \brief Center the view on the given world coordinate
 *
 *  \param [in,out] page_view This GschemPageView
 *  \param [in]     w_x       The world x coordinate of the new center
 *  \param [in]     w_y       The world y coordinate of the new center
 */
void
gschem_page_view_pan (GschemPageView *view, int w_x, int w_y)
{
  gschem_page_view_pan_general (view, w_x, w_y, 1);
}



/*! \brief Pan the view by the given screen coordinate displacement
 *
 *  The w_current parameter will be replaced with signals. This way, any object
 *  can listen for changes in the view.
 *
 *  \param [in,out] page      This GschemPageView
 *  \param [in]     w_current The GschemToplevel
 *  \param [in]     diff_x    The screen x coordinate displacement
 *  \param [in]     diff_y    The screen y coordinate displacement
 */
void
gschem_page_view_pan_mouse (GschemPageView *view, GschemToplevel *w_current, int diff_x, int diff_y)
{
  PAGE *page = NULL;
  GschemPageGeometry *geometry = NULL;
  double world_cx, world_cy;
  double page_cx, page_cy;

  g_return_if_fail (view != NULL);

  page = gschem_page_view_get_page (view);
  g_return_if_fail (page != NULL);

  geometry = gschem_page_view_get_page_geometry (view);
  g_return_if_fail (geometry != NULL);

#if DEBUG
  printf("gschem_page_view_pan_mouse(): diff_x=%d, diff_y=%d\n", diff_x, diff_y);
#endif

  page_cx = (gschem_page_geometry_get_viewport_left (geometry) + gschem_page_geometry_get_viewport_right (geometry)) / 2.0;
  page_cy = (gschem_page_geometry_get_viewport_top (geometry) + gschem_page_geometry_get_viewport_bottom (geometry)) / 2.0;

  world_cx = page_cx - WORLDabs (w_current, diff_x);
  world_cy = page_cy + WORLDabs (w_current, diff_y);

#if DEBUG
  printf("  world_cx=%f, world_cy=%f\n", world_cx, world_cy);
#endif

  gschem_page_view_pan_general (view, world_cx, world_cy, 1);
}



/*! \brief Transform WORLD coordinates to WORLD coordinates
 *  \par Function Description
 *  This function takes in SCREEN x/y coordinates and
 *  transforms them to WORLD x/y coordinates.
 *
 *  \param [in]  w_current  The GschemToplevel object.
 *  \param [in]  mx         The x coordinate in SCREEN units.
 *  \param [in]  my         The y coordinate in SCREEN units.
 *  \param [out] x          The x coordinate in WORLD units.
 *  \param [out] y          The y coordinate in WORLD units.
 *  \note Question: why are we returning in x and y
 *                  if this is SCREEN to WORLD shouldn't WORLD
 *                  coordinates be returned in mx and my?
 */
void
gschem_page_view_SCREENtoWORLD (GschemPageView *view, int mx, int my, int *x, int *y)
{
  GschemPageGeometry *geometry = gschem_page_view_get_page_geometry (view);

  g_return_if_fail (geometry != NULL);

  *x = gschem_page_geometry_mil_x (geometry, mx);
  *y = gschem_page_geometry_mil_y (geometry, my);
}



/*! \brief Set the horizontal scroll adjustment for this view
 *
 *  \param [in,out] view The view
 *  \param [in]     hadjustment The horizontal scroll adjustment
 */
void
gschem_page_view_set_hadjustment (GschemPageView *view, GtkAdjustment *hadjustment)
{
  g_return_if_fail (view != NULL);

  if (view->hadjustment != NULL) {
    g_signal_handlers_disconnect_by_func (G_OBJECT (view->hadjustment),
                                          G_CALLBACK (hadjustment_value_changed),
                                          view);

    g_object_unref (view->hadjustment);
  }

  view->hadjustment = hadjustment;

  if (view->hadjustment != NULL) {
    g_object_ref (view->hadjustment);

    g_signal_connect (G_OBJECT (view->hadjustment),
                      "value-changed",
                      G_CALLBACK (hadjustment_value_changed),
                      view);
  }

  g_object_notify (G_OBJECT (view), "hadjustment");
}



/*! \brief Set the page for this view
 *
 *  The toplevel property must be set and the page must belong to that
 *  toplevel. Currently, the codebase does not allow the page to be
 *  NULL.
 *
 *  \param [in,out] view The view
 *  \param [in]     page The page
 */
void
gschem_page_view_set_page (GschemPageView *view, PAGE *page)
{
  g_return_if_fail (view != NULL);
  g_return_if_fail (view->geometry_table != NULL);

  view->page = page;

  if (page != NULL) {
    g_return_if_fail (page->toplevel != NULL);
    s_page_goto (page->toplevel, page);
  }

  g_object_notify (G_OBJECT (view), "page");
  g_object_notify (G_OBJECT (view), "page-geometry");
  g_signal_emit_by_name (view, "update-grid-info");
}


/*! \brief Set the vertical scroll adjustment for this view
 *
 *  \param [in,out] view The view
 *  \param [in]     vadjustment The vertical scroll adjustment
 */
void
gschem_page_view_set_vadjustment (GschemPageView *view, GtkAdjustment *vadjustment)
{
  g_return_if_fail (view != NULL);

  if (view->vadjustment != NULL) {
    g_signal_handlers_disconnect_by_func (G_OBJECT (view->vadjustment),
                                          G_CALLBACK (vadjustment_value_changed),
                                          view);

    g_object_unref (view->vadjustment);
  }

  view->vadjustment = vadjustment;

  if (view->vadjustment != NULL) {
    g_object_ref (view->vadjustment);

    g_signal_connect (G_OBJECT (view->vadjustment),
                      "value-changed",
                      G_CALLBACK (vadjustment_value_changed),
                      view);
  }

  g_object_notify (G_OBJECT (view), "vadjustment");
}



/*! \brief Signal handler for a horizontal scroll adjustment change
 */
static void
hadjustment_value_changed (GtkAdjustment *hadjustment, GschemPageView *view)
{
  g_return_if_fail (hadjustment != NULL);
  g_return_if_fail (view != NULL);

  GschemPageGeometry *geometry = gschem_page_view_get_page_geometry (view);
  g_return_if_fail (geometry != NULL);

  if (view->hadjustment != NULL) {
    int current_left;
    int new_left;

    g_return_if_fail (view->hadjustment == hadjustment);

    current_left = gschem_page_geometry_get_viewport_left (geometry),
    new_left = (int) hadjustment->value;

    geometry->viewport_left = new_left;
    geometry->viewport_right = geometry->viewport_right - (current_left - new_left);

    gschem_page_view_invalidate_all (view);
  }
}



/*! \brief Set a gobject property
 */
static void
set_property (GObject *object, guint param_id, const GValue *value, GParamSpec *pspec)
{
  GschemPageView *view = GSCHEM_PAGE_VIEW (object);

  switch (param_id) {
    case PROP_HADJUSTMENT:
      gschem_page_view_set_hadjustment (view, g_value_get_object (value));
      break;

    case PROP_PAGE:
      gschem_page_view_set_page (view, g_value_get_pointer (value));
      break;

    case PROP_VADJUSTMENT:
      gschem_page_view_set_vadjustment (view, g_value_get_object (value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
  }
}



/*! \brief Get absolute SCREEN coordinate.
 *
 *  A temporary function until a GschemToplevel is not required for coordinate
 *  conversions. See the function SCREENabs.
 */
int
gschem_page_view_SCREENabs(GschemPageView *view, int val)
{
  double f0,f1,f;
  double i;
  int j;
  PAGE *page;
  GschemPageGeometry *geometry = gschem_page_view_get_page_geometry (view);

  g_return_val_if_fail (view != NULL, 0);

  page = gschem_page_view_get_page (view);

  if (page == NULL) return 0;

  f0 = gschem_page_geometry_get_viewport_left  (geometry);
  f1 = gschem_page_geometry_get_viewport_right (geometry);
  f = view->page->toplevel->width / (f1 - f0);
  i = f * (double)(val);

#ifdef HAS_RINT
  j = rint(i);
#else
  j = i;
#endif

  return(j);
}



/*! \brief Update the horizontal scroll adjustment
 */
static void
gschem_page_view_update_hadjustment (GschemPageView *view)
{
  g_return_if_fail (view != NULL);

  if (view->hadjustment != NULL) {
    PAGE *page = gschem_page_view_get_page (view);

    g_return_if_fail (page != NULL);

    GschemPageGeometry *geometry = gschem_page_view_get_page_geometry (view);
    g_return_if_fail (geometry != NULL);

    gtk_adjustment_set_page_increment (view->hadjustment,
                                       fabs (geometry->viewport_right - geometry->viewport_left) - 100.0);

    gtk_adjustment_set_page_size (view->hadjustment,
                                  fabs (geometry->viewport_right - geometry->viewport_left));

    gtk_adjustment_set_value (view->hadjustment,
                               geometry->viewport_left);

#if DEBUG
    printf("H %f %f\n", view->hadjustment->lower, view->hadjustment->upper);
    printf("Hp %f\n", view->hadjustment->page_size);
#endif

    gtk_adjustment_changed(view->hadjustment);
    gtk_adjustment_value_changed (view->hadjustment);
  }
}



/*! \brief Update the scroll adjustments
 */
void
gschem_page_view_update_scroll_adjustments (GschemPageView *view)
{
  g_return_if_fail (view != NULL);

  gschem_page_view_update_hadjustment (view);
  gschem_page_view_update_vadjustment (view);
}



/*! \brief Update the vertical scroll adjustment
 */
static void
gschem_page_view_update_vadjustment (GschemPageView *view)
{
  g_return_if_fail (view != NULL);

  if (view->vadjustment != NULL) {
    PAGE *page = gschem_page_view_get_page (view);

    g_return_if_fail (page != NULL);

    GschemPageGeometry *geometry = gschem_page_view_get_page_geometry (view);
    g_return_if_fail (geometry != NULL);

    gtk_adjustment_set_page_increment(view->vadjustment,
                                      fabs (geometry->viewport_bottom - geometry->viewport_top) - 100.0);

    gtk_adjustment_set_page_size (view->vadjustment,
                                  fabs (geometry->viewport_bottom - geometry->viewport_top));

    gtk_adjustment_set_value(view->vadjustment,
                             view->page->toplevel->init_bottom - geometry->viewport_bottom);

#if DEBUG
    printf("V %f %f\n", view->vadjustment->lower, view->vadjustment->upper);
    printf("Vp %f\n", view->vadjustment->page_size);
#endif

    gtk_adjustment_changed(view->vadjustment);
    gtk_adjustment_value_changed (view->vadjustment);
  }
}



/*! \brief
 *
 */
static void
remove_page_weak_reference (PAGE *page, gpointer geometry, GschemPageView *view)
{
  typedef void (*NotifyFunction) (void*,void*);

  g_return_if_fail (page != NULL);
  g_return_if_fail (view != NULL);

  s_page_weak_unref (page, (NotifyFunction) page_deleted, view);
}



/*! \brief
 *
 */
static void
page_deleted (PAGE *page, GschemPageView *view)
{
  g_return_if_fail (page != NULL);
  g_return_if_fail (view != NULL);
  g_return_if_fail (view->geometry_table != NULL);

  g_hash_table_remove (view->geometry_table, page);
}



/*! \brief Signal handler for setting the scroll adjustments
 *
 *  Sent from the GtkScrolledWindow to set the adjustments for the
 *  corresponding scroll bars.
 */
static void
set_scroll_adjustments (GschemPageView *view, GtkAdjustment *hadjustment, GtkAdjustment *vadjustment)
{
  gschem_page_view_set_hadjustment (view, hadjustment);
  gschem_page_view_set_vadjustment (view, vadjustment);
}



/*! \brief Signal handler for a vertical scroll adjustment change
 */
static void
vadjustment_value_changed (GtkAdjustment *vadjustment, GschemPageView *view)
{
  g_return_if_fail (vadjustment != NULL);
  g_return_if_fail (view != NULL);

  if ((view->page->toplevel != NULL) && (view->vadjustment != NULL)) {
    int current_bottom;
    int new_bottom;

    g_return_if_fail (view->vadjustment == vadjustment);

    GschemPageGeometry *geometry = gschem_page_view_get_page_geometry (view);
    g_return_if_fail (geometry != NULL);

    current_bottom = geometry->viewport_bottom;
    new_bottom = view->page->toplevel->init_bottom - (int) vadjustment->value;

    geometry->viewport_bottom = new_bottom;
    geometry->viewport_top = geometry->viewport_top - (current_bottom - new_bottom);

    gschem_page_view_invalidate_all (view);
  }
}



/*! \brief Transform WORLD coordinates to SCREEN coordinates
 *
 *  A temporary function until a GschemToplevel is not required for coordinate
 *  conversions. See the function WORLDtoSCREEN.
 */
void
gschem_page_view_WORLDtoSCREEN (GschemPageView *view, int x, int y, int *px, int *py)
{
  GschemPageGeometry *geometry = gschem_page_view_get_page_geometry (view);

  g_return_if_fail (geometry != NULL);

  *px = gschem_page_geometry_pix_x (geometry, x);
  *py = gschem_page_geometry_pix_y (geometry, y);
}



/*! \brief Zoom the view to the extents of a set of objects
 *
 *  By providing a NULL for the objects parameter, this function will zoom to
 *  the extents of all objects in the drawing.
 *
 *  \param [in,out] view    This GschemPageView
 *  \param [in]     objects The list of objects to compute extents, or NULL
 */
void
gschem_page_view_zoom_extents (GschemPageView *view, const GList *objects)
{
  PAGE *page = NULL;
  GschemPageGeometry *geometry = NULL;
  const GList *temp = objects;

  g_return_if_fail (view != NULL);

  page = gschem_page_view_get_page (view);
  g_return_if_fail (page != NULL);

  geometry = gschem_page_view_get_page_geometry (view);
  g_return_if_fail (geometry != NULL);

  if (temp == NULL) {
    temp = s_page_objects (page);
  }

  gschem_page_geometry_zoom_extents (geometry, view->page->toplevel, temp, A_PAN_DONT_REDRAW);

  g_signal_emit_by_name (view, "update-grid-info");
  gschem_page_view_update_scroll_adjustments (view);
  gschem_page_view_invalidate_all (view);
}
