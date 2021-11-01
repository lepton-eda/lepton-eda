/* Lepton EDA Schematic Capture
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

#ifndef ENABLE_GTK3
#include "gtk/gtkmarshal.h"
#endif

#define INVALIDATE_MARGIN 1



enum
{
  PROP_0,
  PROP_HADJUSTMENT,
  PROP_PAGE,
  PROP_PAGE_GEOMETRY,
  PROP_VADJUSTMENT,
  PROP_SHOW_HIDDEN_TEXT,
#ifdef ENABLE_GTK3
  PROP_HSCROLL_POLICY,
  PROP_VSCROLL_POLICY
#endif
};


typedef void (*NotifyFunction) (void*,void*);

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
gschem_page_view_update_scroll_adjustments (GschemPageView *view);

static void
hadjustment_value_changed (GtkAdjustment *vadjustment, GschemPageView *view);

static void
set_property (GObject *object, guint param_id, const GValue *value, GParamSpec *pspec);

#ifndef ENABLE_GTK3
static void
set_scroll_adjustments (GschemPageView *view, GtkAdjustment *hadjustment, GtkAdjustment *vadjustment);
#endif

static void
vadjustment_value_changed (GtkAdjustment *vadjustment, GschemPageView *view);

static void geometry_cache_create (GschemPageView *view);

static
GschemPageGeometry *geometry_cache_lookup (const GschemPageView *view,
                                           const LeptonPage *page);
static void
geometry_cache_insert (GschemPageView *view,
                       LeptonPage *page,
                       GschemPageGeometry *geometry);

static void geometry_cache_dispose (GschemPageView *view);

static void geometry_cache_finalize (GschemPageView *view);

static GObjectClass *gschem_page_view_parent_class = NULL;


#ifndef ENABLE_GTK3
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
#endif


/*! \brief Dispose of the object
 */
static void
dispose (GObject *object)
{
  GschemPageView *view;

  g_return_if_fail (object != NULL);
  view = GSCHEM_PAGE_VIEW (object);
  g_return_if_fail (view != NULL);

  if (view->_page) {
    lepton_page_remove_weak_ptr (view->_page, &view->_page);
    view->_page = NULL;
  }

  gschem_page_view_set_hadjustment (view, NULL);
  gschem_page_view_set_vadjustment (view, NULL);

  geometry_cache_dispose (view);

  /* lastly, chain up to the parent dispose */

  g_return_if_fail (gschem_page_view_parent_class != NULL);
  gschem_page_view_parent_class->dispose (object);
}



/*! \brief Event handler for window realized
 */
static void
event_realize(GtkWidget *widget, gpointer unused)
{
  GschemPageView *view = GSCHEM_PAGE_VIEW(widget);
  GdkWindow *window = gtk_widget_get_window (widget);

  g_return_if_fail (view != NULL);
  g_return_if_fail (window != NULL);

  gtk_widget_get_allocation (widget, &(view->previous_allocation));
}



/*! \brief Event handler for window unrealized
 */
static void
event_unrealize(GtkWidget *widget, gpointer unused)
{
  GschemPageView *view = GSCHEM_PAGE_VIEW(widget);

  g_return_if_fail (view != NULL);
}


/*! \brief Event handler for window unrealized
 */
static void
event_toggle_hidden_text (GtkWidget *widget, gpointer unused)
{
  GschemPageView *view = GSCHEM_PAGE_VIEW (widget);

  g_return_if_fail (view != NULL);

  view->show_hidden_text = !view->show_hidden_text;
}


/*! \brief Finalize object
 */
static void
finalize (GObject *object)
{
  GschemPageView *view = GSCHEM_PAGE_VIEW (object);

  g_return_if_fail (view != NULL);

  geometry_cache_finalize (view);

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

    case PROP_SHOW_HIDDEN_TEXT:
      g_value_set_boolean (value, gschem_page_view_get_show_hidden_text (view));
      break;

#ifdef ENABLE_GTK3
    case PROP_HSCROLL_POLICY:
      g_value_set_enum (value, (gint) gschem_page_view_get_hscroll_policy (view));
      break;

    case PROP_VSCROLL_POLICY:
      g_value_set_enum (value, (gint) gschem_page_view_get_vscroll_policy (view));
      break;
#endif

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
                                                        (GParamFlags) (G_PARAM_READWRITE
                                                                       | G_PARAM_CONSTRUCT)));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_PAGE,
                                   g_param_spec_pointer ("page",
                                                         "Page",
                                                         "Page",
                                                         (GParamFlags) (G_PARAM_READWRITE
                                                                        | G_PARAM_STATIC_STRINGS)));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_PAGE_GEOMETRY,
                                   g_param_spec_boxed ("page-geometry",
                                                       "Page Geometry",
                                                       "Page Geometry",
                                                       GSCHEM_TYPE_PAGE_GEOMETRY,
                                                       (GParamFlags) (G_PARAM_READABLE
                                                                      | G_PARAM_STATIC_STRINGS)));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_VADJUSTMENT,
                                   g_param_spec_object ("vadjustment",
                                                        "Vertical adjustment",
                                                        "Vertical adjustment",
                                                        GTK_TYPE_ADJUSTMENT,
                                                        (GParamFlags) (G_PARAM_READWRITE
                                                                       | G_PARAM_CONSTRUCT)));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_SHOW_HIDDEN_TEXT,
                                   g_param_spec_boolean ("show-hidden-text",
                                                        "Show hidden text",
                                                        "Show hidden text in page-view",
                                                        FALSE,
                                                        (GParamFlags) (G_PARAM_READWRITE |
                                                                       G_PARAM_CONSTRUCT)));

#ifdef ENABLE_GTK3
  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_HSCROLL_POLICY,
                                   g_param_spec_enum ("hscroll-policy",
                                                      "hscroll-policy",
                                                      "hscroll-policy",
                                                      GTK_TYPE_SCROLLABLE_POLICY,
                                                      GTK_SCROLL_MINIMUM,
                                                      (GParamFlags) (G_PARAM_READWRITE |
                                                                     G_PARAM_CONSTRUCT)));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_VSCROLL_POLICY,
                                   g_param_spec_enum ("vscroll-policy",
                                                      "vscroll-policy",
                                                      "vscroll-policy",
                                                      GTK_TYPE_SCROLLABLE_POLICY,
                                                      GTK_SCROLL_MINIMUM,
                                                      (GParamFlags) (G_PARAM_READWRITE |
                                                                     G_PARAM_CONSTRUCT)));
#else
  GTK_WIDGET_CLASS (klass)->set_scroll_adjustments_signal = g_signal_new (
    "set-scroll-adjustments",
    G_OBJECT_CLASS_TYPE (klass),
    (GSignalFlags) (G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION),
    0,
    NULL,
    NULL,
    cclosure_marshal_VOID__OBJECT_OBJECT,
    G_TYPE_NONE,
    2,
    GTK_TYPE_ADJUSTMENT,
    GTK_TYPE_ADJUSTMENT);
#endif

  g_signal_new (
    "update-grid-info",
    G_OBJECT_CLASS_TYPE (klass),
    (GSignalFlags) (G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION),
    0,
    NULL,
    NULL,
    g_cclosure_marshal_VOID__VOID,
    G_TYPE_NONE,
    0);

  g_signal_new ("toggle-hidden-text",
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


#ifdef ENABLE_GTK3
/*! \brief Get the horizontal scrolling policy for this view
 *
 *  \param [in] view The view
 *  \return The horizontal scrolling policy for this view
 */
GtkScrollablePolicy
gschem_page_view_get_hscroll_policy (GschemPageView *view)
{
  g_return_val_if_fail (view != NULL, GTK_SCROLL_MINIMUM);

  return view->hscroll_policy;
}


/*! \brief Get the vertical scrolling policy for this view
 *
 *  \param [in] view The view
 *  \return The vertical scrolling policy for this view
 */
GtkScrollablePolicy
gschem_page_view_get_vscroll_policy (GschemPageView *view)
{
  g_return_val_if_fail (view != NULL, GTK_SCROLL_MINIMUM);

  return view->vscroll_policy;
}
#endif


/*! \brief Get page for this view
 *
 *  \param [in] view The view
 *  \return The page for the view
 */
LeptonPage*
gschem_page_view_get_page (GschemPageView *view)
{
  g_return_val_if_fail (view != NULL, NULL);

  return view->_page;
}



/*! \brief Get page geometry for this view
 *
 *  \param [in] view The view
 *  \return The page geometry for the view
 */
GschemPageGeometry*
gschem_page_view_get_page_geometry (GschemPageView *view)
{
  LeptonPage *page = NULL;
  GschemPageGeometry *geometry = NULL;
  int screen_width;
  int screen_height;

  g_return_val_if_fail (view != NULL, NULL);

  page = gschem_page_view_get_page (view);
  if (page == NULL) {
    return NULL;
  }

  GdkWindow *window = gtk_widget_get_window (GTK_WIDGET (view));
  /* If there's no window yet, defer geometry calculation until
   * later. */
#ifdef ENABLE_GTK3
  if (!GDK_IS_WINDOW (window))
    return NULL;
#else
  if (!GDK_IS_DRAWABLE (window))
    return NULL;
#endif

  geometry = geometry_cache_lookup (view, page);

  screen_width  = gdk_window_get_width  (window);
  screen_height = gdk_window_get_height (window);

  if (geometry == NULL) {
    geometry = gschem_page_geometry_new_with_values (screen_width,
                                                     screen_height,
                                                     WORLD_DEFAULT_LEFT,
                                                     WORLD_DEFAULT_TOP,
                                                     WORLD_DEFAULT_RIGHT,
                                                     WORLD_DEFAULT_BOTTOM,
                                                     WORLD_DEFAULT_LEFT,
                                                     WORLD_DEFAULT_TOP,
                                                     WORLD_DEFAULT_RIGHT,
                                                     WORLD_DEFAULT_BOTTOM);

    geometry_cache_insert (view, page, geometry);

    gschem_page_geometry_zoom_extents (geometry,
                                       s_page_objects (page),
                                       view->show_hidden_text);
  }
  else {

    int right = gschem_page_geometry_get_viewport_right (geometry);
    int left = gschem_page_geometry_get_viewport_left (geometry);
    double val1 = fabs ((double) (right - left) / screen_width);

    int top = gschem_page_geometry_get_viewport_top (geometry);
    int bottom = gschem_page_geometry_get_viewport_bottom (geometry);
    double val2 = fabs ((double) (top - bottom) / screen_height);

    double scale = MAX (val1, val2);

    gschem_page_geometry_set_values (geometry,
                                     scale,
                                     screen_width,
                                     screen_height,
                                     gschem_page_geometry_get_viewport_left (geometry),
                                     gschem_page_geometry_get_viewport_top (geometry),
                                     gschem_page_geometry_get_viewport_right (geometry),
                                     gschem_page_geometry_get_viewport_bottom (geometry));
  }

  return geometry;
}


gboolean
gschem_page_view_get_show_hidden_text (GschemPageView *view)
{
  g_return_val_if_fail (view != NULL, FALSE);

  return view->show_hidden_text;
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

    type = g_type_register_static (GTK_TYPE_DRAWING_AREA,
                                   "GschemPageView",
                                   &info,
                                   (GTypeFlags) 0);

#ifdef ENABLE_GTK3
    static const GInterfaceInfo scrollable_info =
      {
       (GInterfaceInitFunc) NULL,
       (GInterfaceFinalizeFunc) NULL,
       (gpointer) NULL
      };

    g_type_add_interface_static (type,
                                 GTK_TYPE_SCROLLABLE,
                                 &scrollable_info);
#endif
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

  /* this function can be called early during initialization */
  if (view == NULL) {
    return;
  }

  window = gtk_widget_get_window (GTK_WIDGET (view));

  if (window == NULL) {
    return;
  }

  gdk_window_invalidate_rect (window, NULL, FALSE);
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
  int screen_bottom = 0;
  int screen_right = 0;
  int screen_left = 0;
  int screen_top = 0;

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

#ifdef ENABLE_GTK3
  view->hscroll_policy = GTK_SCROLL_MINIMUM;
  view->vscroll_policy = GTK_SCROLL_MINIMUM;
#endif

  geometry_cache_create (view);

  view->_page = NULL;
  view->configured = FALSE;

  view->doing_pan = FALSE;
  view->pan_x = 0;
  view->pan_y = 0;
  view->throttle = 0;

#ifndef ENABLE_GTK3
  g_signal_connect (view,
                    "set-scroll-adjustments",
                    G_CALLBACK (set_scroll_adjustments),
                    NULL);

#endif
  g_signal_connect(view,
                   "realize",
                   G_CALLBACK (event_realize),
                   NULL);

  g_signal_connect(view,
                   "unrealize",
                   G_CALLBACK (event_unrealize),
                   NULL);

  g_signal_connect (view,
                   "toggle-hidden-text",
                   G_CALLBACK (event_toggle_hidden_text),
                   NULL);
}



/*! \brief Create a new instance of the GschemPageView
 *  \par Function Description
 *  This function creates a new instance of the GschemPageView
 *  structure. The resulting view becomes a "viewport" for the
 *  given \a page. If the page is not NULL, a weak reference
 *  callback is added for \a page so that it can do necessary
 *  clean-up for the view when the page is deleted (e.g. due to
 *  using close-page! Scheme function).
 *
 *  \param [in] page The page to refer to.
 *
 *  \return A new instance of the GschemPageView
 */
GschemPageView*
gschem_page_view_new_with_page (LeptonPage *page)
{
  GschemPageView *view = GSCHEM_PAGE_VIEW (g_object_new (GSCHEM_TYPE_PAGE_VIEW,
                                                         "page", page,
                                                         NULL));
  return view;
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
  GschemPageGeometry *geometry = NULL;

  g_return_if_fail (view != NULL);

  geometry = gschem_page_view_get_page_geometry (view);
  g_return_if_fail (geometry != NULL);

  /* make mouse to the new world-center;
     attention: there are information looses because of type cast in mil_x */

  gschem_page_geometry_pan_general (geometry, w_x, w_y, relativ_zoom_factor);

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
  /* Trigger a motion event to update the objects being drawn */
  /* This works e.g. if the view is centered at the mouse pointer position */
  x_event_faked_motion (view, NULL);

  gschem_page_view_update_scroll_adjustments (view);
  gschem_page_view_invalidate_all (view);
}



/*! \brief Pan the view by the given screen coordinate displacement
 *
 *  \param [in,out] view      This GschemPageView
 *  \param [in]     diff_x    The screen x coordinate displacement
 *  \param [in]     diff_y    The screen y coordinate displacement
 */
void
gschem_page_view_pan_mouse (GschemPageView *view, int diff_x, int diff_y)
{
  GschemPageGeometry *geometry = NULL;
  double world_cx, world_cy;
  double page_cx, page_cy;

  g_return_if_fail (view != NULL);

  geometry = gschem_page_view_get_page_geometry (view);
  g_return_if_fail (geometry != NULL);

#if DEBUG
  printf("gschem_page_view_pan_mouse(): diff_x=%1$d, diff_y=%2$d\n", diff_x, diff_y);
#endif

  page_cx = (gschem_page_geometry_get_viewport_left (geometry) + gschem_page_geometry_get_viewport_right (geometry)) / 2.0;
  page_cy = (gschem_page_geometry_get_viewport_top (geometry) + gschem_page_geometry_get_viewport_bottom (geometry)) / 2.0;

  world_cx = page_cx - gschem_page_view_WORLDabs (view, diff_x);
  world_cy = page_cy + gschem_page_view_WORLDabs (view, diff_y);

#if DEBUG
  printf("  world_cx=%1$f, world_cy=%2$f\n", world_cx, world_cy);
#endif

  gschem_page_view_pan_general (view, world_cx, world_cy, 1);

  /* Trigger a motion event to update the objects being drawn */
  /* Don't emit such an event if diffs are zero to avoid recursion */
  if (diff_x == 0 && diff_y == 0) {
    x_event_faked_motion (view, NULL);
  }
}



/*! \brief Start mouse panning in the view
 *  \par Function Description
 *  This function saves current coordinates of the mouse pointer
 *  to pan_x and pan_y  and toggles the view into pan mode.
 *
 *  \param [in,out] view  This GschemPageView
 *  \param [in]     x     The screen x coordinate
 *  \param [in]     y     The screen y coordinate
 */
void gschem_page_view_pan_start (GschemPageView *view, int x, int y)
{
  view->doing_pan = TRUE;
  view->pan_x = x;
  view->pan_y = y;
  view->throttle = 0;
}



/*! \brief Continue mouse panning in the view
 *  \par Function Description
 *  In the view pan mode, this function calculates displacement of
 *  the mouse pointer relative to its previous position and repans
 *  the view taking into account the given mouse pan gain setting.
 *  Then it replaces pan_x and pan_y with the new coordinates.
 *
 *  \param [in,out] view            This GschemPageView
 *  \param [in]     mousepan_gain   Mouse pan gain
 *  \param [in]     x               The new screen x coordinate
 *  \param [in]     y               The new screen y coordinate
 */
void
gschem_page_view_pan_motion (GschemPageView *view, int mousepan_gain, int x, int y)
{
  int pdiff_x, pdiff_y;

  if (view->doing_pan) {
    pdiff_x = x - view->pan_x;
    pdiff_y = y - view->pan_y;

    if (!(view->throttle % 5)) {
      gschem_page_view_pan_mouse(view,
                                 pdiff_x * mousepan_gain,
                                 pdiff_y * mousepan_gain);

      view->pan_x = x;
      view->pan_y = y;
    }
    view->throttle++;
  }
}

/*! \brief End mouse panning in the view
 *  \par Function Description
 *  This function resets the view pan mode and invalidates the
 *  view after panning.
 *
 *  \param [in,out] view      This GschemPageView
 *  \returns TRUE if panning has been finished, or FALSE if there was no panning
 */
gboolean
gschem_page_view_pan_end (GschemPageView *view)
{
  if (view->doing_pan) {
    gschem_page_view_invalidate_all (view);
    view->doing_pan = FALSE;
    return TRUE;
  } else {
    return FALSE;
  }
}



/*! \brief Transform SCREEN coordinates to WORLD coordinates
 *  \par Function Description
 *  This function takes in SCREEN x/y coordinates and
 *  transforms them to WORLD x/y coordinates.
 *
 *  \param [in]  view       The GschemPageView object.
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
                                          (gpointer) hadjustment_value_changed,
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
 *  \note
 *  Be careful when calling this function when tabbed GUI
 *  is enabled (see x_tabs.c) to not disrupt 1:1 relationship
 *  between page and page view objects which that code tries to maintain.
 *  Most likely you want to
 *  call x_window_set_current_page() or x_window_open_page() instead.
 *
 *  The toplevel property must be set and the page must belong to that
 *  toplevel.
 *
 *  \param [in,out] view The view
 *  \param [in]     page The page
 */
void
gschem_page_view_set_page (GschemPageView *view,
                           LeptonPage *page)
{
  g_return_if_fail (view != NULL);

  if (page != view->_page) {

    if (view->_page) {
      lepton_page_remove_weak_ptr (view->_page, &view->_page);
      view->_page = NULL;
    }

    if (page) {
      view->_page = page;
      lepton_page_add_weak_ptr (view->_page, &view->_page);

      g_return_if_fail (page->toplevel != NULL);
      lepton_toplevel_goto_page (page->toplevel, page);

      /* redraw the current page and update UI */
      gschem_page_view_invalidate_all (view);
      gschem_page_view_update_scroll_adjustments (view);

      g_object_notify (G_OBJECT (view), "page");
      g_object_notify (G_OBJECT (view), "page-geometry");
      g_signal_emit_by_name (view, "update-grid-info");

    } else {
      if (view->hadjustment != NULL) {
        gtk_adjustment_set_page_size (view->hadjustment,
                                      gtk_adjustment_get_upper (view->hadjustment));
      }
      if (view->vadjustment != NULL) {
        gtk_adjustment_set_page_size (view->vadjustment,
                                      gtk_adjustment_get_upper (view->vadjustment));
      }
    }
  }
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
                                          (gpointer) vadjustment_value_changed,
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


#ifdef ENABLE_GTK3
/*! \brief Set the horizontal scrolling policy for this view
 *
 *  \param [in] view The view
 */
void
gschem_page_view_set_hscroll_policy (GschemPageView *view, GtkScrollablePolicy policy)
{
  g_return_if_fail (view != NULL);

  view->hscroll_policy = policy;
}


/*! \brief Get the vertical scrolling policy for this view
 *
 *  \param [in] view The view
 */
void
gschem_page_view_set_vscroll_policy (GschemPageView *view, GtkScrollablePolicy policy)
{
  g_return_if_fail (view != NULL);

  view->vscroll_policy = policy;
}
#endif


void
gschem_page_view_set_show_hidden_text (GschemPageView *view,
                                       gboolean show_hidden_text)
{
  g_return_if_fail (view != NULL);
  view->show_hidden_text = show_hidden_text;
}


/*! \brief Signal handler for a horizontal scroll adjustment change
 */
static void
hadjustment_value_changed (GtkAdjustment *hadjustment, GschemPageView *view)
{
  g_return_if_fail (hadjustment != NULL);
  g_return_if_fail (view != NULL);

  GschemPageGeometry *geometry = gschem_page_view_get_page_geometry (view);

  if (view->hadjustment != NULL && geometry != NULL) {
    int current_left;
    int new_left;

    g_return_if_fail (view->hadjustment == hadjustment);

    current_left = gschem_page_geometry_get_viewport_left (geometry);
    new_left = (int) gtk_adjustment_get_value (hadjustment);

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
      gschem_page_view_set_hadjustment (view,
                                        GTK_ADJUSTMENT (g_value_get_object (value)));
      break;

    case PROP_PAGE:
      gschem_page_view_set_page (view, (LeptonPage*) g_value_get_pointer (value));
      break;

    case PROP_VADJUSTMENT:
      gschem_page_view_set_vadjustment (view,
                                        GTK_ADJUSTMENT (g_value_get_object (value)));
      break;

    case PROP_SHOW_HIDDEN_TEXT:
      gschem_page_view_set_show_hidden_text (view,
                                             g_value_get_boolean (value));
      break;

#ifdef ENABLE_GTK3
    case PROP_HSCROLL_POLICY:
      gschem_page_view_set_hscroll_policy (view, (GtkScrollablePolicy) g_value_get_enum (value));
      break;

    case PROP_VSCROLL_POLICY:
      gschem_page_view_set_vscroll_policy (view, (GtkScrollablePolicy) g_value_get_enum (value));
      break;
#endif

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
  }
}



/*! \brief Get absolute SCREEN value
 *
 *  \par Function Description
 *  Converts WORLD value \a val to absolute SCREEN value.
 *
 *  \param [in]     view       This GschemPageView
 *  \param [in]     val        The value to convert
 *  \return The converted value in SCREEN pixels
 */
int
gschem_page_view_SCREENabs(GschemPageView *view, int val)
{
  double f0,f1;
  double i;
  int j;
  GschemPageGeometry *geometry = gschem_page_view_get_page_geometry (view);

  g_return_val_if_fail (view != NULL, 0);

  if (geometry == NULL) return 0;

  f0 = gschem_page_geometry_get_viewport_left  (geometry);
  f1 = gschem_page_geometry_get_viewport_right (geometry);
  i = (double)(geometry->screen_width) * (double)(val) / (f1 - f0);

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

  GschemPageGeometry *geometry = gschem_page_view_get_page_geometry (view);

  if (view->hadjustment != NULL && geometry != NULL) {

    gtk_adjustment_set_page_increment (view->hadjustment,
                                       abs (geometry->viewport_right - geometry->viewport_left) - 100.0);

    gtk_adjustment_set_page_size (view->hadjustment,
                                  abs (geometry->viewport_right - geometry->viewport_left));

    gtk_adjustment_set_value (view->hadjustment,
                               geometry->viewport_left);

#if DEBUG
#ifdef ENABLE_GTK3
    printf ("Horizontal: "
            "lower=%f "
            "upper=%f "
            "page_size=%f "
            "page_increment=%f "
            "step_increment=%f "
            "value=%f\n",
            gtk_adjustment_get_lower (view->hadjustment),
            gtk_adjustment_get_upper (view->hadjustment),
            gtk_adjustment_get_page_size (view->hadjustment),
            gtk_adjustment_get_page_increment (view->hadjustment),
            gtk_adjustment_get_step_increment (view->hadjustment),
            gtk_adjustment_get_value (view->hadjustment));
#else /* GTK2 */
    printf("H %1$f %2$f\n", view->hadjustment->lower, view->hadjustment->upper);
    printf("Hp %1$f\n", view->hadjustment->page_size);
#endif
#endif

#ifndef ENABLE_GTK3
    gtk_adjustment_changed(view->hadjustment);
    gtk_adjustment_value_changed (view->hadjustment);
#endif
  }
}



/*! \brief Update the scroll adjustments
 */
static void
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

  GschemPageGeometry *geometry = gschem_page_view_get_page_geometry (view);

  if (view->vadjustment != NULL && geometry != NULL) {

    gtk_adjustment_set_page_increment(view->vadjustment,
                                      abs (geometry->viewport_bottom - geometry->viewport_top) - 100.0);

    gtk_adjustment_set_page_size (view->vadjustment,
                                  abs (geometry->viewport_bottom - geometry->viewport_top));

    gtk_adjustment_set_value(view->vadjustment,
                             geometry->world_bottom - geometry->viewport_bottom);

#if DEBUG
#ifdef ENABLE_GTK3
    printf ("Vertical: "
            "lower=%f "
            "upper=%f "
            "page_size=%f "
            "page_increment=%f "
            "step_increment=%f "
            "value=%f\n",
            gtk_adjustment_get_lower (view->vadjustment),
            gtk_adjustment_get_upper (view->vadjustment),
            gtk_adjustment_get_page_size (view->vadjustment),
            gtk_adjustment_get_page_increment (view->vadjustment),
            gtk_adjustment_get_step_increment (view->vadjustment),
            gtk_adjustment_get_value (view->vadjustment));
#else /* GTK2 */
    printf("V %1$f %2$f\n", view->vadjustment->lower, view->vadjustment->upper);
    printf("Vp %1$f\n", view->vadjustment->page_size);
#endif
#endif

#ifndef ENABLE_GTK3
    gtk_adjustment_changed(view->vadjustment);
    gtk_adjustment_value_changed (view->vadjustment);
#endif
  }
}


/*! \brief Get absolute WORLD coordinate.
 *  \par Function Description
 *  Get absolute WORLD coordinate.
 *
 *  \param [in,out] view The view
 *  \param [in]     val        The coordinate to convert.
 *  \return The converted WORLD coordinate.
 */
int
gschem_page_view_WORLDabs(GschemPageView *page_view, int val)
{
  GtkAllocation allocation;
  double fw0,fw1,fw,fval;
  double i;
  int j;

  GschemPageGeometry *geometry = gschem_page_view_get_page_geometry (page_view);

  gtk_widget_get_allocation (GTK_WIDGET(page_view), &allocation);

  fw1 = geometry->viewport_right;
  fw0 = geometry->viewport_left;
  fw  = allocation.width;
  fval = val;
  i = fval * (fw1 - fw0) / fw;

#ifdef HAS_RINT
  j = rint(i);
#else
  j = i;
#endif

  return(j);
}



#ifndef ENABLE_GTK3
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
#endif



/*! \brief Signal handler for a vertical scroll adjustment change
 */
static void
vadjustment_value_changed (GtkAdjustment *vadjustment, GschemPageView *view)
{
  g_return_if_fail (vadjustment != NULL);
  g_return_if_fail (view != NULL);

  GschemPageGeometry *geometry = gschem_page_view_get_page_geometry (view);

  if (view->vadjustment != NULL && geometry != NULL) {
    int current_bottom;
    int new_bottom;

    g_return_if_fail (view->vadjustment == vadjustment);

    current_bottom = geometry->viewport_bottom;
    new_bottom = geometry->world_bottom - (int) gtk_adjustment_get_value (vadjustment);

    geometry->viewport_bottom = new_bottom;
    geometry->viewport_top = geometry->viewport_top - (current_bottom - new_bottom);

    gschem_page_view_invalidate_all (view);
  }
}



/*! \brief Transform WORLD coordinates to SCREEN coordinates
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
  GschemPageGeometry *geometry = NULL;
  LeptonPage *page = NULL;
  const GList *temp = objects;

  g_return_if_fail (view != NULL);

  page = gschem_page_view_get_page (view);
  g_return_if_fail (page != NULL);

  geometry = gschem_page_view_get_page_geometry (view);
  g_return_if_fail (geometry != NULL);

  if (temp == NULL) {
    temp = s_page_objects (gschem_page_view_get_page (view));
  }

  gschem_page_geometry_zoom_extents (geometry, temp, view->show_hidden_text);

  /* Trigger a motion event to update the objects being drawn */
  x_event_faked_motion (view, NULL);

  g_signal_emit_by_name (view, "update-grid-info");
  gschem_page_view_update_scroll_adjustments (view);
  gschem_page_view_invalidate_all (view);
}

/*! \brief Zoom in on a single object
 *
 *  \param [in] view      This GschemPageView
 *  \param [in] object    The object
 */
void
gschem_page_view_zoom_object (GschemPageView *view, LeptonObject *object)
{
  int success;
  int x[2];
  int y[2];
  int viewport_center_x, viewport_center_y, viewport_width, viewport_height;
  double scale;

  g_return_if_fail (view != NULL);
  GschemPageGeometry *geometry = gschem_page_view_get_page_geometry (view);
  g_return_if_fail (geometry != NULL);

  g_return_if_fail (object != NULL);
  g_return_if_fail (object->page != NULL);
  g_return_if_fail (object->page->toplevel != NULL);

  success = lepton_object_calculate_visible_bounds (object,
                                                    view->show_hidden_text,
                                                    &x[0],
                                                    &y[0],
                                                    &x[1],
                                                    &y[1]);

  if (success) {

    /* Here we are trying to make the text screen height to be about */
    /* 50 pixels high, perhaps a future enhancement will be to make */
    /* this number configurable */
    viewport_center_x = (x[1] + x[0]) / 2;
    viewport_center_y = (y[1] + y[0]) / 2;

    /* .5 is scale to show really small objects like zero-sized pins:
    */
    scale = (y[1] - y[0]) / 50;

    if (scale == 0)
      scale = (x[1] - x[0]) / 50;

    if (scale == 0)
      scale = .5;

    viewport_height = geometry->screen_height * scale;
    viewport_width  = geometry->screen_width  * scale;

    gschem_page_geometry_set_values (geometry,
                                     scale,
                                     geometry->screen_width,
                                     geometry->screen_height,
                                     viewport_center_x - viewport_width / 2,
                                     viewport_center_y - viewport_height / 2,
                                     viewport_center_x + viewport_width / 2,
                                     viewport_center_y + viewport_height / 2);

    gschem_page_view_invalidate_all (view);
  }
}


/*! \brief Redraw page on the view
 *
 *  \param [in] view      The GschemPageView object which page to redraw
 */
void
gschem_page_view_redraw (GschemPageView *view,
#ifdef ENABLE_GTK3
                         cairo_t *cr,
#else
                         GdkEventExpose *event,
#endif
                         GschemToplevel *w_current)
{
  GschemPageGeometry *geometry;
  LeptonPage *page;

#if DEBUG
  printf("EXPOSE\n");
#endif

  g_return_if_fail (view != NULL);
  g_return_if_fail (w_current != NULL);

  page = gschem_page_view_get_page (view);

  if (page != NULL) {
    geometry = gschem_page_view_get_page_geometry (view);

    g_return_if_fail (view != NULL);

#ifdef ENABLE_GTK3
    o_redraw_rect (w_current,
                   GTK_WIDGET(view),
                   page,
                   geometry,
                   cr);
#else
    o_redraw_rect (w_current,
                   gtk_widget_get_window (GTK_WIDGET(view)),
                   page,
                   geometry,
                   &(event->area));
#endif
  }
}

static void
geometry_cache_page_weak_ref_notify (gpointer target,
                                     gpointer user_data)
{
  g_return_if_fail (target);
  g_return_if_fail (user_data);
  GschemPageView *view = GSCHEM_PAGE_VIEW (user_data);
  if (!view->_geometry_cache)
    return;
  g_hash_table_remove (view->_geometry_cache, target);
}

static void
geometry_cache_create (GschemPageView *view)
{
  g_return_if_fail (view && !view->_geometry_cache);

  view->_geometry_cache =
    g_hash_table_new_full (NULL, /* hash_func */
                           NULL, /* equal_func */
                           NULL, /* key_destroy_func */
                           (GDestroyNotify) gschem_page_geometry_free);
}

static GschemPageGeometry *
geometry_cache_lookup (const GschemPageView *view,
                       const LeptonPage *page)
{
  g_return_val_if_fail (view && view->_geometry_cache, NULL);
  g_return_val_if_fail (page, NULL);

  return (GschemPageGeometry*) g_hash_table_lookup (view->_geometry_cache, page);
}

static void
geometry_cache_insert (GschemPageView *view,
                       LeptonPage *page,
                       GschemPageGeometry *geometry)
{
  g_return_if_fail (view && view->_geometry_cache);
  g_return_if_fail (page);
  g_return_if_fail (geometry);
  g_return_if_fail (!g_hash_table_contains (view->_geometry_cache, page));

  lepton_page_weak_ref (page, geometry_cache_page_weak_ref_notify, view);
  g_hash_table_insert (view->_geometry_cache, page, geometry);
}

static gboolean
geometry_cache_dispose_func (gpointer key,
                             gpointer value,
                             gpointer user_data)
{
  lepton_page_weak_unref ((LeptonPage*) key,
                          geometry_cache_page_weak_ref_notify,
                          user_data);
  gschem_page_geometry_free ((GschemPageGeometry*) value);
  return TRUE;
}

static void
geometry_cache_dispose (GschemPageView *view)
{
  g_return_if_fail (view && view->_geometry_cache);
  g_hash_table_foreach_steal (view->_geometry_cache,
                              geometry_cache_dispose_func,
                              view);
}

static void
geometry_cache_finalize (GschemPageView *view)
{
  g_return_if_fail (view);
  if (!view->_geometry_cache)
    return;

  geometry_cache_dispose (view);
  g_hash_table_destroy (view->_geometry_cache);
  view->_geometry_cache = NULL;
}
