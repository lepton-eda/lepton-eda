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

#include "gschem.h"
#include <gdk/gdkkeysyms.h>



enum
{
  PROP_0,
  PROP_PAGE
};



static void
get_property (GObject *object, guint param_id, GValue *value, GParamSpec *pspec);

static void
gschem_page_view_class_init (GschemPageViewClass *klasse);

static void
gschem_page_view_init (GschemPageView *view);

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
  GschemPageView *view = GSCHEM_PAGE_VIEW (object);

  switch (param_id) {
    case PROP_PAGE:
      g_value_set_pointer (value, gschem_page_view_get_page (view));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
  }
}



/*! \brief Initialize GschemPageView class
 *
 *  \param [in] klasse The class for the GschemPageView
 */
static void
gschem_page_view_class_init (GschemPageViewClass *klasse)
{
  G_OBJECT_CLASS (klasse)->get_property = get_property;
  G_OBJECT_CLASS (klasse)->set_property = set_property;

  g_object_class_install_property (G_OBJECT_CLASS (klasse),
                                   PROP_PAGE,
                                   g_param_spec_pointer ("page",
                                                         "Page",
                                                         "Page",
                                                         G_PARAM_READWRITE | G_PARAM_STATIC_STRINGS));
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



/*! \brief Initialize GschemPageView instance
 *
 *  \param [in,out] selection
 */
static void
gschem_page_view_init (GschemPageView *view)
{
  g_return_if_fail (view != NULL);

  view->page = NULL;
}



/*! \brief Create a new instanceof the GschemPageView
 *
 *  \return A new instanceof the GschemPageView
 */
GschemPageView*
gschem_page_view_new ()
{
  return GSCHEM_PAGE_VIEW (g_object_new (GSCHEM_TYPE_PAGE_VIEW, NULL));
}



/*! \brief Set page for this view
 *
 *  \param [in,out] view The view
 *  \param [in]     page The page
 */
void
gschem_page_view_set_page (GschemPageView *view, PAGE *page)
{
  g_return_if_fail (view != NULL);

  if (view->page != NULL) {
  }

  view->page = page;

  if (view->page != NULL) {
  }

  g_object_notify (G_OBJECT (view), "page");
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
  GschemPageView *view = GSCHEM_PAGE_VIEW (object);

  switch (param_id) {
    case PROP_PAGE:
      gschem_page_view_set_page (view, g_value_get_pointer (value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
  }
}
