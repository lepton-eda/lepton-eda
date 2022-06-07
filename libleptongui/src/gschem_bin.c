/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
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
 * \file gschem_bin.c
 *
 * \brief a container with one child
 *
 * The gschem subclass implements virtual methods needed for subclasses to
 * operate properly. These method implementations would also be common to many
 * derived classes.
 */

#include <config.h>
#include "gschem.h"


G_DEFINE_TYPE(GschemBin, gschem_bin, GTK_TYPE_BIN);


static void
gschem_bin_class_init (GschemBinClass *klass);

static void
gschem_bin_init (GschemBin *klass);

static void
size_allocate (GtkWidget *widget, GtkAllocation *allocation);

#ifndef ENABLE_GTK3
static void
size_request (GtkWidget *widget, GtkRequisition *requisition);
#endif



/*! \brief create a new status log widget
 *
 *  \return a new status log widget
 */
GschemBin*
gschem_bin_new ()
{
  return GSCHEM_BIN (g_object_new (GSCHEM_TYPE_BIN, NULL));
}



#ifdef ENABLE_GTK3
static void
lepton_bin_get_preferred_width (GtkWidget *widget,
                                gint *minimal_width,
                                gint *natural_width)
{
  GtkBin *bin = GTK_BIN (widget);
  GtkWidget *child = gtk_bin_get_child (bin);
  *minimal_width = 0;
  *natural_width = 0;

  if (gtk_widget_get_visible (child)) {
    gtk_widget_get_preferred_width (child,
                                    minimal_width,
                                    natural_width);
  }
}

static void
lepton_bin_get_preferred_height (GtkWidget *widget,
                                 gint *minimal_height,
                                 gint *natural_height)
{
  GtkBin *bin = GTK_BIN (widget);
  GtkWidget *child = gtk_bin_get_child (bin);
  *minimal_height = 0;
  *natural_height = 0;

  if (gtk_widget_get_visible (child)) {
    gtk_widget_get_preferred_height (child,
                                     minimal_height,
                                     natural_height);
  }
}
#endif

/*! \brief initialize class
 */
static void
gschem_bin_class_init (GschemBinClass *klass)
{
  GtkWidgetClass *widget_klass = GTK_WIDGET_CLASS (klass);

  g_return_if_fail (widget_klass != NULL);

  widget_klass->size_allocate = size_allocate;
#ifdef ENABLE_GTK3
  widget_klass->get_preferred_width = lepton_bin_get_preferred_width;
  widget_klass->get_preferred_height = lepton_bin_get_preferred_height;
#else
  widget_klass->size_request = size_request;
#endif
}

static void
gschem_bin_init (GschemBin *klass)
{
}

/*! \private
 *  \brief forward size allocation to child
 */
static void
size_allocate (GtkWidget *widget, GtkAllocation *allocation)
{
  GtkWidget *child = gtk_bin_get_child (GTK_BIN (widget));

  gtk_widget_size_allocate (child , allocation);
}

#ifndef ENABLE_GTK3
/*! \private
 *  \brief forward size request to child
 */
static void
size_request (GtkWidget *widget, GtkRequisition *requisition)
{
  GtkWidget *child = gtk_bin_get_child (GTK_BIN (widget));

  gtk_widget_size_request (child, requisition);
}
#endif
