/* Lepton EDA Schematic Capture
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


static void
class_init (GschemBinClass *klass);

static void
size_allocate (GtkWidget *widget, GtkAllocation *allocation);

static void
size_request (GtkWidget *widget, GtkRequisition *requisition);


/*! \brief register/get class
 */
GType
gschem_bin_get_type ()
{
  static GType type = 0;

  if (type == 0) {
    static const GTypeInfo info = {
      sizeof(GschemBinClass),
      NULL,                                 /* base_init */
      NULL,                                 /* base_finalize */
      (GClassInitFunc) class_init,
      NULL,                                 /* class_finalize */
      NULL,                                 /* class_data */
      sizeof(GschemBin),
      0,                                    /* n_preallocs */
      NULL,
    };

    type = g_type_register_static (GTK_TYPE_BIN,
                                   "GschemBin",
                                   &info,
                                   (GTypeFlags) 0);
  }

  return type;
}


/*! \brief create a new status log widget
 *
 *  \return a new status log widget
 */
GschemBin*
gschem_bin_new ()
{
  return GSCHEM_BIN (g_object_new (GSCHEM_TYPE_BIN, NULL));
}


/*! \brief initialize class
 */
static void
class_init (GschemBinClass *klass)
{
  GtkWidgetClass *widget_klass = GTK_WIDGET_CLASS (klass);

  g_return_if_fail (widget_klass != NULL);

  widget_klass->size_allocate = size_allocate;
  widget_klass->size_request = size_request;
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


/*! \private
 *  \brief forward size request to child
 */
static void
size_request (GtkWidget *widget, GtkRequisition *requisition)
{
  GtkWidget *child = gtk_bin_get_child (GTK_BIN (widget));

  gtk_widget_size_request (child, requisition);
}
