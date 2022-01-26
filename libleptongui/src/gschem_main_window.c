/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2010 gEDA Contributors
 * Copyright (C) 2022 Lepton EDA Contributors
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
 * \file gschem_main_window.c
 *
 * \brief
 */

#include <config.h>
#include "gschem.h"


G_DEFINE_TYPE(GschemMainWindow, gschem_main_window, GTK_TYPE_WINDOW);


static void
gschem_main_window_class_init (GschemMainWindowClass *klass);

static void
gschem_main_window_init (GschemMainWindow *window);



/*! \brief Initialize GschemMainWindow class
 *
 *  \param [in] klass The class for the GschemMainWindow
 */
static void
gschem_main_window_class_init (GschemMainWindowClass *klass)
{
}



/*! \brief Initialize GschemMainWindow instance
 *
 *  \param [in,out] GschemMainWindow object
 */
static void
gschem_main_window_init (GschemMainWindow *window)
{
}



/*! \brief Create a new instance of the GschemMainWindow
 *
 *  \return A new instance of the GschemMainWindow
 */
GschemMainWindow*
gschem_main_window_new ()
{
  return GSCHEM_MAIN_WINDOW (g_object_new (GSCHEM_TYPE_MAIN_WINDOW,
                                           "type", GTK_WINDOW_TOPLEVEL,
                                           NULL));
}
