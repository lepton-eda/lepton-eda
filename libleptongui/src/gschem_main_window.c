/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2010 gEDA Contributors
 * Copyright (C) 2022-2024 Lepton EDA Contributors
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


G_DEFINE_TYPE (SchematicMainWindow,
               schematic_main_window,
               GTK_TYPE_WINDOW);

static void
schematic_main_window_class_init (SchematicMainWindowClass *klass);

static void
schematic_main_window_init (SchematicMainWindow *window);



/*! \brief Initialize SchematicMainWindow class
 *
 *  \param [in] klass The class for the SchematicMainWindow
 */
static void
schematic_main_window_class_init (SchematicMainWindowClass *klass)
{
}



/*! \brief Initialize SchematicMainWindow instance
 *
 *  \param [in,out] window The #SchematicMainWindow instance.
 */
static void
schematic_main_window_init (SchematicMainWindow *window)
{
}



/*! \brief Create a new instance of the SchematicMainWindow
 *
 *  \return A new instance of the SchematicMainWindow
 */
SchematicMainWindow*
gschem_main_window_new ()
{
  return SCHEMATIC_MAIN_WINDOW (g_object_new (SCHEMATIC_TYPE_MAIN_WINDOW,
                                              "type", GTK_WINDOW_TOPLEVEL,
                                              NULL));
}
