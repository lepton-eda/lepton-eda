/* Lepton EDA Schematic Capture
 * Copyright (C) 2017-2019 dmn <graahnul.grom@gmail.com>
 * Copyright (C) 2017-2026 Lepton EDA Contributors
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
 * \file x_widgets.c
 *
 * \brief Widgets management
 *
 * lepton-schematic widgets:
 *
 * - object properties    (ShematicObjectPropertiesWidget)[right dock]
 * - text properties      (SchematicTextPropertiesWidget) [right dock]
 * - options              (SchematicOptionsWidget)        [right dock]
 *
 * - find text results    (SchematicFindTextState)        [bottom dock]
 * - log                  (SchematicLogWidget)            [bottom dock]
 *
 * - page manager         (PageSelectWidget)
 * - font selector        (FontSelectWidget)
 * - color scheme editor  (ColorEditWidget)
 *
 */

#include "config.h"
#include "schematic.h"




static gboolean
g_x_widgets_use_docks = FALSE;

static gboolean
g_x_widgets_use_toplevel_windows = FALSE;



gboolean x_widgets_use_docks()
{
  return g_x_widgets_use_docks;
}



gboolean
x_widgets_use_toplevel_windows()
{
  return !x_widgets_use_docks() && g_x_widgets_use_toplevel_windows;
}



/*! \brief Initialize widgets management
 *
 * \par Function Description
 *
 * Read widgets configuration.
 * Call this before any other functions from this file.
 *
 * Configuration settings for widgets:
 *
 * key:   use-docks
 * group: schematic.gui
 * type:  boolean
 * default value: false
 * description: whether to use docking GUI
 *
 * key:   use-toplevel-windows
 * group: schematic.gui
 * type:  boolean
 * default value: false
 * description: when docking GUI is off, whether to display
 *              widgets as toplevel windows (true) or
 *              dialogs (false)
 */
void x_widgets_init()
{
  static gsize initialized = 0;

  if (g_once_init_enter (&initialized))
  {
    cfg_read_bool ("schematic.gui", "use-docks",
                   FALSE, &g_x_widgets_use_docks);

    cfg_read_bool ("schematic.gui", "use-toplevel-windows",
                   FALSE, &g_x_widgets_use_toplevel_windows);

    g_once_init_leave (&initialized, 1);
  }
}
