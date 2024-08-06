/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2013 gEDA Contributors
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
 * \file main_window.h
 *
 * \brief
 */

#define SCHEMATIC_TYPE_MAIN_WINDOW           (schematic_main_window_get_type())
#define SCHEMATIC_MAIN_WINDOW(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), SCHEMATIC_TYPE_MAIN_WINDOW, SchematicMainWindow))
#define SCHEMATIC_MAIN_WINDOW_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  SCHEMATIC_TYPE_MAIN_WINDOW, SchematicMainWindowClass))
#define SCHEMATIC_IS_MAIN_WINDOW(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), SCHEMATIC_TYPE_MAIN_WINDOW))
#define SCHEMATIC_MAIN_WINDOW_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), SCHEMATIC_TYPE_MAIN_WINDOW, SchematicMainWindowClass))

typedef struct _SchematicMainWindowClass SchematicMainWindowClass;
typedef struct _SchematicMainWindow SchematicMainWindow;

struct _SchematicMainWindowClass
{
  GtkWindowClass parent_class;
};

struct _SchematicMainWindow
{
  GtkWindow parent;
};

GType
schematic_main_window_get_type();

SchematicMainWindow*
schematic_main_window_new ();
