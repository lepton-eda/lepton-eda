/* Lepton EDA Schematic Capture
 * Copyright (C) 2013 Peter Brett <peter@peter-b.co.uk>
 * Copyright (C) 2017-2025 Lepton EDA Contributors
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#ifndef _HOTKEY_STORE_H__
#define _HOTKEY_STORE_H__

/* ---------------------------------------------------------------- */

/*! \class SchematicHotkeyStore hotkey_store.h "hotkey_store.h"
 * \brief GtkTreeModel that contains keybinding data.
 *
 * A GtkListStore that contains a list of actions with
 * their icons and their current keybindings.
 */

#define SCHEMATIC_TYPE_HOTKEY_STORE (schematic_hotkey_store_get_type ())
#define SCHEMATIC_HOTKEY_STORE(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), SCHEMATIC_TYPE_HOTKEY_STORE, SchematicHotkeyStore))
#define SCHEMATIC_HOTKEY_STORE_CLASS(klass) (G_TYPE_CHECK_CLASS_CAST ((klass), SCHEMATIC_TYPE_HOTKEY_STORE, SchematicHotkeyStoreClass))
#define SCHEMATIC_IS_HOTKEY_STORE(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), SCHEMATIC_TYPE_HOTKEY_STORE))
#define SCHEMATIC_IS_HOTKEY_STORE_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), SCHEMATIC_TYPE_HOTKEY_STORE))
#define SCHEMATIC_HOTKEY_STORE_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), SCHEMATIC_TYPE_HOTKEY_STORE, SchematicHotkeyStoreClass))


enum {
  SCHEMATIC_HOTKEY_STORE_COLUMN_ICON  = 0,
  SCHEMATIC_HOTKEY_STORE_COLUMN_LABEL,
  SCHEMATIC_HOTKEY_STORE_COLUMN_KEYS,
  SCHEMATIC_HOTKEY_STORE_NUM_COLUMNS
};


typedef struct _SchematicHotkeyStoreClass SchematicHotkeyStoreClass;
typedef struct _SchematicHotkeyStore SchematicHotkeyStore;

struct _SchematicHotkeyStoreClass
{
  GtkListStoreClass parent_class;
};

struct _SchematicHotkeyStore
{
  GtkListStore parent_instance;
};


GType
schematic_hotkey_store_get_type();

G_BEGIN_DECLS

SchematicHotkeyStore*
schematic_hotkey_store_new ();

G_END_DECLS

#endif /* _HOTKEY_STORE_H__ */
