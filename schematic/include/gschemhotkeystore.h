/* Lepton EDA Schematic Capture
 * Copyright (C) 2013 Peter Brett <peter@peter-b.co.uk>
 * Copyright (C) 2017-2019 Lepton EDA Contributors
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA.
 */

#ifndef GSCHEM_HOTKEY_STORE_H__
#define GSCHEM_HOTKEY_STORE_H__

/* ---------------------------------------------------------------- */

/*! \class GschemHotkeyStore gschemhotkeystore.h "gschemhotkeystore.h"
 * \brief GtkTreeModel that contains keybinding data.
 *
 * A GtkListStore that contains a list of actions with
 * their icons and their current keybindings.
 */

#define GSCHEM_TYPE_HOTKEY_STORE (gschem_hotkey_store_get_type ())
#define GSCHEM_HOTKEY_STORE(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), GSCHEM_TYPE_HOTKEY_STORE, GschemHotkeyStore))
#define GSCHEM_HOTKEY_STORE_CLASS(klass) (G_TYPE_CHECK_CLASS_CAST ((klass), GSCHEM_TYPE_HOTKEY_STORE, GschemHotkeyStoreClass))
#define GSCHEM_IS_HOTKEY_STORE(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GSCHEM_TYPE_HOTKEY_STORE))
#define GSCHEM_IS_HOTKEY_STORE_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GSCHEM_TYPE_HOTKEY_STORE))
#define GSCHEM_HOTKEY_STORE_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), GSCHEM_TYPE_HOTKEY_STORE, GschemHotkeyStoreClass))


enum {
  GSCHEM_HOTKEY_STORE_COLUMN_ICON  = 0,
  GSCHEM_HOTKEY_STORE_COLUMN_LABEL,
  GSCHEM_HOTKEY_STORE_COLUMN_KEYS,
  GSCHEM_HOTKEY_STORE_NUM_COLUMNS
};


typedef struct _GschemHotkeyStoreClass GschemHotkeyStoreClass;
typedef struct _GschemHotkeyStore GschemHotkeyStore;

struct _GschemHotkeyStoreClass
{
  GtkListStoreClass parent_class;
};

struct _GschemHotkeyStore
{
  GtkListStore parent_instance;
};


GType gschem_hotkey_store_get_type();
GschemHotkeyStore* gschem_hotkey_store_new();


#endif /* GSCHEM_HOTKEY_STORE_H__ */

