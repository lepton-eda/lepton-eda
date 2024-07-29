/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales V. Hvezda
 * Copyright (C) 1998-2012 gEDA Contributors
 * Copyright (C) 2017-2024 Lepton EDA Contributors
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


#ifndef __SCHEMATIC_ACTION_H__
#define __SCHEMATIC_ACTION_H__


#define SCHEMATIC_TYPE_ACTION           (schematic_action_get_type())
#define SCHEMATIC_ACTION(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), SCHEMATIC_TYPE_ACTION, SchematicAction))
#define SCHEMATIC_ACTION_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  SCHEMATIC_TYPE_ACTION, SchematicActionClass))
#define SCHEMATIC_IS_ACTION(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), SCHEMATIC_TYPE_ACTION))
#define SCHEMATIC_ACTION_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj),  SCHEMATIC_TYPE_ACTION, SchematicActionClass))

typedef struct _SchematicActionClass SchematicActionClass;
typedef struct _SchematicAction      SchematicAction;


struct _SchematicActionClass {
  GtkActionClass parent_class;

};

struct _SchematicAction {
  GtkAction parent_instance;

  gchar *multikey_accel;
};


GType schematic_action_get_type (void);

SchematicAction *schematic_action_new     (const gchar *name,
                                           const gchar *label,
                                           const gchar *tooltip,
#ifdef ENABLE_GTK3
                                           const gchar *icon_name,
#else /* GTK2 */
                                           const gchar *stock_id,
#endif
                                           const gchar *multikey_accel);

#endif /* __SCHEMATIC_ACTION_H__ */
