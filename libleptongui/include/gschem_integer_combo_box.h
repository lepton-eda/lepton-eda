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
 * \file gschem_integer_combo_box.h
 *
 * \brief A GtkComboBox with and entry for integer values.
 */

#define SCHEMATIC_TYPE_INTEGER_COMBO_BOX           (schematic_integer_combo_box_get_type())
#define SCHEMATIC_INTEGER_COMBO_BOX(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), SCHEMATIC_TYPE_INTEGER_COMBO_BOX, SchematicIntegerComboBox))
#define SCHEMATIC_INTEGER_COMBO_BOX_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  SCHEMATIC_TYPE_INTEGER_COMBO_BOX, SchematicIntegerComboBoxClass))
#define SCHEMATIC_IS_INTEGER_COMBO_BOX(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), SCHEMATIC_TYPE_INTEGER_COMBO_BOX))
#define SCHEMATIC_INTEGER_COMBO_BOX_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), SCHEMATIC_TYPE_INTEGER_COMBO_BOX, SchematicIntegerComboBoxClass))

typedef struct _SchematicIntegerComboBoxClass SchematicIntegerComboBoxClass;
typedef struct _SchematicIntegerComboBox SchematicIntegerComboBox;

struct _SchematicIntegerComboBoxClass
{
#if GTK_CHECK_VERSION (2, 24, 0)
  GtkComboBoxClass parent_class;
#else
  GtkComboBoxEntryClass parent_class;
#endif
};

struct _SchematicIntegerComboBox
{
#if GTK_CHECK_VERSION (2, 24, 0)
  GtkComboBox parent;
#else
  GtkComboBoxEntry parent;
#endif

  gboolean changed;
};

GtkEntry*
schematic_integer_combo_box_get_entry (GtkWidget *widget);

GType
schematic_integer_combo_box_get_type();

int
schematic_integer_combo_box_get_value (GtkWidget *widget);

GtkWidget*
schematic_integer_combo_box_new ();

void
schematic_integer_combo_box_set_model (GtkWidget *widget,
                                       GtkListStore *store);
void
schematic_integer_combo_box_set_value (GtkWidget *widget,
                                       int value);
