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
 * \file gschem_integer_combo_box.h
 *
 * \brief A GtkComboBox with and entry for integer values.
 */

#define GSCHEM_TYPE_INTEGER_COMBO_BOX           (gschem_integer_combo_box_get_type())
#define GSCHEM_INTEGER_COMBO_BOX(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), GSCHEM_TYPE_INTEGER_COMBO_BOX, GschemIntegerComboBox))
#define GSCHEM_INTEGER_COMBO_BOX_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  GSCHEM_TYPE_INTEGER_COMBO_BOX, GschemIntegerComboBoxClass))
#define GSCHEM_IS_INTEGER_COMBO_BOX(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GSCHEM_TYPE_INTEGER_COMBO_BOX))
#define GSCHEM_INTEGER_COMBO_BOX_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), GSCHEM_TYPE_INTEGER_COMBO_BOX, GschemIntegerComboBoxClass))

typedef struct _GschemIntegerComboBoxClass GschemIntegerComboBoxClass;
typedef struct _GschemIntegerComboBox GschemIntegerComboBox;

struct _GschemIntegerComboBoxClass
{
#if GTK_CHECK_VERSION (2, 24, 0)
  GtkComboBoxClass parent_class;
#else
  GtkComboBoxEntryClass parent_class;
#endif
};

struct _GschemIntegerComboBox
{
#if GTK_CHECK_VERSION (2, 24, 0)
  GtkComboBox parent;
#else
  GtkComboBoxEntry parent;
#endif

  gboolean changed;
};

GtkEntry*
gschem_integer_combo_box_get_entry (GtkWidget *widget);

GType
gschem_integer_combo_box_get_type();

int
gschem_integer_combo_box_get_value (GtkWidget *widget);

GtkWidget*
gschem_integer_combo_box_new ();

void
gschem_integer_combo_box_set_model (GtkWidget *widget, GtkListStore *store);

void
gschem_integer_combo_box_set_value (GtkWidget *widget, int value);
