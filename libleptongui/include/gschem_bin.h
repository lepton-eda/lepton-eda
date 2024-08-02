/* Lepton EDA Schematic Capture
 * Copyright (C) 2015 gEDA Contributors
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
/*!
 * \file gschem_bin.h
 *
 * \brief
 */

#define SCHEMATIC_TYPE_BIN           (schematic_bin_get_type())
#define SCHEMATIC_BIN(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), SCHEMATIC_TYPE_BIN, SchematicBin))
#define SCHEMATIC_BIN_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  SCHEMATIC_TYPE_BIN, SchematicBinClass))
#define SCHEMATIC_IS_BIN(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), SCHEMATIC_TYPE_BIN))
#define SCHEMATIC_BIN_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj),  SCHEMATIC_TYPE_BIN, SchematicBinClass))


typedef struct _SchematicBinClass SchematicBinClass;
typedef struct _SchematicBin      SchematicBin;

struct _SchematicBinClass {
  GtkBinClass parent_class;
};

struct _SchematicBin {
  GtkBin parent_instance;
};


GType
schematic_bin_get_type (void);

SchematicBin*
schematic_bin_new ();
