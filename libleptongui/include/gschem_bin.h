/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
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

#define GSCHEM_TYPE_BIN           (gschem_bin_get_type())
#define GSCHEM_BIN(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), GSCHEM_TYPE_BIN, GschemBin))
#define GSCHEM_BIN_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  GSCHEM_TYPE_BIN, GschemBinClass))
#define GSCHEM_IS_BIN(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GSCHEM_TYPE_BIN))
#define GSCHEM_BIN_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), GSCHEM_TYPE_BIN, GschemBinClass))


typedef struct _GschemBinClass GschemBinClass;
typedef struct _GschemBin      GschemBin;

struct _GschemBinClass {
  GtkBinClass parent_class;
};

struct _GschemBin {
  GtkBin parent_instance;
};


GType
gschem_bin_get_type (void);

GschemBin*
gschem_bin_new ();
