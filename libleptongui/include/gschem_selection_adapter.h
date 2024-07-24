/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
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
 * \file gschem_selection_adapter.h
 *
 * \brief
 */

/* Returned by properties when there are no items selected
 */
#define NO_SELECTION (-1)

/* Returned by properties when there are multiple values in the selection
 */
#define MULTIPLE_VALUES (-2)



#define GSCHEM_TYPE_SELECTION_ADAPTER           (gschem_selection_adapter_get_type())
#define GSCHEM_SELECTION_ADAPTER(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), GSCHEM_TYPE_SELECTION_ADAPTER, GschemSelectionAdapter))
#define GSCHEM_SELECTION_ADAPTER_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  GSCHEM_TYPE_SELECTION_ADAPTER, GschemSelectionAdapterClass))
#define GSCHEM_IS_SELECTION_ADAPTER(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GSCHEM_TYPE_SELECTION_ADAPTER))
#define GSCHEM_SELECTION_ADAPTER_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), GSCHEM_TYPE_SELECTION_ADAPTER, GschemSelectionAdapterClass))

typedef struct _GschemSelectionAdapterClass GschemSelectionAdapterClass;
typedef struct _GschemSelectionAdapter GschemSelectionAdapter;

struct _GschemSelectionAdapterClass
{
  GObjectClass parent_class;
};

struct _GschemSelectionAdapter
{
  GObject parent;

  LeptonSelection *selection;
  LeptonToplevel *toplevel;
};

GType
gschem_selection_adapter_get_type();

int
gschem_selection_adapter_get_cap_style (GschemSelectionAdapter *adapter);

int
gschem_selection_adapter_get_dash_length (GschemSelectionAdapter *adapter);

int
gschem_selection_adapter_get_dash_space (GschemSelectionAdapter *adapter);

int
gschem_selection_adapter_get_fill_angle1 (GschemSelectionAdapter *adapter);

int
gschem_selection_adapter_get_fill_angle2 (GschemSelectionAdapter *adapter);

int
gschem_selection_adapter_get_fill_pitch1 (GschemSelectionAdapter *adapter);

int
gschem_selection_adapter_get_fill_pitch2 (GschemSelectionAdapter *adapter);

int
gschem_selection_adapter_get_fill_type (GschemSelectionAdapter *adapter);

int
gschem_selection_adapter_get_fill_width (GschemSelectionAdapter *adapter);

int
gschem_selection_adapter_get_line_type (GschemSelectionAdapter *adapter);

int
gschem_selection_adapter_get_line_width (GschemSelectionAdapter *adapter);

int
gschem_selection_adapter_get_object_color (GschemSelectionAdapter *adapter);

int
gschem_selection_adapter_get_pin_type (GschemSelectionAdapter *adapter);

LeptonSelection *
gschem_selection_adapter_get_selection (GschemSelectionAdapter *adapter);

int
gschem_selection_adapter_get_text_alignment (GschemSelectionAdapter *adapter);

int
gschem_selection_adapter_get_text_color (GschemSelectionAdapter *adapter);

int
gschem_selection_adapter_get_text_rotation (GschemSelectionAdapter *adapter);

int
gschem_selection_adapter_get_text_size (GschemSelectionAdapter *adapter);

const char*
gschem_selection_adapter_get_text_string (GschemSelectionAdapter *adapter);

LeptonToplevel*
gschem_selection_adapter_get_toplevel (GschemSelectionAdapter *adapter);

GschemSelectionAdapter*
gschem_selection_adapter_new ();

void
gschem_selection_adapter_set_cap_style (GschemSelectionAdapter *adapter, int cap_style);

void
gschem_selection_adapter_set_dash_length (GschemSelectionAdapter *adapter, int dash_length);

void
gschem_selection_adapter_set_dash_space (GschemSelectionAdapter *adapter, int dash_space);

void
gschem_selection_adapter_set_fill_angle1 (GschemSelectionAdapter *adapter, int angle);

void
gschem_selection_adapter_set_fill_angle2 (GschemSelectionAdapter *adapter, int angle);

void
gschem_selection_adapter_set_fill_pitch1 (GschemSelectionAdapter *adapter, int pitch);

void
gschem_selection_adapter_set_fill_pitch2 (GschemSelectionAdapter *adapter, int pitch);

void
gschem_selection_adapter_set_fill_type (GschemSelectionAdapter *adapter, int line_type);

void
gschem_selection_adapter_set_fill_width (GschemSelectionAdapter *adapter, int line_width);

void
gschem_selection_adapter_set_line_type (GschemSelectionAdapter *adapter, int line_type);

void
gschem_selection_adapter_set_line_width (GschemSelectionAdapter *adapter, int line_width);

void
gschem_selection_adapter_set_object_color (GschemSelectionAdapter *adapter, int color);

void
gschem_selection_adapter_set_pin_type (GschemSelectionAdapter *adapter, int type);

void
gschem_selection_adapter_set_selection (GschemSelectionAdapter *adapter,
                                        LeptonSelection *selection);

void
gschem_selection_adapter_set_text_color (GschemSelectionAdapter *adapter, int color);

void
gschem_selection_adapter_set_text_alignment (GschemSelectionAdapter *adapter, int alignment);

void
gschem_selection_adapter_set_text_rotation (GschemSelectionAdapter *adapter, int angle);

void
gschem_selection_adapter_set_text_size (GschemSelectionAdapter *adapter, int size);

void
gschem_selection_adapter_set_text_string (GschemSelectionAdapter *adapter,
                                          const char *string,
                                          SchematicWindow *w_current);
void
gschem_selection_adapter_set_toplevel (GschemSelectionAdapter *adapter,
                                       LeptonToplevel *toplevel);
