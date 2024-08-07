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



#define SCHEMATIC_TYPE_SELECTION_ADAPTER           (schematic_selection_adapter_get_type())
#define SCHEMATIC_SELECTION_ADAPTER(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), SCHEMATIC_TYPE_SELECTION_ADAPTER, SchematicSelectionAdapter))
#define SCHEMATIC_SELECTION_ADAPTER_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  SCHEMATIC_TYPE_SELECTION_ADAPTER, SchematicSelectionAdapterClass))
#define SCHEMATIC_IS_SELECTION_ADAPTER(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), SCHEMATIC_TYPE_SELECTION_ADAPTER))
#define SCHEMATIC_SELECTION_ADAPTER_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), SCHEMATIC_TYPE_SELECTION_ADAPTER, SchematicSelectionAdapterClass))

typedef struct _SchematicSelectionAdapterClass SchematicSelectionAdapterClass;
typedef struct _SchematicSelectionAdapter SchematicSelectionAdapter;

struct _SchematicSelectionAdapterClass
{
  GObjectClass parent_class;
};

struct _SchematicSelectionAdapter
{
  GObject parent;

  LeptonSelection *selection;
  LeptonToplevel *toplevel;
};

GType
schematic_selection_adapter_get_type();

int
schematic_selection_adapter_get_cap_style (SchematicSelectionAdapter *adapter);

int
schematic_selection_adapter_get_dash_length (SchematicSelectionAdapter *adapter);

int
schematic_selection_adapter_get_dash_space (SchematicSelectionAdapter *adapter);

int
schematic_selection_adapter_get_fill_angle1 (SchematicSelectionAdapter *adapter);

int
schematic_selection_adapter_get_fill_angle2 (SchematicSelectionAdapter *adapter);

int
schematic_selection_adapter_get_fill_pitch1 (SchematicSelectionAdapter *adapter);

int
schematic_selection_adapter_get_fill_pitch2 (SchematicSelectionAdapter *adapter);

int
schematic_selection_adapter_get_fill_type (SchematicSelectionAdapter *adapter);

int
schematic_selection_adapter_get_fill_width (SchematicSelectionAdapter *adapter);

int
schematic_selection_adapter_get_line_type (SchematicSelectionAdapter *adapter);

int
schematic_selection_adapter_get_line_width (SchematicSelectionAdapter *adapter);

int
schematic_selection_adapter_get_object_color (SchematicSelectionAdapter *adapter);

int
schematic_selection_adapter_get_pin_type (SchematicSelectionAdapter *adapter);

LeptonSelection *
schematic_selection_adapter_get_selection (SchematicSelectionAdapter *adapter);

int
schematic_selection_adapter_get_text_alignment (SchematicSelectionAdapter *adapter);

int
schematic_selection_adapter_get_text_color (SchematicSelectionAdapter *adapter);

int
schematic_selection_adapter_get_text_rotation (SchematicSelectionAdapter *adapter);

int
schematic_selection_adapter_get_text_size (SchematicSelectionAdapter *adapter);

const char*
schematic_selection_adapter_get_text_string (SchematicSelectionAdapter *adapter);

LeptonToplevel*
schematic_selection_adapter_get_toplevel (SchematicSelectionAdapter *adapter);

SchematicSelectionAdapter*
schematic_selection_adapter_new ();

void
schematic_selection_adapter_set_cap_style (SchematicSelectionAdapter *adapter,
                                           int cap_style);
void
schematic_selection_adapter_set_dash_length (SchematicSelectionAdapter *adapter,
                                             int dash_length);
void
schematic_selection_adapter_set_dash_space (SchematicSelectionAdapter *adapter,
                                            int dash_space);
void
schematic_selection_adapter_set_fill_angle1 (SchematicSelectionAdapter *adapter,
                                             int angle);
void
schematic_selection_adapter_set_fill_angle2 (SchematicSelectionAdapter *adapter,
                                             int angle);
void
schematic_selection_adapter_set_fill_pitch1 (SchematicSelectionAdapter *adapter,
                                             int pitch);
void
schematic_selection_adapter_set_fill_pitch2 (SchematicSelectionAdapter *adapter,
                                             int pitch);
void
schematic_selection_adapter_set_fill_type (SchematicSelectionAdapter *adapter,
                                           int line_type);
void
schematic_selection_adapter_set_fill_width (SchematicSelectionAdapter *adapter,
                                            int line_width);
void
schematic_selection_adapter_set_line_type (SchematicSelectionAdapter *adapter,
                                           int line_type);
void
schematic_selection_adapter_set_line_width (SchematicSelectionAdapter *adapter,
                                            int line_width);
void
schematic_selection_adapter_set_object_color (SchematicSelectionAdapter *adapter,
                                              int color);
void
schematic_selection_adapter_set_pin_type (SchematicSelectionAdapter *adapter,
                                          int type);
void
schematic_selection_adapter_set_selection (SchematicSelectionAdapter *adapter,
                                           LeptonSelection *selection);
void
schematic_selection_adapter_set_text_color (SchematicSelectionAdapter *adapter,
                                            int color);
void
schematic_selection_adapter_set_text_alignment (SchematicSelectionAdapter *adapter,
                                                int alignment);
void
gschem_selection_adapter_set_text_rotation (SchematicSelectionAdapter *adapter, int angle);

void
gschem_selection_adapter_set_text_size (SchematicSelectionAdapter *adapter, int size);

void
gschem_selection_adapter_set_text_string (SchematicSelectionAdapter *adapter,
                                          const char *string,
                                          SchematicWindow *w_current);
void
gschem_selection_adapter_set_toplevel (SchematicSelectionAdapter *adapter,
                                       LeptonToplevel *toplevel);
