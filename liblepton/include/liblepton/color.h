/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
 * Copyright (C) 2017-2021 Lepton EDA Contributors
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

#ifndef LEPTON_COLOR_H_
#define LEPTON_COLOR_H_


G_BEGIN_DECLS

struct st_color
{
  guint8 r, g, b, a;
  gboolean enabled;
};

typedef struct st_color GedaColor;


gdouble
lepton_color_get_blue_double (const GedaColor *color);

gdouble
lepton_color_get_green_double (const GedaColor *color);

gdouble
lepton_color_get_red_double (const GedaColor *color);

gdouble
lepton_color_get_alpha_double (const GedaColor *color);


#define BACKGROUND_COLOR                0
#define PIN_COLOR                       1
#define NET_ENDPOINT_COLOR              2
#define GRAPHIC_COLOR                   3
#define NET_COLOR                       4
#define ATTRIBUTE_COLOR                 5
#define LOGIC_BUBBLE_COLOR              6
#define DOTS_GRID_COLOR                 7
#define DETACHED_ATTRIBUTE_COLOR        8
#define TEXT_COLOR                      9
#define BUS_COLOR                       10
#define SELECT_COLOR                    11
#define BOUNDINGBOX_COLOR               12
#define ZOOM_BOX_COLOR                  13
#define STROKE_COLOR                    14
#define LOCK_COLOR                      15
#define OUTPUT_BACKGROUND_COLOR         16
#define FREESTYLE1_COLOR                17
#define FREESTYLE2_COLOR                18
#define FREESTYLE3_COLOR                19
#define FREESTYLE4_COLOR                20
#define JUNCTION_COLOR                  21
#define MESH_GRID_MAJOR_COLOR           22
#define MESH_GRID_MINOR_COLOR           23


#define COLORS_COUNT 24

typedef GedaColor GedaColorMap[ COLORS_COUNT ];


size_t
colors_count();

gboolean
color_id_valid (size_t id);

size_t
default_color_id();

const GedaColor*
lepton_colormap_color_by_id (const GedaColor *color_map,
                             size_t id);

void
lepton_colormap_disable_color (GedaColor *color_map,
                               size_t id);
void
lepton_colormap_set_color (GedaColor *color_map,
                           size_t id,
                           guint8 r,
                           guint8 g,
                           guint8 b,
                           guint8 a);
void
lepton_color_map_init (GedaColorMap map);

void
s_color_init (void);

const gchar*
color_get_name (int color_index);

const char*
color_get_strname (int color_index);

GedaColor*
print_colors_array ();

G_END_DECLS


#endif /* LEPTON_COLOR_H_ */
