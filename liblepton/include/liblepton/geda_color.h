/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
 * Copyright (C) 2017-2020 Lepton EDA Contributors
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

#ifndef GEDA_COLOR_H_
#define GEDA_COLOR_H_


G_BEGIN_DECLS

struct st_color
{
  guint8 r, g, b, a;
  gboolean enabled;
};

typedef struct st_color GedaColor;


gdouble
geda_color_get_blue_double (const GedaColor *color);

gdouble
geda_color_get_green_double (const GedaColor *color);

gdouble
geda_color_get_red_double (const GedaColor *color);

gdouble
geda_color_get_alpha_double (const GedaColor *color);

gboolean
s_color_rgba_decode (const gchar *rgba, guchar *r, guchar *g, guchar *b, guchar *a);

gchar*
s_color_rgba_encode (guint8 r, guint8 g, guint8 b, guint8 a);


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

#define DEFAULT_COLOR                   GRAPHIC_COLOR

#define MAX_COLORS 25

typedef GedaColor GedaColorMap[MAX_COLORS];


/* Color map for printing */
extern GedaColorMap print_colors;


size_t
colors_count();


void
geda_color_map_init (GedaColorMap map);

GedaColor*
geda_color_map_get_color (GedaColorMap map, int index);

void
s_color_init (void);

void
s_color_map_from_scm (GedaColor *map, SCM lst, const char *scheme_proc_name);

SCM
s_color_map_to_scm (const GedaColor *map);


const gchar*
color_get_name (int color_index);

const char*
color_get_strname (int color_index);

G_END_DECLS


#endif /* GEDA_COLOR_H_ */
