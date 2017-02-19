/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
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
/*! \file geda_color_map.c
 *
 *  \brief Functions for working with coordinates
 */

#include <config.h>
#include <math.h>
#include <stdio.h>

#include "libgeda_priv.h"

GedaColorMap print_colors;

#define NOCOLOR {0xff, 0xff, 0xff, 0xff, FALSE}
#define WHITE   {0xff, 0xff, 0xff, 0xff, TRUE}
#define GRAY    {0x88, 0x88, 0x88, 0xff, TRUE}
#define BLACK   {0x00, 0x00, 0x00, 0xff, TRUE}
#define ENDMAP  {0x00, 0x00, 0x00, 0x00, FALSE}

static GedaColor default_colors[] = {
  WHITE,           /*  0: background         */
  BLACK,           /*  1: pin                */
  BLACK,           /*  2: net-endpoint       */
  BLACK,           /*  3: graphic            */
  BLACK,           /*  4: net                */
  BLACK,           /*  5: attribute          */
  BLACK,           /*  6: logic-bubble       */
  BLACK,           /*  7: dots-grid          */
  BLACK,           /*  8: detached-attribute */
  BLACK,           /*  9: text               */
  BLACK,           /* 10: bus                */
  GRAY,            /* 11: select             */
  GRAY,            /* 12: bounding-box       */
  GRAY,            /* 13: zoom-box           */
  GRAY,            /* 14: stroke             */
  BLACK,           /* 15: lock               */
  NOCOLOR,         /* 16: output-background  */
  NOCOLOR,         /* 17: freestyle1         */
  NOCOLOR,         /* 18: freestyle2         */
  NOCOLOR,         /* 19: freestyle3         */
  NOCOLOR,         /* 20: freestyle4         */
  BLACK,           /* 21: junction           */
  GRAY,            /* 22: mesh-grid-major    */
  NOCOLOR,         /* 23: mesh-grid-minor    */
  ENDMAP
};

/*! \brief Initialise a color map to B&W
 *  \par Function Description
 *  Initialises a color map to a simple default: black features on a
 *  white background, with "special" colors as gray.
 *
 *  \warning \a map must be have length of at least #MAX_COLORS.
 *
 *  \param map Color map to initialise.
 */
void
geda_color_map_init (GedaColorMap map)
{
  int i;
  gboolean reached_end = FALSE;
  GedaColor c;
  for (i = 0; i < MAX_COLORS; i++) {
    if (reached_end) {
      map[i].enabled = FALSE;
      continue;
    }
    c = default_colors[i];
    if (c.a == 0) { /* Check for end of default map */
      reached_end = TRUE;
      i--;
      continue;
    }
    map[i] = c;
  }
}

/*! \brief Get a color from the color map
 *
 *  \param [in] map The color map
 *  \param [in] index The index of the color
 *  \returns The color
 */
GedaColor*
geda_color_map_get_color (GedaColorMap map, int index)
{
  g_return_val_if_fail (index >= 0, &map[DEFAULT_COLOR]);
  g_return_val_if_fail (index < MAX_COLORS, &map[DEFAULT_COLOR]);
  g_return_val_if_fail (map[index].enabled, &map[DEFAULT_COLOR]);

  return &map[index];
}

/*! \brief Initialises the color subsystem
 *  \par Function Description
 *  At the moment, just initialises the print color map.
 */
void
s_color_init(void)
{
  geda_color_map_init (print_colors);
}

