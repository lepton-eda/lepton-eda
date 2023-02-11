/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2014 gEDA Contributors
 * Copyright (C) 2017-2023 Lepton EDA Contributors
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

#ifndef _GSCHEM_DEFINES_H_INCL
#define _GSCHEM_DEFINES_H_INCL

/* Default extents of the schematic drawing area in world
 * coordinates. The negative values allow symbols, residing at the
 * origin, to be edited without translation to other coordinates.
 */
#define WORLD_DEFAULT_LEFT -60500
#define WORLD_DEFAULT_RIGHT 121000
#define WORLD_DEFAULT_BOTTOM 90750
#define WORLD_DEFAULT_TOP -45375

#define ZOOM_OUT 0
#define ZOOM_IN 1
#define ZOOM_FULL 2

#define ZOOM_EXTENTS_PADDING_PX 5

/* For grip size in pixels (i.e. device units) */
#define GRIP_SIZE               10.0

/* for bus_ripper_type */
#define COMP_BUS_RIPPER         0
#define NET_BUS_RIPPER          1

/* for bus_ripper_rotation */
#define SYMMETRIC               0
#define NON_SYMMETRIC           1

#define FREE        1
#define CONSTRAINED 2

/* for attrib_edit_dialog invocation flag */
#define FROM_MENU               0
#define FROM_HOTKEY             1

/* for text cap style */
#define LOWER 0
#define UPPER 1
#define BOTH  2

/* These modes are for actionfeedback_mode */
/* there's a hack in i_keypress.c dealing with the 0 and 1 (has to be these */
/* values */
#define OUTLINE         0
#define BOUNDINGBOX     1

/* used for undo_type */
#define UNDO_DISK               0
#define UNDO_MEMORY             1

/* selection types */
/* used in o_select_object */
#define SINGLE                  0
#define MULTIPLE                1

/* for dots_grid_mode */
#define DOTS_GRID_VARIABLE_MODE 0
#define DOTS_GRID_FIXED_MODE    1

/* mouse buttons actions */
#define MOUSEBTN_DO_STROKE   0
#define MOUSEBTN_DO_REPEAT   1
#define MOUSEBTN_DO_ACTION   2
#define MOUSEBTN_DO_POPUP    4
#define MOUSEBTN_DO_PAN      5

/* for scroll-wheel */
#define SCROLL_WHEEL_CLASSIC 0
#define SCROLL_WHEEL_GTK     1

/* for selected_from */
#define DONTCARE                0
#define MENU                    1
#define HOTKEY                  2

/* The prefix of the default filename used for newly created pages
 *
 * TRANSLATORS:
 *
 * This string is used to generate a filename for newly-created files.
 * It will be used to create a filename of the form "untitled_N.sch",
 * where N is a number.
 * Please make sure that the translation contains characters
 * suitable for use in a filename.
 *
 * */
#define UNTITLED_FILENAME_PREFIX _("untitled")

#endif /* !_GSCHEM_DEFINES_H_INCL */
