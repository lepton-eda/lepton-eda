/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's Library
 * Copyright (C) 1998-2004 Ales V. Hvezda
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
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifndef _DEFINES_H_INCL
#define _DEFINES_H_INCL

/* Current schematic/symbol file format */
#define FILEFORMAT_VERSION     1

/* release version which had file format changes */
/* New file format changes after 20030921 use the above version */
/* and not these #defines anymore. */
#define VERSION_20000220 20000220
#define VERSION_20000704 20000704
#define VERSION_20020825 20020825
#define VERSION_20030921 20030921
/* 20030921 wasn't a real version, just a MinGW test version, but it is */
/* out there */

/* Set this string to something interesting to create a custom */
/* version of gEDA/gaf.  This string is prepended to all messages that */
/* output the program's VERSION.  You would set this if you are creating a */
/* specific custom version of gEDA/gaf.  This string is also used to */
/* distinguish between stable and development versions of gEDA/gaf. */
/* For example, if you set this string  to "1.0-", the resulting output is: */
/* 1.0-20060906. */
#define PREPEND_VERSION_STRING "1.0-"


/* for color mechanism used in gschem */
#define MAX_COLORS 25

/* for new fileselection dialog box */
#define MAX_FILES 2048
#define MAX_DIRS 2048

#define ZOOM_OUT 0
#define ZOOM_IN 1
#define ZOOM_FULL 2

#define CONNECTION_NONE         0 /* this one is not used */
#define CONNECTION_REGULAR      1
#define CONNECTION_ROUND        2

#if 0 /* Comment this in if you don't have gtk 1.0.4 or greater */
#define TRUE    1
#define FALSE   0
#endif

/* X's obsession with *64 */
#define FULL_CIRCLE 360*64

/* for show_name_value in st_objects */
#define SHOW_NAME_VALUE         0
#define SHOW_VALUE              1
#define SHOW_NAME               2
#define LEAVE_NAME_VALUE_ALONE  -1

/* for visibility in st_objects */
#define INVISIBLE               0
#define VISIBLE                 1
#define LEAVE_VISIBILITY_ALONE -1

/* For pin and net styles */
#define THIN            0     
#define THICK           1

/* various thicknesses (in mils) */
#define NET_WIDTH	10
#define PIN_WIDTH	10
#define LINE_WIDTH	10
#define BUS_WIDTH	30

/* various visual cue sizes (in mils) */
#define CUE_BOX_SIZE 		30
#define CUE_CIRCLE_SMALL_SIZE	30
#define CUE_CIRCLE_LARGE_SIZE	60

/* For grip size */
#define GRIP_SIZE1		25 
#define GRIP_SIZE2		50 
#define GRIP_SIZE3		80 
#define SMALL_ZOOMFACTOR1	150 
#define SMALL_ZOOMFACTOR2	30 


/* For text location on component not found graphics */
#define NOT_FOUND_TEXT_X	100
#define NOT_FOUND_TEXT_Y	100

/* for bus_ripper_type */
#define COMP_BUS_RIPPER         0
#define NET_BUS_RIPPER          1

/* for bus_ripper_rotation */
#define SYMMETRIC               0
#define NON_SYMMETRIC           1

#undef max
#define max(a,b) ((a) > (b) ? (a) : (b))

#undef min
#define min(a,b) ((a) < (b) ? (a) : (b)) 

#define FREE        1
#define CONSTRAINED 2

/* for attrib_edit_dialog invocation flag */
#define FROM_MENU		0
#define FROM_HOTKEY		1

/* for s_clib_getfilename() */
#define OPEN_DIR	0
#define READ_DIR	1
#define CLOSE_DIR	2
#define SET_COUNT	3

/* for s_slib_search() */
#define SLIB_SEARCH_START	0
#define SLIB_SEARCH_NEXT	1
#define SLIB_SEARCH_DONE	2

/* for text cap style */
#define LOWER 0
#define UPPER 1
#define BOTH  2 

/* for text alignment */
/*   2 -- 5 -- 8  */
/*   |    |    |  */
/*   1 -- 4 -- 7  */
/*   |    |    |  */
/*   0 -- 3 -- 6  */
#define LOWER_LEFT	0
#define MIDDLE_LEFT	1
#define UPPER_LEFT	2
#define LOWER_MIDDLE	3
#define MIDDLE_MIDDLE	4
#define UPPER_MIDDLE	5
#define LOWER_RIGHT	6
#define MIDDLE_RIGHT	7
#define UPPER_RIGHT	8

/* maximum single line length when reading in text strings */
#define MAX_TEXT_LINE_LENGTH  1024

/* one character string used to calculate tab's width */
/* Warning: it MUST be a string. */
#define TAB_CHAR_MODEL "b"

/* multi text line spacing (multipled times character height) */
#define LINE_SPACING    2.0

/* These modes are for net_endpoint_mode */
#define NONE		0
#define FILLEDBOX	1
#define EMPTYBOX	2
#define X		3

/* These modes are for net_midpoint_mode */
/* NONE also applies here */
#define FILLED	3
#define EMPTY 	4


/* The conn modes for type */
#define CONN_NULL               0
#define CONN_ENDPOINT		1
#define CONN_MIDPOINT		2

/* The conn whole type */
#define NO_MIDPOINT		0
#define HAS_MIDPOINT		1	
#define HAS_BUS_MIDPOINT	2	

/* The conn modes for visual_cue */
#define NO_CUE			1	
#define NET_DANGLING_CUE	2
#define PIN_DANGLING_CUE	3
#define BUS_DANGLING_CUE	4
#define MIDPOINT_CUE		5
#define BUS_MIDPOINT_CUE	6
#define INVALID_CUE		7	


/* used by world_tiles to set the size of the array */
#define MAX_TILES_X		10
#define MAX_TILES_Y		10

/* These modes are for actionfeedback_mode */
/* there's a hack in i_keypress.c dealing with the 0 and 1 (has to be these */
/* values */
#define OUTLINE         0 
#define BOUNDINGBOX     1 


/* there are modes for text-feedback */
#define ONLY_WHEN_READABLE	0
#define ALWAYS			1

/* used in o_undo_callback */
#define UNDO_ACTION		0
#define REDO_ACTION		1

/* used for undo_type */
#define UNDO_DISK		0
#define UNDO_MEMORY		1

/* used for undo_savestate flag */
#define UNDO_ALL		0
#define UNDO_VIEWPORT_ONLY	1

/* These are for where status information goes */
#define LOG_WINDOW		0
#define STDOUT_TTY		1
#define BOTH_LOGWIN_STDOUT	2

/* list copying flags */
#define NORMAL_FLAG		0
#define SELECTION_FLAG		1

/* hierarchy loading flags */
#define HIERARCHY_NORMAL_LOAD   0
#define HIERARCHY_FORCE_LOAD    1

/* hierarchy traversing flags */
#define HIERARCHY_NODUPS (1<<0)
#define HIERARCHY_POSTORDER (1<<1)
#define HIERARCHY_INNERLOOP (1<<7)

/* selection types */
/* used in o_select_object */
#define SINGLE                  0
#define MULTIPLE                1

#define MILS_PER_INCH		1000 

/* for text_output */
#define VECTOR_FONTS		0
#define PS_FONTS		1

/* for grid_mode */
#define GRID_VARIABLE_MODE	0
#define GRID_FIXED_MODE		1

/* for print dialog box */
#define EXTENTS			0
#define WINDOW			1
#define EXTENTS_NOMARGINS	2

/* for output-capstyle */ 
#define BUTT_CAP 		0
#define ROUND_CAP 		1
#define SQUARE_CAP 		2

/* for print dialog box */
#define LANDSCAPE		0
#define PORTRAIT 		1

/* for type to s_cue_output_all */
#define POSTSCRIPT		0
#define PNG			1

/* for log-window keyword */ 
#define MAP_LATER		0 
#define MAP_ON_STARTUP		1 

/* for log-window-type */
#define DECORATED		0 
#define TRANSIENT		1 

/* for third-mouse */
#define POPUP_ENABLED		0
#define MOUSEPAN_ENABLED	1

/* for middle-mouse */
#define STROKE			0
#define REPEAT			1
#define ACTION			2
#define MID_MOUSEPAN_ENABLED	3

/* for selected_from */
#define DONTCARE		0
#define MENU			1
#define HOTKEY			2	

/* for a_pan_general and a_zoom */
#define A_PAN_IGNORE_BORDERS 	1
#define A_PAN_DONT_REDRAW 	2

/* for o_net_orientation */
#define NEITHER			0
#define HORIZONTAL		1
#define VERTICAL		2
#define HORIZONTAL_ABOVE	3
#define HORIZONTAL_BELOW	4
#define VERTICAL_LEFT		5
#define VERTICAL_RIGHT		6

/* for pin_type */
#define PIN_TYPE_NET		0
#define PIN_TYPE_BUS		1

/* gnetlist: hierarchy_*_order */
#define APPEND			0
#define PREPEND			1

/* gnetlist: netlist_mode */
#define gEDA			0
#define SPICE			1
#define TANGO			2

/* gnetlist: net-naming-priority */
#define NETATTRIB_ATTRIBUTE	0
#define NETNAME_ATTRIBUTE	1

/* gschcheck: Error types */
#define NO_ERR                  0
#define FLOAT_NET               1
#define FLOAT_PIN               2
#define DUP_NET_NAME            4

/* Max level of symlinks */
#define MAX_LINK_LEVEL 256

#if defined(__MINGW32__) && !defined(M_PI)
#define M_PI  3.14159265358979323846
#endif

/* Logs a normal message. */
/* inspired of GLib's g_message() (gmessages.h) - LGPL */
#ifdef G_HAVE_ISO_VARARGS
#define s_log_message(...)  g_log (G_LOG_DOMAIN,         \
                                   G_LOG_LEVEL_MESSAGE,  \
                                   __VA_ARGS__)
#elif defined(G_HAVE_GNUC_VARARGS)
#define s_log_message(format...)    g_log (G_LOG_DOMAIN,         \
                                           G_LOG_LEVEL_MESSAGE,  \
                                           format)
#else   /* no varargs macros */
static void
s_log_message (const gchar *format,
               ...)
{
  va_list args;
  va_start (args, format);
  g_logv (G_LOG_DOMAIN, G_LOG_LEVEL_MESSAGE, format, args);
  va_end (args);
}
#endif  /* !__GNUC__ */

#endif

/* Backup filename creation string */
#define AUTOSAVE_BACKUP_FILENAME_STRING "#%s#"
