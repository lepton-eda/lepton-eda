/* gEDA - GNU Electronic Design Automation
 * libgeda - include files
 * Copyright (C) 1998 Ales V. Hvezda
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


/* for color mechanism used in gschem */
#define MAX_COLORS 25

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

/* for visibility in st_objects */
#define INVISIBLE       0
#define VISIBLE         1

/* For pin and net styles */
#define THIN            0     
#define THICK           1

#undef max
#define max(a,b) ((a) > (b) ? (a) : (b))

#undef min
#define min(a,b) ((a) < (b) ? (a) : (b)) 

#define FREE        1
#define CONSTRAINED 2

/* flags to setup_saveas_file_selector() */
#define SAVEAS	0 /* next action after click is nothing */
#define QUIT	1 /* next action after click is quit */
#define OPEN	2 /* next action after click is open */
#define NEW	3 /* next action after click is open */
#define CLOSE	4 /* next action after click is open */

/* for s_clib_getfilename() */
#define OPEN_DIR	0
#define READ_DIR	1
#define CLOSE_DIR	2

/* for s_slib_search() */
#define SLIB_SEARCH_START	0
#define SLIB_SEARCH_NEXT	1
#define SLIB_SEARCH_DONE	2

/* for text cap style */
#define LOWER 0
#define UPPER 1
#define BOTH  2 

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
#define CONN_HEAD		-1	
#define INVALID			0	
#define CONN_NET		1
#define CONN_PIN		2
#define CONN_MIDPOINT		3
#define CONN_BUS		4
#define CONN_BUS_MIDPOINT	5

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

/* These modes are for actionfeedback_mode */
/* there's a hack in i_keypress.c dealing with the 0 and 1 (has to be these */
/* values */
#define OUTLINE         0 
#define BOUNDINGBOX     1 


/* there are modes for text-feedback */
#define ONLY_WHEN_READABLE	0
#define ALWAYS			1


/* These are for where status information goes */
#define LOG_WINDOW		0
#define STDOUT_TTY		1
#define BOTH_LOGWIN_STDOUT	2

/* list copying flags */
#define NORMAL			0
#define SELECTION		1

#define MILS_PER_INCH		1000 

/* for text_output */
#define VECTOR_FONTS		0
#define PS_FONTS		1

/* for print dialog box */
#define LIMITS			0
#define WINDOW			1

/* for output-capstyle */ 
#define BUTT_CAP 		0
#define ROUND_CAP 		1
#define SQUARE_CAP 		2

/* for print dialog box */
#define LANDSCAPE		0
#define PORTRAIT 		1

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

/* for selected_from */
#define DONTCARE		0
#define MENU			1
#define HOTKEY			2	

/* for o_net_orientation */
#define NEITHER			0
#define HORIZONTAL		1
#define VERTICAL		2
#define HORIZONTAL_ABOVE	3
#define HORIZONTAL_BELOW	4
#define VERTICAL_LEFT		5
#define VERTICAL_RIGHT		6


/* gnetlist: netlist_mode */
#define gEDA			0
#define SPICE			1
#define TANGO			2

/* gnetlist: net-naming-priority */
#define NET_ATTRIBUTE		0
#define LABEL_ATTRIBUTE		1

/* gschcheck: Error types */
#define NO_ERR                  0
#define FLOAT_NET               1
#define FLOAT_PIN               2
#define DUP_NET_NAME            4
