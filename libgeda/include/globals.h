/* gEDA - GNU Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998 Ales V. Hvezda
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
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifndef _GLOBALS_H_INCL
#define _GLOBALS_H_INCL


#if 0 /* none of this is needed? */
/* color stuff */
extern GdkColormap *colormap; 
extern GdkVisual *visual; 

/* colors */
extern GdkColor white;
extern GdkColor black;
extern GdkColor red;
extern GdkColor green;
extern GdkColor blue;
extern GdkColor cyan;
extern GdkColor yellow;
extern GdkColor grey;
extern GdkColor grey90;
extern GdkColor darkgreen;
extern GdkColor darkred;
extern GdkColor darkyellow;
extern GdkColor darkcyan;
extern GdkColor darkblue;
extern GdkColor darkgrey; 

/* color structure */
extern COLOR colors[MAX_COLORS];

extern char rc_filename[256]; /* size is hack */
#endif

extern int logfile_fd;
extern int do_logging;
extern int logging_dest;

/* colors which are used in o_image */
extern int image_black;
extern int image_white;

#ifdef HAS_LIBGDGEDA
/* I hate to include an include inside an include (ha!) however, I don't */
/* see an easy way to get this to build right */
#include <gdgeda/gd.h>
extern gdImagePtr current_im_ptr;
#endif

#endif
