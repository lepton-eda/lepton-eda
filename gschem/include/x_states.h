/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License 
 * as published by the Free Software Foundation; either version 2 of 
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License 
 * along with this library; if not, write to the Free Software 
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#ifndef X_STATES_H
#define X_STATES_H

#undef NONE

/* NOTE: when adding states, also update i_status_string() function */

enum x_states { 
  NONE, 		/* 0 */
  SELECT, 		/* 1 */
  DRAWLINE, 		/* 2 */
  DRAWBOX, 		/* 3 */
  MOVE, 		/* 4 */
  COPY, 		/* 5 */
  DRAWCIRCLE, 		/* 6 */
  ZOOM,			/* 7 */
  PAN, 			/* 8 */
  DRAWNET, 		/* 9 */
  NETCONT, 		/* 10 */
  DRAWPIN, 		/* 11 */
  DRAWARC, 		/* 12 */
  STARTDRAWNET, 	/* 13 */
  DRAWCOMP,		/* 14 */
  SBOX, 		/* 15 */
  STARTPAN, 		/* 16 */
  STARTSELECT, 		/* 17 */
  STARTCOPY, 		/* 18 */
  STARTMOVE, 		/* 19 */
  ENDCOPY, 		/* 20 */
  ENDMOVE, 		/* 21 */
  ENDLINE, 		/* 22 */
  ENDBOX, 		/* 23 */
  ENDCIRCLE, 		/* 24 */
  ENDARC, 		/* 25 */
  ENDPIN, 		/* 26 */
  ENDCOMP, 		/* 27 */
  DRAWTEXT, 		/* 28 */
  ENDTEXT, 		/* 29 */
  ENDROTATEP, 		/* 30 */
  ENDMIRROR, 		/* 31 */
  ZOOMBOXSTART, 	/* 32 */
  ZOOMBOXEND, 		/* 33 */
  STARTROUTENET, 	/* 34 */
  ENDROUTENET, 		/* 35 */
  MOUSEPAN, 		/* 36 */
  DRAWBUS, 		/* 37 */
  BUSCONT, 		/* 38 */
  STARTDRAWBUS, 	/* 39 */
  STARTPASTE, 		/* 40 */
  ENDPASTE, 		/* 41 */
  GRIPS, 		/* 42 */
  DRAWPICTURE, 		/* 43 */
  ENDPICTURE, 		/* 44 */
  MCOPY, 		/* 45 */
  STARTMCOPY,		/* 46 */
  ENDMCOPY		/* 47 */
};


#endif
