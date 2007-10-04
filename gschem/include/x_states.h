/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2004 Ales V. Hvezda
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
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
  TEXTENTRY, 		/* 11 */
  DRAWPIN, 		/* 12 */
  DRAWARC, 		/* 13 */
  STARTDRAWNET, 	/* 14 */
  DRAWCOMP,		/* 15 */
  SBOX, 		/* 16 */
  STARTPAN, 		/* 17 */
  STARTSELECT, 		/* 18 */
  STARTCOPY, 		/* 19 */
  STARTMOVE, 		/* 20 */
  ENDCOPY, 		/* 21 */
  ENDMOVE, 		/* 22 */
  ENDLINE, 		/* 23 */
  ENDBOX, 		/* 24 */
  ENDCIRCLE, 		/* 25 */
  ENDARC, 		/* 26 */
  ENDPIN, 		/* 27 */
  ENDCOMP, 		/* 28 */
  DRAWTEXT, 		/* 29 */
  ENDTEXT, 		/* 30 */
  ENDROTATEP, 		/* 31 */
  ENDMIRROR, 		/* 32 */
  ZOOMBOXSTART, 	/* 33 */
  ZOOMBOXEND, 		/* 34 */
  STARTROUTENET, 	/* 35 */
  ENDROUTENET, 		/* 36 */
  MOUSEPAN, 		/* 37 */
  DRAWBUS, 		/* 38 */
  BUSCONT, 		/* 39 */
  STARTDRAWBUS, 	/* 40 */
  STARTPASTE, 		/* 41 */
  ENDPASTE, 		/* 42 */
  GRIPS, 		/* 43 */
  DRAWPICTURE, 		/* 44 */
  ENDPICTURE, 		/* 45 */
  MCOPY, 		/* 46 */
  STARTMCOPY,		/* 47 */
  ENDMCOPY		/* 48 */
};


#endif
