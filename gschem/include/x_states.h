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
  DRAWATTRIB, 		/* 29 */
  ENDATTRIB,		/* 30 */
  DRAWTEXT, 		/* 31 */
  ENDTEXT, 		/* 32 */
  ENDROTATEP, 		/* 33 */
  ENDMIRROR, 		/* 34 */
  ZOOMBOXSTART, 	/* 35 */
  ZOOMBOXEND, 		/* 36 */
  STARTROUTENET, 	/* 37 */
  ENDROUTENET, 		/* 38 */
  MOUSEPAN, 		/* 39 */
  DRAWBUS, 		/* 40 */
  BUSCONT, 		/* 41 */
  STARTDRAWBUS, 	/* 42 */
  STARTPASTE, 		/* 43 */
  ENDPASTE, 		/* 44 */
  GRIPS, 		/* 45 */
  DRAWPICTURE, 		/* 46 */
  ENDPICTURE, 		/* 47 */
  MCOPY, 		/* 48 */
  STARTMCOPY,		/* 49 */
  ENDMCOPY		/* 50 */
};


#endif
