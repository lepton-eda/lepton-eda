/* gEDA - GNU Electronic Design Automation
 * libgeda - include files
 * Copyright (C) 1998 Ales V. Hvezda
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
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 */

#ifndef X_STATES_H
#define X_STATES_H

#undef NONE

enum x_states { NONE, SELECT, DRAWLINE, DRAWBOX, MOVE, COPY, DRAWCIRCLE, ZOOM,
  PAN, DRAWNET, NETCONT, TEXTENTRY, DRAWPIN, DRAWARC, STARTDRAWNET, DRAWCOMP,
  SBOX, STARTPAN, STARTSELECT, STARTCOPY, STARTMOVE, ENDCOPY, ENDMOVE, 
  ENDLINE, ENDBOX, ENDCIRCLE, ENDARC, ENDPIN, ENDCOMP, DRAWATTRIB, ENDATTRIB,
  DRAWTEXT, ENDTEXT, ENDROTATEP, ENDMIRROR, ZOOMBOXSTART, ZOOMBOXEND, 
  STARTROUTENET, ENDROUTENET};

#endif
