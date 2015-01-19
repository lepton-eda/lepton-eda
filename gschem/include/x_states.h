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
  NONE,         /*  0 */
  SELECT,       /*  1 */
  DRAWLINE,     /*  2 */
  DRAWBOX,      /*  3 */
  MOVE,         /*  4 */
  COPY,         /*  5 */
  DRAWCIRCLE,   /*  6 */
  ZOOM,         /*  7 */
  PAN,          /*  8 */
  DRAWNET,      /*  9 */
  NETCONT,      /* 10 */
  DRAWPIN,      /* 11 */
  DRAWARC,      /* 12 */
  STARTDRAWNET, /* 13 */
  DRAWCOMP,     /* 14 */
  SBOX,         /* 15 */
  STARTSELECT,  /* 16 */
  STARTCOPY,    /* 17 */
  STARTMOVE,    /* 18 */
  ENDCOPY,      /* 19 */
  ENDMOVE,      /* 20 */
  ENDLINE,      /* 21 */
  ENDBOX,       /* 22 */
  ENDCIRCLE,    /* 23 */
  ENDARC,       /* 24 */
  ENDPIN,       /* 25 */
  ENDCOMP,      /* 26 */
  DRAWTEXT,     /* 27 */
  ENDTEXT,      /* 28 */
  ENDROTATEP,   /* 29 */
  ENDMIRROR,    /* 30 */
  ZOOMBOXSTART, /* 31 */
  ZOOMBOXEND,   /* 32 */
  STARTROUTENET,/* 33 */
  ENDROUTENET,  /* 34 */
  DRAWBUS,      /* 35 */
  BUSCONT,      /* 36 */
  STARTDRAWBUS, /* 37 */
  STARTPASTE,   /* 38 */
  ENDPASTE,     /* 39 */
  GRIPS,        /* 40 */
  DRAWPICTURE,  /* 41 */
  ENDPICTURE,   /* 42 */
  MCOPY,        /* 43 */
  STARTMCOPY,   /* 44 */
  ENDMCOPY,     /* 45 */
  DRAWPATH,     /* 46 */
  PATHCONT,     /* 47 */
  ENDPATH,      /* 48 */
};


#endif
