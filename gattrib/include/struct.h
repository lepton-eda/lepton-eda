/* gEDA - GPL Electronic Design Automation
 * gattrib -- gEDA component and net attribute manipulation using spreadsheet.
 * Copyright (C) 2003 Stuart D. Brorson.
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
 */

/* ----------------------------------------------------------------- *
 *  This file holds definitions of the structures used in gattrib.
 * ----------------------------------------------------------------- */


#ifndef SHEET_DATA_STRUCT
#define SHEET_DATA_STRUCT

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkkeysyms.h>

#include <glib.h>
#ifdef HAS_GTK22
#include <glib-object.h>
#endif

/* -------  Includes needed to make the GTK stuff work  ------ */

#ifdef HAS_GTK22
#include "gtksheet_2_2.h"
#include "gtkitementry_2_2.h"
#else
#include "gtksheet_1_2.h"
#include "gtkitementry_1_2.h"
#endif

#include "pixmaps.h"

/* ========  Data structures used in processing below here  ========== */


/* ----------------------------------------------------------------- *
 *  This struct used in dealing with guile's read-in of the rc files.
 * ----------------------------------------------------------------- */
typedef struct {
  int m_val;
  char *m_str;
} vstbl_entry;


/* ----------------------------------------------------------------- *
 *  The sheet data hierarchy built by the prog should look like this:
 *  SHEET_DATA->(STRING_LIST *master_XXX_list)          // list of comps/nets/pins (row labels) 
 *            ->(STRING_LIST *master_XXX_attrib_list)   // list of attached names  (column labels)
 *            ->(TABLE *XXX_table)                      // table of attrib values (table entries)
 * ----------------------------------------------------------------- */
typedef struct st_sheet_data SHEET_DATA;
typedef struct st_table TABLE;
typedef struct st_string_list STRING_LIST;
typedef struct st_pin_list PIN_LIST;
typedef struct st_main_window MAIN_WINDOW;


/* -------------------------------------------------------------------- *
 * st_sheet_data defines SHEET_DATA, and holds master lists holding
 * sorted lists of comp/netlist names.  Also holds pointers to the heads
 * of the attribute-holding component and net structures.
 * -------------------------------------------------------------------- */
struct st_sheet_data {
  STRING_LIST *master_comp_list_head;         /* Sorted list of all component refdeses used in design */
  STRING_LIST *master_comp_attrib_list_head;  /* Sorted list of all component attribs used in design */
  int comp_count;                             /* This cannnot change -- user must edit design using gschem */
  int comp_attrib_count;                      /* This can change in this prog if the user adds attribs */

  STRING_LIST *master_net_list_head;          /* Sorted list of all net names used in design */
  STRING_LIST *master_net_attrib_list_head;   /* Sorted list of all net attribs used in design */
  int net_count;                              /* This cannnot change -- user must edit design using gschem */
  int net_attrib_count;                       /* This can change in this prog if the user adds attribs */

  STRING_LIST *master_pin_list_head;          /* Sorted list of all refdes:pin items used in design.   */
  STRING_LIST *master_pin_attrib_list_head;   /* Sorted list of all pin attribs used in design */
  int pin_count;                              /* This cannnot change -- user must edit design using gschem */
  int pin_attrib_count;                       /* This can change in this prog if the user adds attribs */

  TABLE **component_table;                    /* points to 2d array of component attribs */
  TABLE **net_table;                          /* points to 2d array of net attribs */
  TABLE **pin_table;                          /* points to 2d array of pin attribs */
};



/* -------------------------------------------------------------------- *
 * st_table defined what is held in a spreadsheet cell for both 
 * comp and net spreadsheets.  Holds pointer to individual comp/net name, and 
 * pointer to attrib list.  Ideally, the name pointer points to the 
 * refdes/netname string held in the TOPLEVEL data structure, so that 
 * when SHEET_DATA is manipulated, so is TOPLEVEL.
 * -------------------------------------------------------------------- */
#define ATTRIB_VIS_INVISIBLE 0
#define ATTRIB_VIS_VALUE_ONLY 1
#define ATTRIB_VIS_NAME_ONLY 2
#define ATTRIB_VIS_BOTH 3
struct st_table {
  int row;                       /* location on spreadsheet */
  int col;                       /* location on spreadsheet */
  gchar *row_name;               /* comp, net, or refdes:pin name */
  gchar *col_name;               /* attrib name */
  gchar *attrib_value;           /* attrib value */
  int visibility;         /* 0 = invisible, 1 = value only, 2 = name only, 
			   * 3 = both name & value visible  */

};


/* -------------------------------------------------------------------- *
 * STRING_LIST is a list of strings.  This struct is used for several 
 * different jobs, including serving as base class for master lists.
 * -------------------------------------------------------------------- */
struct st_string_list {
  gchar *data;      /* holds string */
  int pos;         /* pos on spreadsheet */
  int length;      /* number of items in list */
  STRING_LIST *prev;
  STRING_LIST *next;
};

/* -------------------------------------------------------------------- *
 * PIN_LIST is a special struct used for keeping track of pins.  Since
 * the master_pin_list must keep track of both refdes and pin, we need a 
 * special struct for pins.  Later processing will for a STRING_LIST 
 * of refdes:pinnumber pairs for insertion in the spreadsheet.
 * -------------------------------------------------------------------- */
struct st_pin_list {
  gchar *refdes;       /* holds refdes string */
  gint pinnumber;
  gchar *pinlabel;     /* holds pin label string */
  int pos;             /* pos on spreadsheet */
  int length;          /* number of items in list */
  PIN_LIST *prev;
  PIN_LIST *next;
};

#endif






