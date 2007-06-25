/* gEDA - GPL Electronic Design Automation
 * gattrib -- gEDA component and net attribute manipulation using spreadsheet.
 * Copyright (C) 2003-2007 Stuart D. Brorson.
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

#include <config.h>

#include <stdio.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif



/*------------------------------------------------------------------
 * Gattrib specific includes.  Note that include order is important.
 *------------------------------------------------------------------*/
#include <libgeda/libgeda.h>       /* geda library fcns  */
#include "../include/struct.h"     /* typdef and struct declarations */
#include "../include/prototype.h"  /* function prototypes */
#include "../include/globals.h"

#include "../include/i_vars.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif



/*------------------------------------------------------------------
 * Define the vars we'll use later
 *------------------------------------------------------------------*/
int   default_graphic_color = GRAPHIC_COLOR;
int   default_text_color = TEXT_COLOR;
int   default_text_size = 10;
int   default_text_caps = LOWER;

int   default_attribute_color = ATTRIBUTE_COLOR;
int   default_paper_width = 11000; /* letter size */
int   default_paper_height = 85000;


/*------------------------------------------------------------------
 * This initializes the vars in pr_current.  It is copied from 
 * gnetlist.  It is called by the guile initializtion fcns.
 * It competes with i_window_vars_set.  This is badly architected!!
 *------------------------------------------------------------------*/
void i_vars_set(TOPLEVEL * pr_current)
{
    i_vars_libgeda_set(pr_current);
}


/*------------------------------------------------------------------
 * This initializes the vars in pr_current.  It is copied from 
 * gschem.  It is called from s_toplevel_init.
 *------------------------------------------------------------------*/
void i_window_vars_set(TOPLEVEL * pr_current)
{

  i_vars_libgeda_set(pr_current); 

  pr_current->graphic_color = default_graphic_color;
  pr_current->text_color = default_text_color;
  pr_current->text_size = default_text_size;
  pr_current->text_caps = default_text_caps;
  
  pr_current->attribute_color = default_attribute_color;
  pr_current->paper_width = default_paper_width;
  pr_current->paper_height = default_paper_height;
  
}
