/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2000 Ales V. Hvezda
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

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

#include "defines.h"
#include "struct.h"
#include "globals.h"  

#include "../include/prototype.h"


/* Returns 0 on failure and 1 on success */

int
f_open(TOPLEVEL *w_current, char *filename)
{
	int opened=FALSE;

	/* has the head been freed yet? */
	/* probably not hack PAGE */

	set_window(w_current, w_current->init_left, w_current->init_right,
                   w_current->init_top, w_current->init_bottom);

	w_current->page_current->object_tail = (OBJECT *) 
			o_read(w_current, w_current->page_current->object_tail, 
		               filename);

	if (w_current->page_current->object_tail != NULL) {
		s_log_message("Opened schematic [%s]\n", filename);
		opened = TRUE;
	} else {
	  	/* Failed to open page */
          	opened = FALSE;	 
	}

        w_current->page_current->object_tail = (OBJECT *) 
			return_tail(w_current->page_current->object_head); 

	/* make sure you init net_consolide to false (default) in all */
	/* programs */
	if (w_current->net_consolidate == TRUE) {	
		o_net_consolidate(w_current);
	}

	w_current->page_current->CHANGED=0; /* added 4/7/98 */

	if (!opened) {
		return (FALSE);
	} else {
		return (TRUE);
	}
}

void
f_close(TOPLEVEL *w_current)
{

}

void
f_save_close(TOPLEVEL *w_current, char *filename)
{
	o_save(w_current, filename);
	s_page_free(w_current, w_current->page_current);
}

void
f_save(TOPLEVEL *w_current, char *filename)
{
	o_save(w_current, filename);
}
