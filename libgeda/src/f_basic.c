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
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <config.h>
#include <stdio.h> 

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>


#include <guile/gh.h>

#include "struct.h"
#include "defines.h"
#include "globals.h"  

#include "../include/prototype.h"


void
f_open(TOPLEVEL *w_current, char *filename)
{
	/* has the head been freed yet? */
	/* probably not hack PAGE */

	set_window(w_current, w_current->init_left, w_current->init_right,
                   w_current->init_top, w_current->init_bottom);

	w_current->page_current->object_tail = (OBJECT *) 
			o_read(w_current, w_current->page_current->object_tail, filename);

	if (w_current->page_current->object_tail != NULL) {
		s_log_message("Opened schematic [%s]\n", filename);
	}	

        w_current->page_current->object_tail = (OBJECT *) 
			return_tail(w_current->page_current->object_head); 

/* CONNECTION stuff 
	o_CONN_recalc_all(w_current, w_current->page_current->object_head);
*/

	/* new ALES stuff */
	o_ales_disconnect_update(w_current->page_current);
	

	w_current->page_current->zoom_factor = 0;

	w_current->page_current->CHANGED=0; /* added 4/7/98 */
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
