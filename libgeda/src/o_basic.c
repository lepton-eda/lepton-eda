/* Lots of Gross code... needs lots of cleanup */
/* mainly readability issues */

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

#include <config.h>
#include <stdio.h>

/* instrumentation code */
#if 0
#include <sys/time.h>
#include <unistd.h>
#endif

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

#include "struct.h"
#include "defines.h"
#include "globals.h"
#include "o_types.h"
#include "colors.h"

#include "../include/prototype.h"

int 
inside_region(int left, int top, int right, int bottom, int x, int y)
{
	return ((x >= left && x <= right && y >= top && y <= bottom) ? 1 : 0);
}

void
o_redraw_single(TOPLEVEL *w_current, OBJECT *o_current)
{
	if (o_current == NULL)
		return;
	
	if (w_current->DONT_REDRAW) /* highly experimental */
		return;

	if (o_current->draw_func != NULL && o_current->type != OBJ_HEAD) {
		w_current->inside_redraw = 1;
		(*o_current->draw_func)(w_current, o_current);
		w_current->inside_redraw = 0;
	}
}

void
o_recalc(TOPLEVEL *w_current, OBJECT *object_list)
{
	OBJECT *o_current;

	if (object_list == NULL)
		return;
	
	o_current = object_list;
	while (o_current != NULL) {
		switch(o_current->type) {

			case(OBJ_LINE):
				o_line_recalc(w_current, o_current);
			break;

			case(OBJ_NET):
				o_net_recalc(w_current, o_current);
			break;

			case(OBJ_BOX):
				o_box_recalc(w_current, o_current);
			break;

			case(OBJ_CIRCLE):
				o_circle_recalc(w_current, o_current);
			break;

			case(OBJ_COMPLEX):
				o_complex_recalc(w_current, o_current);
			break;

			case(OBJ_PIN):
				o_pin_recalc(w_current, o_current);
			break;

			case(OBJ_ARC):
				o_arc_recalc(w_current, o_current);
			break;
		}

		o_current = o_current->next;
	}
}

