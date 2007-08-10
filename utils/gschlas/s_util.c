/* gEDA - GPL Electronic Design Automation
 * gschlas - gEDA Load and Save
 * Copyright (C) 2002-2007 Ales Hvezda
 * Copyright (C) 2002-2007 gEDA Contributors (see ChangeLog for details)
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
#include <sys/stat.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_ASSERT_H
#include <assert.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h> 
#endif

#include <libgeda/libgeda.h>

#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/* If embed_mode is true, then embed all components in all pages, */
/* otherwise unembed all components in all pages */
void
s_util_embed(TOPLEVEL *pr_current, int embed_mode)
{
    PAGE *p_current;
    OBJECT *o_current;

    p_current = pr_current->page_head;

    while (p_current != NULL) {

      o_current = p_current->object_head;

      if (p_current->pid != -1) {

        while (o_current != NULL) {

 	  if (o_current->type == OBJ_COMPLEX || 
              o_current->type == OBJ_PICTURE) {
	    if (embed_mode == TRUE) {
		o_embed(pr_current, o_current);
	    } else {
		o_unembed(pr_current, o_current);
            }
	  }

	  o_current = o_current->next;
	}
      }

      p_current = p_current->next;
    }
}

