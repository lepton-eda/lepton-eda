/* Lepton EDA Schematic Load and Save utility
 * Copyright (C) 2002-2010 Ales Hvezda
 * Copyright (C) 2002-2015 gEDA Contributors
 * Copyright (C) 2017-2019 Lepton EDA Contributors
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA.
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

#include <liblepton/liblepton.h>

#include "../include/prototype.h"

/* If embed_mode is true, then embed all components in all pages, */
/* otherwise unembed all components in all pages */
void
s_util_embed(TOPLEVEL *pr_current, int embed_mode)
{
  GList *p_iter, *o_iter;

  for (p_iter = geda_list_get_glist (pr_current->pages);
       p_iter != NULL;
       p_iter = g_list_next (p_iter)) {
    PAGE *p_current = (PAGE*) p_iter->data;

    /* Cast removes const qualifier from return value of
     * s_page_objects() */
    for (o_iter = (GList *) s_page_objects (p_current);
         o_iter != NULL;
         o_iter = g_list_next (o_iter)) {

      OBJECT *o_current = (OBJECT*) o_iter->data;

      if (o_current->type == OBJ_COMPLEX ||
                o_current->type == OBJ_PICTURE) {
        if (embed_mode == TRUE) {
          o_embed(pr_current, o_current);
        } else {
          o_unembed(pr_current, o_current);
        }
      }

    }
  }
}
