/* gEDA - GPL Electronic Design Automation
 * gattrib - gEDA gEDA component and net attribute manipulation using spreadsheet.
 * Copyright (C) 1998-2008 Ales V. Hvezda
 * Copyright (C) 2003-2008 Stuart D. Brorson.
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

/* --- This is necessary for i_basic.c --- */
#include "../include/x_states.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif


/* ------------------------------------------------------------- *
 *
 * ------------------------------------------------------------- */
#if 0 /* not used, but leaving it here in case we need it later */
static void i_update_status(TOPLEVEL * w_current, const char *string)
{
  if (!w_current->status_label) {
    return;
  }

  if (string) {
    /* NOTE: consider optimizing this if same label */
    gtk_label_set(GTK_LABEL(w_current->status_label), (char *) string);
  }
}
#endif


