/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998, 1999, 2000 Kazu Hirata / Ales Hvezda
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
#ifdef HAVE_STRARG_H
#include <stdarg.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include <gtk/gtk.h>
#include <libguile.h>

#include "defines.h"
#include "struct.h"
#include "globals.h"

#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \brief Perform runtime initialization of libgeda library.
 *  \par Function Description
 *  This function is responsible for making sure that any runtime
 *  initialization is done for all the libgeda routines.  It should
 *  be called before any other libgeda functions are called.
 *
 */
void libgeda_init(void)
{
  char *new_data=NULL;
  char *geda_data = getenv("GEDADATA");

  /* This stuff reverted on 2.17.2005 by SDB in response to W. Hoch. */
  if (geda_data == NULL) {
    new_data = g_strdup_printf("GEDADATA=%s", GEDADATADIR);
    putenv(new_data);
    /*free(new_data); putenv takes over the memory? */

    /* We'll use this someday. . . . . */
    /* g_setenv ("GEDADATA", GEDADATADIR, FALSE); */ /* requires glib-2.4.* */
  }

  s_toplevel_init ();
  s_clib_init();
  s_slib_init();
  s_menu_init();
  s_attrib_init();
  s_color_init();

  o_text_init(); 

  g_register_libgeda_funcs();
}


