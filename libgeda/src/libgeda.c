/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998, 1999, 2000 Kazu Hirata / Ales Hvezda
 * Copyright (C) 1998-2007 Ales Hvezda
 * Copyright (C) 1998-2007 gEDA Contributors (see ChangeLog for details)
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

#include "libgeda_priv.h"

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
  char *geda_data = getenv("GEDADATA");

  if (geda_data == NULL) {
    g_setenv ("GEDADATA", GEDADATADIR, FALSE);
  }

  /* Initialise gettext */
  bindtextdomain (GETTEXT_PACKAGE, LOCALEDIR);

  /* Initialise gobject */
  g_type_init ();

  s_clib_init();
  s_slib_init();
  s_menu_init();
  s_attrib_init();
  s_color_init();

  o_text_init(); 

  g_register_libgeda_funcs();
  g_register_libgeda_vars();

  g_init_object_smob();
  g_init_attrib_smob();
  g_init_page_smob();
}


