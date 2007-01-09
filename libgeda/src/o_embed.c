/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2007 Ales V. Hvezda
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
#include <libguile.h>

#ifdef HAS_LIBGD
#include <gd.h>
#endif
#include <libgen.h>

#include "defines.h"
#include "struct.h"
#include "globals.h"
#include "o_types.h"
#include "funcs.h"
#include "colors.h"

#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 * 
 */
void o_embed(TOPLEVEL *w_current, OBJECT *o_current)
{
  gchar *new_basename;

  /* check o_current is a complex and is not already embedded */
  if (o_current->type == OBJ_COMPLEX &&
      !o_complex_is_embedded (o_current))
  {
    /* change the clib of complex to "EMBEDDED" */
    if (o_current->complex_clib) {
      g_free (o_current->complex_clib);
    }
    o_current->complex_clib = g_strdup ("EMBEDDED");

    /* change the basename to "EMBEDDED"+basename */
    new_basename = g_strconcat ("EMBEDDED",
                                o_current->complex_basename,
                                NULL);
    g_free (o_current->complex_basename);
    o_current->complex_basename = new_basename;

    s_log_message ("Component [%s] has been embedded\n",
                   o_current->complex_basename + 8);
    
    /* page content has been modified */
    w_current->page_current->CHANGED = 1;
  }

  /* If it's a picture and it's not embedded */
  if ( (o_current->type == OBJ_PICTURE) &&
       (o_current->picture->embedded == 0) ) {

    o_current->picture->embedded = 1;
    
    s_log_message ("Picture [%s] has been embedded\n",
		   basename(o_current->picture->filename));
    
    
    /* page content has been modified */
    w_current->page_current->CHANGED = 1;
  }
  
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 * 
 */
void o_unembed(TOPLEVEL *w_current, OBJECT *o_current)
{
  gchar *new_basename, *new_clib;
  const GSList *clibs;
  
  /* check o_current is an embedded complex */
  if (o_current->type == OBJ_COMPLEX &&
      o_complex_is_embedded (o_current))
  {
    /* get ride of the EMBEDDED word in basename */
    new_basename = g_strdup (o_current->complex_basename + 8);
    
    /* search for the symbol in the library */
    clibs = s_clib_search_basename (new_basename);

    if (!clibs) {
      /* symbol not found in the symbol library: signal an error */
      s_log_message ("Could not find component [%s], while trying to unembed. Component is still embedded\n",
                     o_current->complex_basename + 8);
      
    } else {
      /* set the object new basename */
      g_free (o_current->complex_basename);
      o_current->complex_basename = new_basename;

      /* set the object new clib */
      g_free (o_current->complex_clib);
      if (g_slist_next (clibs)) {
        s_log_message ("More than one component found with name [%s]\n",
                       new_basename);
        /* PB: for now, use the first directory in clibs */
        /* PB: maybe open a dialog to select the right one? */
      }
      new_clib = g_strdup ((gchar*)clibs->data);
      o_current->complex_clib = new_clib;

      s_log_message ("Component [%s] has been successfully unembedded\n",
                     o_current->complex_basename);
      
      /* page content has been modified */
      w_current->page_current->CHANGED = 1;
      
    }
  }

  /* If it's a picture and it's embedded */
  if ( (o_current->type == OBJ_PICTURE) &&
       (o_current->picture->embedded == 1) ) {

    o_current->picture->embedded = 0;
    
    s_log_message ("Picture [%s] has been unembedded\n",
		   basename(o_current->picture->filename));
    
    
    /* page content has been modified */
    w_current->page_current->CHANGED = 1;
  }
  
}
