/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
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

#include <libgen.h>

#include "libgeda_priv.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 * 
 */
void o_embed(TOPLEVEL *toplevel, OBJECT *o_current)
{

  /* check o_current is a complex and is not already embedded */
  if (o_current->type == OBJ_COMPLEX &&
      !o_complex_is_embedded (o_current))
  {

    /* set the embedded flag */
    o_current->complex_embedded = TRUE;

    s_log_message ("Component [%s] has been embedded\n",
                   o_current->complex_basename);
    
    /* page content has been modified */
    toplevel->page_current->CHANGED = 1;
  }

  /* If it's a picture and it's not embedded */
  if ( (o_current->type == OBJ_PICTURE) &&
       (o_current->picture->embedded == 0) ) {

    o_current->picture->embedded = 1;
    
    s_log_message ("Picture [%s] has been embedded\n",
		   basename(o_current->picture->filename));
    
    
    /* page content has been modified */
    toplevel->page_current->CHANGED = 1;
  }
  
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 * 
 */
void o_unembed(TOPLEVEL *toplevel, OBJECT *o_current)
{
  const CLibSymbol *sym;
  
  /* check o_current is an embedded complex */
  if (o_current->type == OBJ_COMPLEX &&
      o_complex_is_embedded (o_current))
  {
        
    /* search for the symbol in the library */
    sym = s_clib_get_symbol_by_name (o_current->complex_basename);

    if (sym == NULL) {
      /* symbol not found in the symbol library: signal an error */
      s_log_message ("Could not find component [%s], while trying to unembed. Component is still embedded\n",
                     o_current->complex_basename);
      
    } else {
      /* clear the embedded flag */
      o_current->complex_embedded = FALSE;

      s_log_message ("Component [%s] has been successfully unembedded\n",
                     o_current->complex_basename);
      
      /* page content has been modified */
      toplevel->page_current->CHANGED = 1;
      
    }
  }

  /* If it's a picture and it's embedded */
  if ( (o_current->type == OBJ_PICTURE) &&
       (o_current->picture->embedded == 1) ) {

    o_current->picture->embedded = 0;
    
    s_log_message ("Picture [%s] has been unembedded\n",
		   basename(o_current->picture->filename));
    
    
    /* page content has been modified */
    toplevel->page_current->CHANGED = 1;
  }
  
}
