/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/*! \file o_embed.c
 *  \brief functions to embed and unembed symbols
 */

#include <config.h>

#include <stdio.h>

#include "libgeda_priv.h"


/*! \brief embed an object into a schematic
 *  \par Function Description
 *  This functions embeds an object \a o_current into a
 *  liblepton. Currently complex objects are just marked to
 *  be embedded later. Picture objects are embedded immediately.
 *
 *  \param o_current The OBJECT to embed
 */
void
o_embed (OBJECT *o_current)
{
  int page_modified = 0;
  PAGE *page = NULL;
  TOPLEVEL *toplevel = NULL;

  page = o_get_page (o_current);

  if (page != NULL) {
    toplevel = page->toplevel;
  }

  /* check o_current is a complex and is not already embedded */
  if (o_current->type == OBJ_COMPONENT &&
      !o_complex_is_embedded (o_current))
  {

    /* set the embedded flag */
    o_current->complex_embedded = TRUE;

    s_log_message (_("Component [%1$s] has been embedded."),
                   o_current->complex_basename);
    page_modified = 1;
  }

  /* If it's a picture and it's not embedded */
  if ( (o_current->type == OBJ_PICTURE) &&
       !o_picture_is_embedded (o_current) ) {
    o_picture_embed (toplevel, o_current);

    page_modified = 1;
  }

  if (page_modified && page != NULL) {
    /* page content has been modified */
    page->CHANGED = 1;
  }
}

/*! \brief unembed an object from a schematic
 *  \par Function Description
 *  This functions unembeds an object \a o_current from a
 *  liblepton structure. Complex objects are just marked to
 *  be not embedded. Picture objects are unembedded immediately.
 *
 *  \param o_current The OBJECT to unembed
 */
void
o_unembed (OBJECT *o_current)
{
  const CLibSymbol *sym;
  int page_modified = 0;
  PAGE *page = NULL;
  TOPLEVEL *toplevel = NULL;

  page = o_get_page (o_current);

  if (page != NULL) {
    toplevel = page->toplevel;
  }

  /* check o_current is an embedded complex */
  if (o_current->type == OBJ_COMPONENT &&
      o_complex_is_embedded (o_current))
  {

    /* search for the symbol in the library */
    sym = s_clib_get_symbol_by_name (o_current->complex_basename);

    if (sym == NULL) {
      /* symbol not found in the symbol library: signal an error */
      s_log_message (_("Could not find component [%1$s], while trying to "
                       "unembed. Component is still embedded."),
                     o_current->complex_basename);

    } else {
      /* clear the embedded flag */
      o_current->complex_embedded = FALSE;

      s_log_message (_("Component [%1$s] has been successfully unembedded."),
                     o_current->complex_basename);

      page_modified = 1;
    }
  }

  /* If it's a picture and it's embedded */
  if ( (o_current->type == OBJ_PICTURE) &&
       o_picture_is_embedded (o_current)) {
    o_picture_unembed (toplevel, o_current);

    page_modified = 1;
  }

  if (page_modified && page != NULL) {
    page->CHANGED = 1;
  }
}
