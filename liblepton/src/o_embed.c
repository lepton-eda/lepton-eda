/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
 * Copyright (C) 2017-2021 Lepton EDA Contributors
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
 *  liblepton. Currently component objects are just marked to
 *  be embedded later. Picture objects are embedded immediately.
 *
 *  \param o_current The LeptonObject to embed
 */
void
o_embed (LeptonObject *o_current)
{
  int page_modified = 0;
  LeptonPage *page = NULL;

  page = o_get_page (o_current);

  /* check o_current is a component and is not already embedded */
  if (o_current->type == OBJ_COMPONENT &&
      !lepton_component_object_get_embedded (o_current))
  {

    /* set the embedded flag */
    o_current->component_embedded = TRUE;

    g_message (_("Component [%1$s] has been embedded."),
               o_current->component_basename);
    page_modified = 1;
  }

  /* If it's a picture and it's not embedded */
  if ( (o_current->type == OBJ_PICTURE) &&
       !o_picture_is_embedded (o_current) ) {
    o_picture_embed (o_current);

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
 *  liblepton structure. Component objects are just marked to
 *  be not embedded. Picture objects are unembedded immediately.
 *
 *  \param o_current The LeptonObject to unembed
 */
void
o_unembed (LeptonObject *o_current)
{
  const CLibSymbol *sym;
  int page_modified = 0;
  LeptonPage *page = NULL;

  page = o_get_page (o_current);

  /* check o_current is an embedded component */
  if (o_current->type == OBJ_COMPONENT &&
      lepton_component_object_get_embedded (o_current))
  {

    /* search for the symbol in the library */
    sym = s_clib_get_symbol_by_name (o_current->component_basename);

    if (sym == NULL) {
      /* symbol not found in the symbol library: signal an error */
      g_message (_("Could not find component [%1$s], while trying to "
                   "unembed. Component is still embedded."),
                 o_current->component_basename);

    } else {
      /* clear the embedded flag */
      o_current->component_embedded = FALSE;

      g_message (_("Component [%1$s] has been successfully unembedded."),
                 o_current->component_basename);

      page_modified = 1;
    }
  }

  /* If it's a picture and it's embedded */
  if ( (o_current->type == OBJ_PICTURE) &&
       o_picture_is_embedded (o_current)) {
    o_picture_unembed (o_current);

    page_modified = 1;
  }

  if (page_modified && page != NULL) {
    page->CHANGED = 1;
  }
}
