/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
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
#include <config.h>

#include <stdio.h>
#include <sys/stat.h>
#include <math.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "gschem.h"

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_text_prepare_place(GschemToplevel *w_current, char *text, int color, int align, int rotate, int size)
{
  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  LeptonPage *page = gschem_page_view_get_page (page_view);
  if (page == NULL) {
    return;
  }

  /* Insert the new object into the buffer at world coordinates (0,0).
   * It will be translated to the mouse coordinates during placement. */

  w_current->first_wx = 0;
  w_current->first_wy = 0;

  w_current->last_drawb_mode = LAST_DRAWB_MODE_NONE;

  /* remove the old place list if it exists */
  lepton_object_list_delete (page->place_list);
  page->place_list = NULL;

  /* here you need to add OBJ_TEXT when it's done */
  page->place_list =
    g_list_append(page->place_list,
                  lepton_text_object_new (color,
                                          0,
                                          0,
                                          align,
                                          rotate, /* zero is angle */
                                          text,
                                          size,
                                          /* has to be visible so you can place it */
                                          /* visibility is set when you create the object */
                                          VISIBLE,
                                          SHOW_NAME_VALUE));

  i_action_start (w_current);
  i_set_state (w_current, TEXTMODE);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  The object passed in should be the REAL object, NOT any copy in any
 *  selection list
 */
void o_text_change(GschemToplevel *w_current, LeptonObject *object, char *string,
                   int visibility, int show)
{
  g_return_if_fail (w_current != NULL);

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  LeptonPage *page = gschem_page_view_get_page (page_view);

  g_return_if_fail (page != NULL);

  if (object == NULL) {
    return;
  }

  if (!lepton_object_is_text (object))
  {
    return;
  }

  lepton_text_object_set_string (object, string);

  lepton_text_object_set_visibility (object, visibility);
  lepton_text_object_set_show (object, show);
  lepton_text_object_recreate (object);

  /* handle slot= attribute, it's a special case */
  LeptonObject *attachment = lepton_object_get_attached_to (object);
  if (attachment != NULL &&
      g_ascii_strncasecmp (string, "slot=", 5) == 0)
  {
    o_slot_end (w_current, attachment, string);
  }

  gschem_toplevel_page_content_changed (w_current, page);
}
