/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2025 Lepton EDA Contributors
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

#include "schematic.h"


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  The object passed in should be the REAL object, NOT any copy in any
 *  selection list
 */
void
o_text_change (SchematicWindow *w_current,
               LeptonObject *object,
               char *string,
               int visibility,
               int show)
{
  g_return_if_fail (w_current != NULL);

  SchematicCanvas *page_view = schematic_window_get_current_canvas (w_current);
  g_return_if_fail (page_view != NULL);

  LeptonPage *page = schematic_canvas_get_page (page_view);

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

  schematic_window_active_page_changed (w_current);
}
