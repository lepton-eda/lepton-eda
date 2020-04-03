/* Lepton EDA Schematic Capture
 * Copyright (C) 2017 dmn <graahnul.grom@gmail.com>
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#include "gschem.h"



/*! \brief Saves current state onto the undo stack
 *
 *  \return SCM_BOOL_T on success, SCM_BOOL_F otherwise
 */
SCM_DEFINE (undo_save_state, "%undo-save-state", 0, 0, 0,
            (), "Saves current state onto the undo stack")
{
  GschemToplevel* w_current = g_current_window();

  GschemPageView* view = gschem_toplevel_get_current_page_view (w_current);
  g_return_val_if_fail (view != NULL, SCM_BOOL_F);

  GedaPage* page = gschem_page_view_get_page (view);
  g_return_val_if_fail (page != NULL, SCM_BOOL_F);

  o_undo_savestate (w_current, page, UNDO_ALL);

  return SCM_BOOL_T;
}




static void
export_funcs_undo (void* unused)
{
  #include "scheme_undo.x"

  scm_c_export (s_undo_save_state, NULL);
}



void
scheme_init_undo()
{
  scm_c_define_module ("schematic core undo",
                       &export_funcs_undo,
                       NULL);
}

