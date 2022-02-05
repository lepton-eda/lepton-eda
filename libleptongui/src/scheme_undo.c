/* Lepton EDA Schematic Capture
 * Copyright (C) 2017 dmn <graahnul.grom@gmail.com>
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#include <config.h>
#include "gschem.h"




static void
export_funcs_undo (void* unused)
{
  #include "scheme_undo.x"

  scm_c_export (NULL);
}



void
scheme_init_undo()
{
  scm_c_define_module ("schematic core undo",
                       &export_funcs_undo,
                       NULL);
}
