/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2017 gEDA Contributors
 * Copyright (C) 2017-2019 Lepton EDA Contributors
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

#include "libgeda_priv.h"



/* \note
 * Kazu Hirata <kazu@seul.org> on July 16, 1999 - Added these absolute
 * defaults used when default_... is NULL.
 */


GPtrArray *default_always_promote_attributes = NULL;

int   default_attribute_promotion = TRUE;
int   default_promote_invisible = FALSE;
int   default_keep_invisible = TRUE;

int   default_make_backup_files = TRUE;



/*! \brief Initialize variables in TOPLEVEL object
 *  \par Function Description
 *  This function will initialize variables to default values.
 *
 *  \param [in] toplevel  The TOPLEVEL object to be updated.
 *
 */
void i_vars_libgeda_set(TOPLEVEL *toplevel)
{

  toplevel->attribute_promotion = default_attribute_promotion;
  toplevel->promote_invisible = default_promote_invisible;
  toplevel->keep_invisible = default_keep_invisible;

  toplevel->make_backup_files = default_make_backup_files;

  /* copy the always_promote_attributes list from the default */
  if (toplevel->always_promote_attributes) {
    g_ptr_array_unref (toplevel->always_promote_attributes);
    toplevel->always_promote_attributes = NULL;
  }
  if (default_always_promote_attributes) {
    toplevel->always_promote_attributes =
      g_ptr_array_ref (default_always_promote_attributes);
  }

}



/*! \brief Free default names
 *  \par Function Description
 *  This function will free all of the default variables for libgeda.
 *
 */
void i_vars_libgeda_freenames()
{
  g_ptr_array_unref (default_always_promote_attributes);
  default_always_promote_attributes = NULL;
}
