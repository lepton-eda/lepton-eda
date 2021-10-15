/* Lepton EDA library
 * Copyright (C) 1998, 1999, 2000 Kazu Hirata / Ales Hvezda
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2010, 2016 gEDA Contributors
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

#include "config.h"
#include "version.h"

#include "liblepton_priv.h"
#include "liblepton/libleptonguile.h"

/*! \brief Perform runtime initialization of liblepton library.
 *
 *  \par Function Description
 *  This function is responsible for making sure that any runtime
 *  initialization is done for all the liblepton routines. It should
 *  be called before any other liblepton functions are called.
 */
void liblepton_init(void)
{
#ifdef ENABLE_NLS
  /* Initialise gettext */
  bindtextdomain (LIBLEPTON_GETTEXT_DOMAIN, LOCALEDIR);
  bind_textdomain_codeset(LIBLEPTON_GETTEXT_DOMAIN, "UTF-8");
#endif

  eda_paths_init();

  s_clib_init();
  s_attrib_init();
  s_color_init();
}
