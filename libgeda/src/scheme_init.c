/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library - Scheme API
 * Copyright (C) 2010 Peter Brett <peter@peter-b.co.uk>
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

/*!
 * \file scheme_init.c
 * Scheme API initialisation
 */
#include <config.h>

#include "libgeda_priv.h"
#include "libgedaguile_priv.h"

/*! Non-zero if the Scheme API has been initialised. */
static int init_called = 0;

/*! \brief Initialise the Scheme API.
 * \par Function Description
 * Registers all modules, procedures and variables exported by the
 * libgeda Scheme API.
 */
void
edascm_init ()
{
  if (init_called) return;
  init_called = 1;

  edascm_init_smob ();
  edascm_init_toplevel ();
  edascm_init_object ();
}
