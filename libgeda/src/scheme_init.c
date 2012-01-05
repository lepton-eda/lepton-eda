/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library - Scheme API
 * Copyright (C) 2010-2012 Peter Brett <peter@peter-b.co.uk>
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
 * \brief Scheme API initialisation
 */
#include <config.h>

#include "libgeda_priv.h"
#include "libgedaguile_priv.h"

/*! Non-zero if the Scheme API has been initialised. */
static int init_called = 0;

SCM_GLOBAL_SYMBOL (edascm_object_state_sym, "object-state");

/*! \brief Scheme API initialisation worker function.
 * \par Function Description
 * Called by edascm_init() with current thread in Guile mode.
 */
static void *
edascm_init_impl (void *data)
{
  #include "scheme_init.x"

  edascm_init_smob ();
  edascm_init_toplevel ();
  edascm_init_object ();
  edascm_init_complex ();
  edascm_init_page ();
  edascm_init_attrib ();
  edascm_init_os ();
  edascm_init_deprecated ();
  return NULL;
}

/*! \brief Initialise the Scheme API.
 * \ingroup guile_c_iface
 * \par Function Description
 * Registers all modules, procedures and variables exported by the
 * libgeda Scheme API.
 */
void
edascm_init ()
{
  if (init_called) return;
  init_called = 1;

  scm_with_guile (edascm_init_impl, NULL);
}
