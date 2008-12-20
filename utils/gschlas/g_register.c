/* gEDA - GPL Electronic Design Automation
 * gschlas - gEDA Load and Save
 * Copyright (C) 2002-2008 Ales Hvezda
 * Copyright (C) 2002-2008 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <config.h>

#include <stdio.h>
#include <sys/stat.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_ASSERT_H
#include <assert.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h> 
#endif

#include <libgeda/libgeda.h>

#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

void
g_register_funcs(void)
{
  /* general functions */
  scm_c_define_gsubr ("quit", 0, 0, 0, g_quit);
  scm_c_define_gsubr ("exit", 0, 0, 0, g_quit);

  /* gsymcheckrc functions */
  scm_c_define_gsubr ("gschlas-version", 1, 0, 0, g_rc_gschlas_version);
  scm_c_define_gsubr ("force-boundingbox", 1, 0, 0, g_rc_force_boundingbox);
}

SCM
g_quit(void)
{
  gschlas_quit();
  exit(0);
}

