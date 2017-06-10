/* gEDA - GPL Electronic Design Automation
 * gnetlist - gEDA Netlist
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
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

 /*
  * 2005/05/02  Almost totally reimplemented to support dynamic allocation.
  *
  * Changes are Copyright (C) 2005 Carlos A. R. Azevedo
  */

#include <config.h>

#include <stdio.h>
#include <ctype.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_ASSERT_H
#include <assert.h>
#endif

#include <liblepton/liblepton.h>

#include "../include/globals.h"
#include "../include/prototype.h"
#include "../include/gettext.h"

static SCM reset_rename_x_proc;


void init_rename_procs (void)
{
  reset_rename_x_proc =    scm_c_public_ref ("gnetlist rename",
                                             "reset-rename!");
}

void s_rename_init(void)
{
  scm_call_0 (reset_rename_x_proc);
}
