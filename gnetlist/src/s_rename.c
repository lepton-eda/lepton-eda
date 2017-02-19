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

static SCM search_rename_proc;
static SCM add_rename_proc;
static SCM get_rename_list_proc;
static SCM set_rename_list_x_proc;


void init_rename_procs (void)
{
  search_rename_proc =     scm_c_public_ref ("gnetlist rename",
                                             "search-rename");
  add_rename_proc =        scm_c_public_ref ("gnetlist rename",
                                             "add-rename");
  get_rename_list_proc =   scm_c_public_ref ("gnetlist rename",
                                             "get-rename-list");
  set_rename_list_x_proc = scm_c_public_ref ("gnetlist rename",
                                             "set-rename-list!");
}

void s_rename_init(void)
{
  scm_call_1 (set_rename_list_x_proc, SCM_EOL);
}

int s_rename_search(char *src, char *dest, int quiet_flag)
{
  return scm_is_true (scm_call_3 (search_rename_proc,
                                  src ? scm_from_utf8_string (src) : SCM_BOOL_F,
                                  dest ? scm_from_utf8_string (dest) : SCM_BOOL_F,
                                  SCM_BOOL_T));
}


void s_rename_add(char *src, char *dest)
{
  scm_call_2 (add_rename_proc,
              src ? scm_from_utf8_string (src) : SCM_BOOL_F,
              dest ? scm_from_utf8_string (dest) : SCM_BOOL_F);
}
