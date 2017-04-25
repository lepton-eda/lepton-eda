/* Lepton EDA
 * lepton-symcheck - Lepton Symbol Checker
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA.
 */

#include <config.h>
#include <version.h>

#include <stdio.h>
#include <sys/stat.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <liblepton/liblepton.h>
#include <liblepton/libgedaguile.h>

#include "../include/prototype.h"
#include "../include/gettext.h"

void
main_prog(void *closure, int argc, char *argv[])
{
  TOPLEVEL *pr_current;
  SCM check_all_symbols;
  
  libgeda_init();

#if defined(__MINGW32__) && defined(DEBUG)
  fprintf(stderr, "This is the MINGW32 port.\n");
#endif  

  check_all_symbols = scm_c_public_lookup ("symbol check",
                                           "check-all-symbols");

  scm_dynwind_begin ((scm_t_dynwind_flags) 0);

  pr_current = s_toplevel_new ();
  edascm_dynwind_toplevel (pr_current);

#if DEBUG
  s_page_print_all(pr_current);
#endif

  scm_call_0 (scm_variable_ref (check_all_symbols));

  scm_dynwind_end ();
}

int 
main (int argc, char *argv[])
{
  scm_boot_guile (argc, argv, main_prog, NULL);
  return 0;
}
