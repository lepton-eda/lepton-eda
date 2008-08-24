/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2007 Ales Hvezda
 * Copyright (C) 1998-2007 gEDA Contributors (see ChangeLog for details)
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

#include <config.h>

#include <stdio.h>
#include <sys/stat.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "libgeda_priv.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \brief */
struct gsubr_t {
  char* name;
  int req;
  int opt;
  int rst;
  SCM (*fnc)();
};

/*! \brief */
static struct gsubr_t libgeda_funcs[] = {
  { "eval-protected",           1, 1, 0, g_scm_eval_protected },
  { "eval-string-protected",    1, 0, 0, g_scm_eval_string_protected },

  { "component-library",        1, 1, 0, g_rc_component_library },
  { "component-library-command", 3, 0, 0, g_rc_component_library_command },
  { "component-library-funcs",  3, 0, 0, g_rc_component_library_funcs },
  { "component-library-search", 1, 0, 0, g_rc_component_library_search },
  { "source-library",           1, 0, 0, g_rc_source_library },
  { "source-library-search",    1, 0, 0, g_rc_source_library_search },
  
  { "world-size",               3, 0, 0, g_rc_world_size },
  
  { "reset-component-library",  0, 0, 0, g_rc_reset_component_library },
  { "reset-source-library",     0, 0, 0, g_rc_reset_source_library },
  
  { "untitled-name",            1, 0, 0, g_rc_untitled_name },
  { "scheme-directory",         1, 0, 0, g_rc_scheme_directory },
  { "bitmap-directory",         1, 0, 0, g_rc_bitmap_directory },
  { "font-directory",           1, 0, 0, g_rc_font_directory },
  { "bus-ripper-symname",       1, 0, 0, g_rc_bus_ripper_symname },
  { "postscript-prolog",        1, 0, 0, g_rc_postscript_prolog },
  { "map-font-character-to-file", 2, 0, 0, g_rc_map_font_character_to_file },
  { "attribute-promotion",       1, 0, 0, g_rc_attribute_promotion },
  { "promote-invisible",         1, 0, 0, g_rc_promote_invisible },
  { "keep-invisible",            1, 0, 0, g_rc_keep_invisible },
  { "always-promote-attributes",1, 0, 0, g_rc_always_promote_attributes },
  { NULL,                       0, 0, 0, NULL } };

/*! \brief Register all libgeda functions with scheme.
 *  \par Function Description
 *  Creates g_subr_t objects to make g_rc_* functions that are defined
 *  in g_rc.c visible to Scheme.
 */
void g_register_libgeda_funcs (void)
{
  struct gsubr_t *tmp = libgeda_funcs;
  
  while (tmp->name != NULL) {
    scm_c_define_gsubr (tmp->name, tmp->req, tmp->opt, tmp->rst, tmp->fnc);
    tmp++;
  }
  
}


/*! \brief Register some libgeda variables with scheme.
 *  \par Function Description
 *  Define some variables to be visible to Scheme.
 */
void g_register_libgeda_vars (void)
{
  scm_c_define("geda-rc-path", 
	       scm_from_locale_string (g_rc_parse_path()));
  scm_c_define("geda-data-path",
	       scm_from_locale_string (g_getenv ("GEDADATA")));
  scm_c_define("path-sep", 
	       scm_from_locale_string(G_DIR_SEPARATOR_S));

  scm_c_define("OBJ_LINE", SCM_MAKE_CHAR((unsigned char) OBJ_LINE));
  scm_c_define("OBJ_BOX", SCM_MAKE_CHAR((unsigned char) OBJ_BOX));
  scm_c_define("OBJ_PICTURE", SCM_MAKE_CHAR((unsigned char) OBJ_PICTURE));
  scm_c_define("OBJ_CIRCLE", SCM_MAKE_CHAR((unsigned char) OBJ_CIRCLE));
  scm_c_define("OBJ_NET", SCM_MAKE_CHAR((unsigned char) OBJ_NET));
  scm_c_define("OBJ_BUS", SCM_MAKE_CHAR((unsigned char) OBJ_BUS));
  scm_c_define("OBJ_COMPLEX", SCM_MAKE_CHAR((unsigned char) OBJ_COMPLEX));
  scm_c_define("OBJ_TEXT", SCM_MAKE_CHAR((unsigned char) OBJ_TEXT));
  scm_c_define("OBJ_PIN", SCM_MAKE_CHAR((unsigned char) OBJ_PIN));
  scm_c_define("OBJ_ARC", SCM_MAKE_CHAR((unsigned char) OBJ_ARC));
  scm_c_define("OBJ_ROUTE", SCM_MAKE_CHAR((unsigned char) OBJ_ROUTE));
  scm_c_define("OBJ_THRU_HOLE", SCM_MAKE_CHAR((unsigned char) OBJ_THRU_HOLE));
  scm_c_define("OBJ_PLACEHOLDER", SCM_MAKE_CHAR((unsigned char) OBJ_PLACEHOLDER));
  
}
