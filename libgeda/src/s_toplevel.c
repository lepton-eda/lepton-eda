/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998, 1999, 2000 Kazu Hirata / Ales Hvezda
 * Copyright (C) 1998-2008 Ales Hvezda
 * Copyright (C) 1998-2008 gEDA Contributors (see ChangeLog for details)
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
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_STRARG_H
#include <stdarg.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include "libgeda_priv.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
TOPLEVEL *s_toplevel_new (void)
{
  TOPLEVEL *toplevel;

  toplevel = (TOPLEVEL*)g_new (TOPLEVEL, 1);

  toplevel->RC_list = NULL;

  toplevel->untitled_name      = NULL;
  toplevel->font_directory     = NULL;
  toplevel->scheme_directory   = NULL;
  toplevel->bitmap_directory   = NULL;

  toplevel->init_left = 0;
  toplevel->init_top  = 0;
  /* init_right and _bottom are set before this function is called */

  toplevel->width  = 1;
  toplevel->height = 1;
  toplevel->snap = SNAP_GRID;

  toplevel->override_color = -1;

  toplevel->DONT_REDRAW       = 0;
  toplevel->ADDING_SEL        = 0;

  toplevel->pages = geda_list_new();
  toplevel->page_current = NULL;

  toplevel->show_hidden_text = 0;

  toplevel->major_changed_refdes = NULL;

  toplevel->snap_size = 100;

  /* BLOCK SET IN GSCHEM, BUT USED IN LIBGEDA - NEEDS A RETHINK */
  toplevel->background_color   = 0;
  toplevel->override_net_color = -1;
  toplevel->override_bus_color = -1;
  toplevel->override_pin_color = -1;
  toplevel->pin_style = 0;
  toplevel->net_style = 0;
  toplevel->bus_style = 0;
  toplevel->line_style = 0;
  /* END BLOCK - ALTHOUGH THERE ARE MORE CASES! */

  toplevel->object_clipping = 0;

  toplevel->text_output = 0;

  toplevel->print_orientation = 0;

  toplevel->image_color = FALSE;

  toplevel->print_color = FALSE;

  toplevel->print_color_background = 0;

  toplevel->setpagedevice_orientation = FALSE;

  toplevel->setpagedevice_pagesize = FALSE;

  toplevel->postscript_prolog = NULL;
  toplevel->postscript_font_scale = 1.0;

  toplevel->net_consolidate = FALSE;

  /* The following is an attempt at getting (deterministic) defaults */
  /* for the following variables */
  toplevel->attribute_promotion = FALSE;
  toplevel->promote_invisible   = FALSE;
  toplevel->keep_invisible      = FALSE;

  toplevel->print_output_type = 0;

  toplevel->print_output_capstyle = BUTT_CAP;

  toplevel->paper_width  = 0;
  toplevel->paper_height = 0;

  toplevel->bus_ripper_symname = NULL;

  toplevel->force_boundingbox = FALSE;

  toplevel->print_vector_threshold = 3;

  toplevel->always_promote_attributes = NULL;

  toplevel->net_naming_priority = 0;
  toplevel->hierarchy_traversal = 0;
  toplevel->hierarchy_uref_mangle = 0;
  toplevel->hierarchy_netname_mangle = 0;
  toplevel->hierarchy_netattrib_mangle = 0;
  toplevel->hierarchy_uref_separator      = NULL;
  toplevel->hierarchy_netname_separator   = NULL;
  toplevel->hierarchy_netattrib_separator = NULL;
  toplevel->hierarchy_netattrib_order = 0;
  toplevel->hierarchy_netname_order = 0;
  toplevel->hierarchy_uref_order = 0;
  toplevel->unnamed_netname = NULL;

  /* Auto-save interval */
  toplevel->auto_save_interval = 0;
  toplevel->auto_save_timeout = 0;

  /* disable the events */
  toplevel->DONT_REDRAW = 1;

  return toplevel;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void s_toplevel_delete (TOPLEVEL *toplevel)
{
  if (toplevel->auto_save_timeout != 0) {
    /* Assume this works */
    g_source_remove (toplevel->auto_save_timeout);
  }

  g_free (toplevel->untitled_name);
  g_free (toplevel->font_directory);
  g_free (toplevel->scheme_directory);
  g_free (toplevel->bitmap_directory);
  g_free (toplevel->bus_ripper_symname);
  
  /* free all fonts */
  /* if you close a window, then you free the font set... */
  /* this is probably a bad idea... */
  /* The font set can ONLY be freed when exiting!!! */
  /*  o_text_freeallfonts (toplevel); */

  /* delete all pages */
  s_page_delete_list (toplevel);

  /* Delete the page list */
  g_object_unref(toplevel->pages);

  g_free (toplevel);

}
