/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998, 1999, 2000 Kazu Hirata / Ales Hvezda
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
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_STRARG_H
#include <stdarg.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include <gtk/gtk.h>
#include <libguile.h>

#include "defines.h"
#include "struct.h"
#include "globals.h"
#include "funcs.h"

#include "../include/prototype.h"

#include "geda_list.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/* global_wid always increments, it needs to be unique per run */
static int global_wid = 0;

/* head pointer to toplevel structure, this points to all the toplevels that
   currently exist */
static TOPLEVEL *toplevel_head = NULL;

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void s_toplevel_init (void)
{
  toplevel_head = (TOPLEVEL*)g_new (TOPLEVEL, 1);
  toplevel_head->wid = -1;
  toplevel_head->next = NULL;  
  toplevel_head->prev = NULL;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
TOPLEVEL *s_toplevel_new (void)
{
  TOPLEVEL *toplevel, *tmp;

  toplevel = (TOPLEVEL*)g_new (TOPLEVEL, 1);

  toplevel->wid = global_wid++;

  toplevel->num_untitled = 0;

  toplevel->start_x = -1;
  toplevel->start_y = -1;
  toplevel->save_x  = -1;
  toplevel->save_y  = -1;
  toplevel->last_x  = -1;
  toplevel->last_y  = -1;
  toplevel->loc_x   = -1;
  toplevel->loc_y   = -1;
  toplevel->distance = -1;
  
  toplevel->current_attribute = NULL;

  toplevel->current_visible = -1; /* not sure on these */
  toplevel->current_show    = -1;

  toplevel->internal_symbol_name = NULL;
  
  toplevel->RC_list = NULL;

  toplevel->series_name        = NULL;
  toplevel->untitled_name      = NULL;
  toplevel->font_directory     = NULL;
  toplevel->scheme_directory   = NULL;
  toplevel->bitmap_directory   = NULL;

  toplevel->event_state = -1;

  toplevel->inside_action = 0;
  toplevel->rotated_inside = 0;

  toplevel->init_left = 0;
  toplevel->init_top  = 0;
  /* init_right and _bottom are set before this function is called */

  toplevel->win_width  = 0;
  toplevel->win_height = 0;
  toplevel->width  = 1;
  toplevel->height = 1;
  toplevel->image_width  = 0;
  toplevel->image_height = 0;
  toplevel->snap = 1;
  toplevel->grid = 1;
  toplevel->min_zoom = 0;
  toplevel->max_zoom = 8;

  toplevel->starting_width = 0;

  toplevel->text_alignment = 0;

  toplevel->line_type = 0;

  toplevel->fill_type = 0;

  toplevel->override_color = -1;
  toplevel->inside_redraw = 0;
  toplevel->window_aspectratio = 0;
  toplevel->display_height = 0;
  toplevel->display_width  = 0;

  toplevel->DONT_DRAW_CONN    = 0;
  toplevel->DONT_RESIZE       = 0;
  toplevel->DONT_EXPOSE       = 0;
  toplevel->DONT_REDRAW       = 0;
  toplevel->DONT_RECALC       = 0;
  toplevel->FORCE_CONN_UPDATE = 0;
  toplevel->ADDING_SEL        = 0;
  toplevel->REMOVING_SEL      = 0;

  toplevel->drawbounding_action_mode = FREE;
  toplevel->last_drawb_mode = -1;

  toplevel->CONTROLKEY = 0;
  toplevel->SHIFTKEY   = 0;
  toplevel->ALTKEY     = 0;

  toplevel->doing_pan = 0;

  toplevel->pages = geda_list_new();
  toplevel->page_current = NULL;

  toplevel->buffer_number = 0;

  toplevel->show_hidden_text = 0;

  toplevel->complex_rotate = 0;

  toplevel->last_callback = NULL;
  toplevel->cwd = NULL;

  toplevel->major_changed_refdes = NULL;

  toplevel->main_window  = NULL;
  toplevel->drawing_area = NULL;
  toplevel->menubar      = NULL;
  toplevel->popup_menu   = NULL;
  toplevel->h_scrollbar  = NULL;
  toplevel->v_scrollbar  = NULL;    
  toplevel->h_adjustment = NULL;
  toplevel->v_adjustment = NULL;
  toplevel->left_label   = NULL;
  toplevel->middle_label = NULL;
  toplevel->right_label  = NULL;
  toplevel->filename_label = NULL;
  toplevel->grid_label = NULL;
  toplevel->status_label = NULL;

  toplevel->toolbar_select = NULL;
  toplevel->toolbar_net    = NULL;
  toplevel->toolbar_bus    = NULL;
  toplevel->toolbar_edit   = NULL;
  toplevel->toolbar_move   = NULL;
  toplevel->toolbar_copy   = NULL;
  toplevel->toolbar_delete = NULL;
  toplevel->toolbar_rotate = NULL;
  toplevel->toolbar_mirror = NULL;

  toplevel->fowindow = NULL;
  toplevel->fswindow = NULL;
  toplevel->sowindow = NULL;
  toplevel->saveas_flag = 0;

  toplevel->aswindow      = NULL;
  toplevel->attr_list     = NULL;
  toplevel->asentry_name  = NULL;
  toplevel->asentry_value = NULL;

  toplevel->cswindow      = NULL;
  toplevel->clib_list     = NULL;
  toplevel->basename_list = NULL;

/*   toplevel->fileselect */

  toplevel->iwindow = NULL;
  toplevel->ifilename_entry = NULL; 

  toplevel->pswindow   = NULL;

  toplevel->tiwindow = NULL;
  toplevel->tewindow = NULL;
  toplevel->teentry  = NULL;
  toplevel->ltwindow = NULL;
  toplevel->ftwindow = NULL;
  toplevel->sewindow = NULL;
  toplevel->seentry  = NULL;
  toplevel->exwindow = NULL;
  toplevel->aawindow = NULL;
  toplevel->mawindow = NULL;
  toplevel->aewindow = NULL;
  toplevel->aaentry_start = NULL;
  toplevel->aaentry_sweep = NULL;
  toplevel->trwindow = NULL;
  toplevel->trentry  = NULL;
  toplevel->tswindow = NULL;
  toplevel->tshowwindow = NULL;
  toplevel->thidewindow = NULL;
  toplevel->tfindwindow = NULL;
  toplevel->tsentry  = NULL;
	
  toplevel->abwindow = NULL;
  toplevel->hkwindow = NULL;
  toplevel->cowindow = NULL;
  toplevel->coord_world = NULL;
  toplevel->coord_screen = NULL;

  toplevel->pfswindow = NULL;
  toplevel->pcfswindow = NULL;

  toplevel->current_pixbuf = NULL;
  toplevel->pixbuf_filename = NULL;
  toplevel->pixbuf_wh_ratio = 0;

  toplevel->clwindow = NULL;
  toplevel->edit_color = 0;

  toplevel->window = NULL; 
	
  toplevel->gc              = NULL;
  toplevel->xor_gc          = NULL;
  toplevel->outline_xor_gc  = NULL;
  toplevel->bounding_xor_gc = NULL;
  toplevel->bus_gc          = NULL;

  toplevel->keyaccel_string = NULL;

  toplevel->backingstore = NULL;

  toplevel->graphic_color = 0;
  toplevel->pin_color     = 0;
  toplevel->text_color    = 0;

  toplevel->logic_bubble_color = 0; 
  toplevel->zoom_box_color = 0; 
  toplevel->text_caps = 0;
  toplevel->attribute_color    = 0;
  toplevel->detachedattr_color = 0;
  toplevel->text_size = 0;

  toplevel->snap_size = 100;

  toplevel->grid_color         = 0;
  toplevel->background_color   = 0;
  toplevel->select_color       = 0;
  toplevel->bb_color           = 0;
  toplevel->lock_color         = 0;
  toplevel->net_endpoint_color = 0;
  toplevel->junction_color     = 0;
  toplevel->net_color          = 0;
  toplevel->bus_color          = 0;
  toplevel->override_net_color = -1;
  toplevel->override_bus_color = -1;
  toplevel->override_pin_color = -1;
  toplevel->pin_style = 0;
  toplevel->net_style = 0;
  toplevel->bus_style = 0;
  toplevel->line_style = 0;
  toplevel->zoom_with_pan = 0;

  toplevel->actionfeedback_mode = OUTLINE;

  toplevel->text_feedback = 0;

  toplevel->text_display_zoomfactor = 0;

  toplevel->net_endpoint_mode = NONE;

  toplevel->net_midpoint_mode = NONE;

  toplevel->object_clipping = 0;

  toplevel->embed_complex = 0;

  toplevel->include_complex = 0;

  toplevel->text_output = 0;

  toplevel->scrollbars_flag = 0;

  toplevel->print_orientation = 0;

  toplevel->image_color = FALSE;

  toplevel->print_color = FALSE;

  toplevel->print_color_background = 0;

  toplevel->setpagedevice_orientation = FALSE;

  toplevel->setpagedevice_pagesize = FALSE;

  toplevel->postscript_prolog = NULL;
  toplevel->postscript_font_scale = 1.0;

  toplevel->stroke_color = 0;

  toplevel->log_window = 0;

  toplevel->log_window_type = 0;

  toplevel->third_button = 0;

  toplevel->middle_button = 0;

  toplevel->net_consolidate = FALSE;

  toplevel->file_preview = 0;

  toplevel->enforce_hierarchy = 0;

  toplevel->text_origin_marker = 0;

  toplevel->fast_mousepan = 0;

  toplevel->raise_dialog_boxes = 0;

  /* The following is an attempt at getting (deterministic) defaults */
  /* for the following variables */
  toplevel->attribute_promotion = FALSE;
  toplevel->promote_invisible   = FALSE;
  toplevel->keep_invisible      = FALSE;

  toplevel->continue_component_place = 0;

  toplevel->undo_levels = 0;

  toplevel->undo_control = 0;

  toplevel->undo_type = 0;

  toplevel->draw_grips = 0;

  toplevel->netconn_rubberband = 0;

  toplevel->sort_component_library = 0;

  toplevel->warp_cursor = 0;

  toplevel->toolbars = 0;

  toplevel->handleboxes = 0;

  toplevel->print_output_type = 0;

  toplevel->print_output_capstyle = BUTT_CAP;

  toplevel->image_output_type = 0;

  toplevel->paper_width  = 0;
  toplevel->paper_height = 0;

  toplevel->bus_ripper_size = 0;

  toplevel->bus_ripper_type = 0;

  toplevel->bus_ripper_symname = NULL;

  toplevel->bus_ripper_rotation = 0;

  toplevel->force_boundingbox = FALSE;

  toplevel->grid_dot_size = 1;
  toplevel->grid_mode = GRID_VARIABLE_MODE;
  toplevel->grid_fixed_threshold = 10;

  toplevel->print_vector_threshold = 3;

  toplevel->add_attribute_offset = 50;

  toplevel->drag_can_move = TRUE;

  toplevel->always_promote_attributes = NULL;

  toplevel->mousepan_gain = 5;
  toplevel->keyboardpan_gain = 10;

  toplevel->print_command = NULL;

  toplevel->select_slack_pixels = 4;

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

  /* set the rest of the variables */
  if (variable_set_func) {
    (*variable_set_func) (toplevel);
  }


  /* disable the events */
  toplevel->DONT_DRAW_CONN = 1;
  toplevel->DONT_RESIZE = 1;
  toplevel->DONT_EXPOSE = 1;
  toplevel->DONT_RECALC = 1;
  toplevel->DONT_REDRAW = 1;

  
  /* now append toplevel to this list: */
  /*   - find the tail of the toplevel list */
  for (tmp = toplevel_head; tmp->next != NULL; tmp = tmp->next);
  /*   - link toplevel with tmp */
  tmp->next = toplevel;
  toplevel->prev = tmp;
  toplevel->next = NULL;

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
    gboolean ret;
    ret = g_source_remove (toplevel->auto_save_timeout);
    g_assert (ret);
  }

  if (toplevel->wid == -1) {
    /* do no delete head */
    return;
  }

  g_free (toplevel->internal_symbol_name);

  g_free (toplevel->series_name);
  g_free (toplevel->untitled_name);
  g_free (toplevel->font_directory);
  g_free (toplevel->scheme_directory);
  g_free (toplevel->bitmap_directory);
  g_free (toplevel->bus_ripper_symname);
  g_free (toplevel->print_command);
  
  /* free all fonts */
  /* if you close a window, then you free the font set... */
  /* this is probably a bad idea... */
  /* The font set can ONLY be freed when exiting!!! */
  /*  o_text_freeallfonts (toplevel); */

  /* delete all pages */
  s_page_delete_list (toplevel);

  /* Delete the page list */
  g_object_unref(toplevel->pages);

  /* unlink toplevel from toplevel list */
  toplevel->prev->next = toplevel->next;
  if (toplevel->next != NULL) {
    toplevel->next->prev = toplevel->prev;
  }

  g_free (toplevel);

}
