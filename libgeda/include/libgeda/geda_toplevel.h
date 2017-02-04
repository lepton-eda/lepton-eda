/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998, 1999, 2000 Kazu Hirata / Ales Hvezda
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2013 gEDA Contributors (see ChangeLog for details)
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
/*! \file geda_toplevel.h
 */

struct st_toplevel
{
  /* have to decided on component list stuff */
  /* if it should go in here or not */
  /* leave outside for now */

  GList *RC_list;                       /* List of RC files which have been read in. */

  char *bitmap_directory; 		/* path of the bitmaps */

  int init_left, init_right; 		/* Starting values for above */
  int init_top, init_bottom;

  int width, height;			/* height, width of window */

  /* page system */
  PAGE *page_current;
  GedaPageList *pages;

  /* show_hidden_text is used to control which text is hidden in gschem */
  int show_hidden_text;

  GList* major_changed_refdes;          /* A list of all refdes's that have */
                                        /* major symbol version changes */

  /* backup variables */
  int auto_save_interval;
  gint auto_save_timeout;

  /* either TRUE or FALSE (color or no color) */
  int image_color;

  /* controls if the net consolidation code is used */
  int net_consolidate;

  /*controls if attribute promotion happens */
  int attribute_promotion;

  /* controls if invisible attribs are promoted */
  int promote_invisible;

  /* controls if invisible attribs are kept and not deleted */
  int keep_invisible;

  /* controls the generation of backup (~) files */
  int make_backup_files;

  /* filename of the bus ripper component if set above */
  char *bus_ripper_symname;

  /* controls if the whole bounding box is used in the auto whichend code */
  int force_boundingbox;

  /* List of attributes to always promote */
  GPtrArray *always_promote_attributes;

  /* Callback function for calculating text bounds */
  RenderedBoundsFunc rendered_text_bounds_func;
  void *rendered_text_bounds_data;

  /* Callback functions for object change notification */
  GList *change_notify_funcs;

  /* Callback function for deciding whether to load a backup file. */
  LoadBackupQueryFunc load_newer_backup_func;
  void *load_newer_backup_data;

  GList *weak_refs; /* Weak references */
};

void
s_toplevel_add_weak_ptr (TOPLEVEL *toplevel, void *weak_pointer_loc);

void
s_toplevel_delete (TOPLEVEL *toplevel);

TOPLEVEL*
s_toplevel_new (void);

void
s_toplevel_remove_weak_ptr (TOPLEVEL *toplevel, void *weak_pointer_loc);

void
s_toplevel_set_page_current (TOPLEVEL *toplevel, PAGE *page);

void
s_toplevel_weak_ref (TOPLEVEL *toplevel, void (*notify_func)(void *, void *), void *user_data);

void
s_toplevel_weak_unref (TOPLEVEL *toplevel, void (*notify_func)(void *, void *), void *user_data);
