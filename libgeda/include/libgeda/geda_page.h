/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
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
/*! \file geda_page.h
 */

struct st_page
{
  TOPLEVEL* toplevel;
  int pid;

  GList *_object_list;
  SELECTION *selection_list; /* new selection mechanism */
  GList *place_list;
  OBJECT *object_lastplace; /* the last found item */
  GList *connectible_list;  /* connectible page objects */

  char *page_filename;
  int CHANGED;			/* changed flag */

  /* Undo/Redo Stacks and pointers */
  /* needs to go into page mechanism actually */
  UNDO *undo_bottom;
  UNDO *undo_current;
  UNDO *undo_tos; 	/* Top Of Stack */

  /* up and down the hierarchy */
  /* this holds the pid of the parent page */
  int up;
  /* int down; not needed */

  /* used to control which pages are viewable when moving around */
  int page_control;

  /* backup variables */
  GTimeVal last_load_or_save_time;
  char saved_since_first_loaded;
  gint ops_since_last_backup;
  gchar do_autosave_backup;

  GList *weak_refs; /* Weak references */
};
