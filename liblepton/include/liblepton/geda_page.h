/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2017 gEDA Contributors
 * Copyright (C) 2017-2021 Lepton EDA Contributors
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

G_BEGIN_DECLS

struct st_page
{
  TOPLEVEL* toplevel;
  int pid;

  GList *_object_list;
  SELECTION *selection_list; /* new selection mechanism */
  GList *place_list;
  LeptonObject *object_lastplace; /* the last found item */
  GList *connectible_list;  /* connectible page objects */

  /* The page filename. You must access this field only via the
   * accessor functions s_page_set_filename() and
   * s_page_get_filename() */
  char *_filename;

  int CHANGED;                  /* changed flag */

  /* Undo/Redo Stacks and pointers */
  /* needs to go into page mechanism actually */
  UNDO *undo_bottom;
  UNDO *undo_current;
  UNDO *undo_tos;       /* Top Of Stack */

  /* up and down the hierarchy */
  /* this holds the pid of the parent page */
  int up;
  /* int down; not needed */

  /* used to control which pages are viewable when moving around */
  int page_control;

  /* backup variables */
  char saved_since_first_loaded;
  gint ops_since_last_backup;
  gchar do_autosave_backup;

  /* list of 'symbol version changed' info messages, e.g.:
   * "refdes: R1 (resistor.sym)"
  */
  GList* major_changed_refdes;

  GList *weak_refs; /* Weak references */
};


PAGE*
s_page_new (TOPLEVEL *toplevel, const gchar *filename);

void
s_page_delete (TOPLEVEL *toplevel, PAGE *page);

void
s_page_delete_list(TOPLEVEL *toplevel);

void
s_page_weak_ref (PAGE *page, void (*notify_func)(void *, void *), void *user_data);

void
s_page_weak_unref (PAGE *page, void (*notify_func)(void *, void *), void *user_data);

void
s_page_add_weak_ptr (PAGE *page, void *weak_pointer_loc);

void
s_page_remove_weak_ptr (PAGE *page, void *weak_pointer_loc);

void
s_page_goto (TOPLEVEL *toplevel, PAGE *p_new);

PAGE*
s_page_search (TOPLEVEL *toplevel, const gchar *filename);

PAGE*
s_page_search_by_basename (TOPLEVEL *toplevel, const gchar *filename);

PAGE*
s_page_search_by_page_id (LeptonPageList *list,
                          int pid);

void
s_page_print_all (TOPLEVEL *toplevel);

gboolean
s_page_check_changed (LeptonPageList *list);

void
s_page_clear_changed (LeptonPageList *list);

void
s_page_autosave_init(TOPLEVEL *toplevel);

gint
s_page_autosave (TOPLEVEL *toplevel);

void
s_page_append (PAGE *page,
               LeptonObject *object);

void
s_page_append_list (PAGE *page,
                    GList *obj_list);

void
s_page_remove (PAGE *page, LeptonObject *object);

void
s_page_replace (PAGE *page,
                LeptonObject *object1,
                LeptonObject *object2);

void
s_page_delete_objects (PAGE *page);

const GList*
s_page_objects (PAGE *page);

GList*
s_page_objects_in_regions (TOPLEVEL *toplevel,
                           PAGE *page,
                           BOX *rects,
                           int n_rects,
                           gboolean include_hidden);

const gchar *s_page_get_filename (const PAGE *page);

void s_page_set_filename (PAGE *page, const char *filename);

G_END_DECLS
