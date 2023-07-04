/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2017 gEDA Contributors
 * Copyright (C) 2017-2023 Lepton EDA Contributors
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
/*! \file page.h
 */

G_BEGIN_DECLS

struct st_page
{
  LeptonToplevel* toplevel;
  int pid;

  GList *_object_list;
  LeptonSelection *selection_list; /* selection mechanism */
  GList *place_list;
  LeptonObject *object_lastplace; /* the last found item */
  GList *connectible_list;  /* connectible page objects */

  /* The page filename. You must access this field only via the
   * accessor functions lepton_page_set_filename() and
   * lepton_page_get_filename() */
  char *_filename;

  int CHANGED;                  /* changed flag */

  /* Undo/Redo Stacks and pointers */
  /* needs to go into page mechanism actually */
  LeptonUndo *undo_bottom;
  LeptonUndo *undo_current;
  LeptonUndo *undo_tos;       /* Top Of Stack */

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

int
lepton_page_get_changed (LeptonPage *page);

void
lepton_page_set_changed (LeptonPage *page,
                         int changed);
int
lepton_page_get_pid (LeptonPage *page);

void
lepton_page_set_pid (LeptonPage *page,
                     int pid);
int
lepton_page_get_page_control (LeptonPage *page);

void
lepton_page_set_page_control (LeptonPage *page,
                              int page_control);
LeptonUndo*
lepton_page_get_undo_bottom (LeptonPage *page);

void
lepton_page_set_undo_bottom (LeptonPage *page,
                             LeptonUndo* undo);
LeptonUndo*
lepton_page_get_undo_current (LeptonPage *page);

void
lepton_page_set_undo_current (LeptonPage *page,
                              LeptonUndo* undo);
LeptonUndo*
lepton_page_get_undo_tos (LeptonPage *page);

int
lepton_page_get_up (LeptonPage *page);

void
lepton_page_set_up (LeptonPage *page,
                    int up);
GList*
lepton_page_get_place_list (LeptonPage *page);

void
lepton_page_set_place_list (LeptonPage *page,
                            GList *place_list);
void
lepton_page_delete_place_list (LeptonPage *page);

LeptonSelection*
lepton_page_get_selection_list (LeptonPage *page);

void
lepton_page_set_selection_list (LeptonPage *page,
                                LeptonSelection *selection_list);
LeptonPage*
lepton_page_new (LeptonToplevel *toplevel,
                 const gchar *filename);
void
lepton_page_delete (LeptonToplevel *toplevel,
                    LeptonPage *page);
void
lepton_page_weak_ref (LeptonPage *page,
                      void (*notify_func)(void *, void *),
                      void *user_data);
void
lepton_page_weak_unref (LeptonPage *page,
                        void (*notify_func)(void *, void *),
                        void *user_data);
void
lepton_page_add_weak_ptr (LeptonPage *page,
                          void *weak_pointer_loc);
void
lepton_page_remove_weak_ptr (LeptonPage *page,
                             void *weak_pointer_loc);
void
lepton_page_append (LeptonPage *page,
                    LeptonObject *object);
void
lepton_page_append_list (LeptonPage *page,
                         GList *obj_list);
void
lepton_page_remove (LeptonPage *page,
                    LeptonObject *object);
void
lepton_page_replace (LeptonPage *page,
                     LeptonObject *object1,
                     LeptonObject *object2);
void
lepton_page_delete_objects (LeptonPage *page);

const GList*
lepton_page_objects (LeptonPage *page);

GList*
lepton_page_objects_in_regions (LeptonPage *page,
                                LeptonBox *rects,
                                int n_rects,
                                gboolean include_hidden);
const gchar*
lepton_page_get_filename (const LeptonPage *page);

void
lepton_page_set_filename (LeptonPage *page,
                          const char *filename);
GList*
lepton_page_list_get_glist (LeptonPageList *page_list);

G_END_DECLS
