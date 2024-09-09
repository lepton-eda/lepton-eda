/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2017 gEDA Contributors
 * Copyright (C) 2017-2024 Lepton EDA Contributors
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

/* The fields whose names have the prefix '_' are private ones.
 * Please don't use them directly, use the provided accessors
 * instead. */
struct st_page
{
  /* The toplevel structure the page belongs to.  It contains the
   * lists of all open pages and configuration settings. */
  LeptonToplevel* toplevel;
  /* The internal unique page ID. */
  int pid;
  /* The list of page objects. */
  GList *_object_list;
  /* The list of selected objects. */
  LeptonSelection *selection_list; /* selection mechanism */
  /* The list of the objects to be placed on the current page in
   * GUI. */
  GList *place_list;
  /* The object found last by the find functions. */
  LeptonObject *object_lastplace;
  /* The list of connectible page objects. */
  GList *connectible_list;
  /* The page filename. */
  char *_filename;
  /* The flag that means the page has been changed and not yet
   * saved. */
  int CHANGED;

  /* Undo/Redo Stacks and pointers. */
  /* The bottom of the Undo stack. */
  LeptonUndo *undo_bottom;
  /* The pointer to the current Undo item in the stack. */
  LeptonUndo *undo_current;
  /* The top of the Undo stack (Top Of Stack). */
  LeptonUndo *undo_tos;

  /* Hierarchy. */
  /* The pid of the parent page in the hierarchy. */
  int up;
  /* The control value that defines which pages are viewable when
   * moving around in the hierarchy. */
  int page_control;

  /* Backup variables. */
  /* The flag that indicates that the page has been opened and
   * changed but not ever saved yet so the initial file has to
   * backed up. */
  char saved_since_first_loaded;
  /* The number of operations since the last backup to determine
   * when automatic save should be carried out. */
  gint ops_since_last_backup;
  /* If the page has to be saved automatically. */
  gchar do_autosave_backup;

  /* list of 'symbol version changed' info messages, e.g.:
   * "refdes: R1 (resistor.sym)"
  */
  GList* major_changed_refdes;

  /* The list of weak references of the page. */
  GList *weak_refs;
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

void
lepton_page_set_undo_tos (LeptonPage *page,
                          LeptonUndo* undo);
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
LeptonToplevel*
lepton_page_get_toplevel (LeptonPage *page);

void
lepton_page_set_toplevel (LeptonPage *page,
                          LeptonToplevel *toplevel);
LeptonPage*
lepton_page_new (LeptonToplevel *toplevel,
                 const gchar *filename);
LeptonObject*
lepton_page_get_object_lastplace (LeptonPage *page);

void
lepton_page_set_object_lastplace (LeptonPage *page,
                                  LeptonObject *object_lastplace);
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
