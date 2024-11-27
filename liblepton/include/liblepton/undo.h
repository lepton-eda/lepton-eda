/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
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
/*! \file undo.h
 */

G_BEGIN_DECLS

struct st_undo
{
  /* One of these fields is used, depending on if you are doing
   * in-memory or file based undo state saving. */
  /* The name of the file to save the undo information to. */
  char *filename;
  /* The list of objects to save in memory. */
  GList *object_list;

  /* TRUE if only viewport size has to be saved.  Otherwise,
     FALSE. */
  gboolean type;

  /* Viewport information */
  /* The coordinates of the viewport center. */
  int x, y;
  /* The scale factor of the viewport. */
  double scale;

  /* Hierarchy information of the current page */
  /* The pid of the parent page in the hierarchy. */
  int up;
  /* The control value that defines which pages are viewable when
   * moving around in the hierarchy. */
  int page_control;

  /* The previous element in the undo list, if any. */
  LeptonUndo *prev;
  /* The next element in the undo list, if any. */
  LeptonUndo *next;
};

char*
lepton_undo_get_filename (LeptonUndo *undo);

void
lepton_undo_set_filename (LeptonUndo *undo,
                          const char *filename);
GList*
lepton_undo_get_object_list (LeptonUndo *undo);

void
lepton_undo_set_object_list (LeptonUndo *undo,
                             GList *object_list);
LeptonUndo*
lepton_undo_get_next (LeptonUndo *undo);

void
lepton_undo_set_next (LeptonUndo *undo,
                      LeptonUndo *next);
int
lepton_undo_get_page_control (LeptonUndo *undo);

void
lepton_undo_set_page_control (LeptonUndo *undo,
                              int page_control);
LeptonUndo*
lepton_undo_get_prev (LeptonUndo *undo);

void
lepton_undo_set_prev (LeptonUndo *undo,
                      LeptonUndo *prev);
double
lepton_undo_get_scale (LeptonUndo *undo);

void
lepton_undo_set_scale (LeptonUndo *undo,
                       double scale);
int
lepton_undo_get_type (LeptonUndo *undo);

void
lepton_undo_set_type (LeptonUndo *undo,
                      int type);
int
lepton_undo_get_up (LeptonUndo *undo);

void
lepton_undo_set_up (LeptonUndo *undo,
                    int up);
int
lepton_undo_get_x (LeptonUndo *undo);

void
lepton_undo_set_x (LeptonUndo *undo,
                   int x);
int
lepton_undo_get_y (LeptonUndo *undo);

void
lepton_undo_set_y (LeptonUndo *undo,
                   int y);
LeptonUndo*
lepton_undo_return_tail (LeptonUndo *head);

LeptonUndo*
lepton_undo_add (LeptonUndo *head,
                 int type,
                 char *filename,
                 GList *object_list,
                 int x,
                 int y,
                 double scale,
                 int page_control,
                 int up);
void
lepton_undo_print_all (LeptonUndo *head);

void
lepton_undo_destroy_all (LeptonUndo *head);

void
lepton_undo_remove_rest (LeptonUndo *head);

int
lepton_undo_levels (LeptonUndo *head);

void
lepton_undo_init (LeptonPage *p_current);

void
lepton_undo_free_all (LeptonPage *p_current);

G_END_DECLS
