/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
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
/*! \file undo.h
 */

G_BEGIN_DECLS

struct st_undo
{
  /* one of these is used, depending on if you are doing in-memory */
  /* or file based undo state saving */
  char *filename;
  GList *object_list;

  /* either UNDO_ALL or UNDO_VIEWPORT_ONLY */
  int type;

  /* viewport information */
  int x, y;
  double scale;

  /* up and down the hierarchy */
  int up;
  /* used to control which pages are viewable when moving around */
  int page_control;

  LeptonUndo *prev;
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

int
lepton_undo_get_page_control (LeptonUndo *undo);

LeptonUndo*
lepton_undo_get_prev (LeptonUndo *undo);

int
lepton_undo_get_type (LeptonUndo *undo);

int
lepton_undo_get_up (LeptonUndo *undo);

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
