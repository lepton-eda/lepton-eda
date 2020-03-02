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
/*! \file geda_undo.h
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

  UNDO *prev;
  UNDO *next;
};

UNDO*
s_undo_return_tail (UNDO *head);

UNDO *
s_undo_return_head (UNDO *tail);

UNDO *
s_undo_new_head (void);

void
s_undo_destroy_head (UNDO *u_head);

UNDO *
s_undo_add (UNDO *head, int type, char *filename, GList *object_list, int x, int y, double scale, int page_control, int up);

void
s_undo_print_all (UNDO *head);

void
s_undo_destroy_all (TOPLEVEL *toplevel, UNDO *head);

void
s_undo_remove_rest (TOPLEVEL *toplevel, UNDO *head);

int
s_undo_levels (UNDO *head);

void
s_undo_init (PAGE *p_current);

void
s_undo_free_all (TOPLEVEL *toplevel, PAGE *p_current);

G_END_DECLS
