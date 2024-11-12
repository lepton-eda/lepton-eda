/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
 * Copyright (C) 2017-2026 Lepton EDA Contributors
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
/*!
 * \file find_text_state.h
 *
 * \brief
 */

#ifndef _FIND_TEXT_STATE_H_
#define _FIND_TEXT_STATE_H_


#define SCHEMATIC_FIND_TEXT_STATE_TYPE           (schematic_find_text_state_get_type())
#define SCHEMATIC_FIND_TEXT_STATE(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), SCHEMATIC_FIND_TEXT_STATE_TYPE, SchematicFindTextState))
#define SCHEMATIC_FIND_TEXT_STATE_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  SCHEMATIC_FIND_TEXT_STATE_TYPE, SchematicFindTextStateClass))
#define IS_SCHEMATIC_FIND_TEXT_STATE(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), SCHEMATIC_FIND_TEXT_STATE_TYPE))
#define SCHEMATIC_FIND_TEXT_STATE_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), SCHEMATIC_FIND_TEXT_STATE_TYPE, SchematicFindTextStateClass))


enum
{
  FIND_TYPE_PATTERN,
  FIND_TYPE_REGEX,
  FIND_TYPE_SUBSTRING,
  FIND_TYPE_CHECK
};


typedef struct _SchematicFindTextStateClass SchematicFindTextStateClass;
typedef struct _SchematicFindTextState SchematicFindTextState;

struct _SchematicFindTextStateClass
{
  SchematicBinClass parent_class;
};

struct _SchematicFindTextState
{
  SchematicBin parent;

  GtkListStore *store;
  GtkWidget *tree_widget;
};


GType
schematic_find_text_state_get_type ();

G_BEGIN_DECLS

int
schematic_find_text_state_find (SchematicWindow *w_current,
                                SchematicFindTextState *state,
                                GList *pages,
                                int type,
                                const char *text,
                                gboolean descend,
                                gboolean include_hidden);
GtkWidget*
schematic_find_text_state_new ();

GtkTreeSelection*
schematic_find_text_state_get_selection (SchematicFindTextState *state);

void
schematic_find_text_state_select (GtkTreeSelection *selection,
                                  SchematicFindTextState *state);
G_END_DECLS

#endif /* _FIND_TEXT_STATE_H_ */
