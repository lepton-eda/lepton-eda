/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
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
/*!
 * \file gschem_find_text_state.h
 *
 * \brief
 */

#define GSCHEM_TYPE_FIND_TEXT_STATE           (gschem_find_text_state_get_type())
#define GSCHEM_FIND_TEXT_STATE(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), GSCHEM_TYPE_FIND_TEXT_STATE, GschemFindTextState))
#define GSCHEM_FIND_TEXT_STATE_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  GSCHEM_TYPE_FIND_TEXT_STATE, GschemFindTextStateClass))
#define GSCHEM_IS_FIND_TEXT_STATE(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GSCHEM_TYPE_FIND_TEXT_STATE))
#define GSCHEM_FIND_TEXT_STATE_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), GSCHEM_TYPE_FIND_TEXT_STATE, GschemFindTextStateClass))


typedef struct _GschemFindTextStateClass GschemFindTextStateClass;
typedef struct _GschemFindTextState GschemFindTextState;

struct _GschemFindTextStateClass
{
  GtkAlignmentClass parent_class;
};

struct _GschemFindTextState
{
  GtkAlignment parent;

  GtkListStore *store;
};


int
gschem_find_text_state_find (GschemFindTextState *state, GList *pages, const char *text, gboolean descend);

GType
gschem_find_text_state_get_type ();

GschemFindTextState*
gschem_find_text_state_new ();
