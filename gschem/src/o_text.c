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
#include <config.h>

#include <stdio.h>
#include <sys/stat.h>
#include <math.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "gschem.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#define MINIMUM_MARK_SMALL_DIST 1

#if 1
# define FONT_NAME "Arial"
#else
# define FONT_NAME "Helvetica"
#endif

#undef DEBUG_TEXT

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int o_text_get_rendered_bounds (void *user_data, OBJECT *o_current,
                                int *min_x, int *min_y,
                                int *max_x, int *max_y)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL *) user_data;
  TOPLEVEL *toplevel;
  EdaRenderer *renderer;
  cairo_t *cr;
  cairo_matrix_t render_mtx;
  int result, render_flags = 0;
  double t, l, r, b;

  g_return_val_if_fail ((w_current != NULL), FALSE);
  toplevel = w_current->toplevel;

  cr = gdk_cairo_create (w_current->drawable);

  /* Set up renderer based on configuration in w_current. Note that we
   * *don't* enable hinting, because if its enabled the calculated
   * bounds are zoom-level-dependent. */
  if (toplevel->show_hidden_text)
    render_flags |= EDA_RENDERER_FLAG_TEXT_HIDDEN;
  renderer = g_object_ref (w_current->renderer);
  g_object_set (G_OBJECT (renderer),
                "cairo-context", cr,
                "render-flags", render_flags,
                NULL);

  /* We need to transform the cairo context to approximate world
   * coordinates. */
  cairo_matrix_init (&render_mtx, 1, 0, 0, -1, -1, 1);
  cairo_set_matrix (cr, &render_mtx);

  /* Use the renderer to calculate text bounds */
  result = eda_renderer_get_user_bounds (renderer, o_current, &l, &t, &r, &b);

  /* Clean up */
  eda_renderer_destroy (renderer);
  cairo_destroy (cr);

  /* Round bounds to nearest integer */
  *min_x = lrint (fmin (l, r));
  *min_y = lrint (fmin (t, b));
  *max_x = lrint (fmax (l, r));
  *max_y = lrint (fmax (t, b));

  return result;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_text_prepare_place(GSCHEM_TOPLEVEL *w_current, char *text)
{
  TOPLEVEL *toplevel = w_current->toplevel;

  /* Insert the new object into the buffer at world coordinates (0,0).
   * It will be translated to the mouse coordinates during placement. */

  w_current->first_wx = 0;
  w_current->first_wy = 0;

  w_current->last_drawb_mode = LAST_DRAWB_MODE_NONE;

  /* remove the old place list if it exists */
  s_delete_object_glist(toplevel, toplevel->page_current->place_list);
  toplevel->page_current->place_list = NULL;

  /* here you need to add OBJ_TEXT when it's done */
  toplevel->page_current->place_list =
    g_list_append(toplevel->page_current->place_list,
                  o_text_new (toplevel, OBJ_TEXT, TEXT_COLOR,
                              0, 0, LOWER_LEFT, 0, /* zero is angle */
                              text,
                              w_current->text_size,
                              /* has to be visible so you can place it */
                              /* visibility is set when you create the object */
                              VISIBLE, SHOW_NAME_VALUE));

  w_current->inside_action = 1;
  i_set_state (w_current, ENDTEXT);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_text_edit(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current)
{
  /* you need to check to make sure only one object is selected */
  /* no actually this is okay... not here in o_edit */
  text_edit_dialog(w_current,
                   o_text_get_string (w_current->toplevel, o_current),
                   o_current->text->size, o_current->text->alignment);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_text_edit_end(GSCHEM_TOPLEVEL *w_current, char *string, int len, int text_size,
		     int text_alignment)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  OBJECT *object;
  GList *s_current;
  int numselect;

  /* skip over head */
  s_current = geda_list_get_glist( toplevel->page_current->selection_list );
  numselect = g_list_length( geda_list_get_glist( toplevel->page_current->selection_list ));
  
  while(s_current != NULL) {
    object = (OBJECT *) s_current->data;

    if (object) {
      if (object->type == OBJ_TEXT) {

        object->text->size = text_size;
        object->text->alignment = text_alignment;

        /* probably the text object should be extended to carry a color */
        /* and we should pass it here with a function parameter (?) */
        object->color = w_current->edit_color;

        /* only change text string if there is only ONE text object selected */
        if (numselect == 1 && string) {
          o_text_set_string (w_current->toplevel, object, string);
	  /* handle slot= attribute, it's a special case */
	  if (object->attached_to != NULL &&
	      g_ascii_strncasecmp (string, "slot=", 5) == 0) {
	    o_slot_end (w_current, object->attached_to, string);
	  }
        }
        o_text_recreate(toplevel, object);
      } 
    }
    
    s_current = g_list_next(s_current);
  }
  
  toplevel->page_current->CHANGED = 1;
  o_undo_savestate(w_current, UNDO_ALL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  The object passed in should be the REAL object, NOT any copy in any
 *  selection list
 */
void o_text_change(GSCHEM_TOPLEVEL *w_current, OBJECT *object, char *string,
		   int visibility, int show)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  if (object == NULL) {
    return;
  }

  if (object->type != OBJ_TEXT) {
    return;
  }

  o_text_set_string (toplevel, object, string);

  o_set_visibility (toplevel, object, visibility);
  object->show_name_value = show;
  o_text_recreate(toplevel, object);

  /* handle slot= attribute, it's a special case */
  if (object->attached_to != NULL &&
      g_ascii_strncasecmp (string, "slot=", 5) == 0) {
    o_slot_end (w_current, object->attached_to, string);
  }

  toplevel->page_current->CHANGED = 1;
}
