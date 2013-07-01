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

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int o_text_get_rendered_bounds (void *user_data, OBJECT *o_current,
                                int *min_x, int *min_y,
                                int *max_x, int *max_y)
{
  GschemToplevel *w_current = (GschemToplevel *) user_data;
  TOPLEVEL *toplevel;
  EdaRenderer *renderer;
  cairo_t *cr;
  cairo_matrix_t render_mtx;
  int result, render_flags = 0;
  double t, l, r, b;

  g_return_val_if_fail ((w_current != NULL), FALSE);
  toplevel = gschem_toplevel_get_toplevel (w_current);

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
void o_text_prepare_place(GschemToplevel *w_current, char *text, int color, int align, int rotate, int size)
{
  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  TOPLEVEL *toplevel = gschem_page_view_get_toplevel (page_view);
  PAGE *page = gschem_page_view_get_page (page_view);

  g_return_if_fail (toplevel != NULL);
  g_return_if_fail (page != NULL);

  /* Insert the new object into the buffer at world coordinates (0,0).
   * It will be translated to the mouse coordinates during placement. */

  w_current->first_wx = 0;
  w_current->first_wy = 0;

  w_current->last_drawb_mode = LAST_DRAWB_MODE_NONE;

  /* remove the old place list if it exists */
  s_delete_object_glist(toplevel, page->place_list);
  page->place_list = NULL;

  /* here you need to add OBJ_TEXT when it's done */
  page->place_list =
    g_list_append(page->place_list,
                  o_text_new (toplevel, OBJ_TEXT, color,
                              0, 0, align, rotate, /* zero is angle */
                              text,
                              size,
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
void o_text_edit(GschemToplevel *w_current, OBJECT *o_current)
{
  /* you need to check to make sure only one object is selected */
  /* no actually this is okay... not here in o_edit */
  text_edit_dialog(w_current,
                   o_text_get_string (w_current->toplevel, o_current),
                   o_current->text->size, o_current->text->alignment);
}

/*! \brief Complete the text edit
 *
 *  \par Function Description
 *  This function completes the text edit by setting all the selected text
 *  objects to the desired values.
 *
 *  \param [in] w_current The topleval gschem struct.
 *  \param [in] string    The text to set the selected text objects to. If this
 *                        string is NULL or has zero length, then this function
 *                        leaves the text unchanged.
 *  \param [in] color     The color to set the selected text to. If the color
 *                        is less than zero, then this function leaves the color
 *                        unchanged.
 *  \param [in] align     The text alignment to set the selected text to. If
 *                        the alignment is less than zero, this function leaves
 *                        the alignment unchanged.
 *  \param [in] rotate    The rotation angle to set the selected text to. If
 *                        the rotation angle is less than zero, this function
 *                        leaves the rotation angle unchanged.
 *  \param [in] size      The size to set all the selected text to. If the
 *                        size is less than or equal to zero, this function
 *                        leaves the size unchanged.
 */
void o_text_edit_end(GschemToplevel *w_current, char *string, int color, int align, int rotate, int size)
{
  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  TOPLEVEL *toplevel = gschem_page_view_get_toplevel (page_view);
  PAGE *page = gschem_page_view_get_page (page_view);
  OBJECT *object;
  GList *s_current;
  char *textstr = string;

  g_return_if_fail (toplevel != NULL);
  g_return_if_fail (page != NULL);

  if ((textstr != NULL) && (g_utf8_strlen(textstr, -1) == 0)) {
    textstr = NULL;
  }

  /* skip over head */
  s_current = geda_list_get_glist (page->selection_list);

  while(s_current != NULL) {
    object = (OBJECT *) s_current->data;

    if (object) {
      if (object->type == OBJ_TEXT) {

        if (size > 0) {
          object->text->size = size;
        }

        if (align >= 0) {
          object->text->alignment = align;
        }

        if (color >= 0) {
          object->color = color;
        }

        if (rotate >= 0) {
          object->text->angle = rotate;
        }

        if (textstr != NULL) {
          o_text_set_string (toplevel, object, textstr);
          /* handle slot= attribute, it's a special case */
          if (object->attached_to != NULL &&
              g_ascii_strncasecmp (textstr, "slot=", 5) == 0) {
            o_slot_end (w_current, object->attached_to, textstr);
          }
        }

        o_text_recreate(toplevel, object);
      } 
    }
    
    s_current = g_list_next(s_current);
  }
  
  gschem_toplevel_page_content_changed (w_current, page);
  o_undo_savestate(w_current, page, UNDO_ALL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  The object passed in should be the REAL object, NOT any copy in any
 *  selection list
 */
void o_text_change(GschemToplevel *w_current, OBJECT *object, char *string,
		   int visibility, int show)
{
  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  TOPLEVEL *toplevel = gschem_page_view_get_toplevel (page_view);
  PAGE *page = gschem_page_view_get_page (page_view);

  g_return_if_fail (toplevel != NULL);
  g_return_if_fail (page != NULL);

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

  gschem_toplevel_page_content_changed (w_current, page);
}
