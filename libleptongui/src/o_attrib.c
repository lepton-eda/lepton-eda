/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2022 Lepton EDA Contributors
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
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <math.h>

#include "gschem.h"



#define SPACING_FROM_END       100
#define SPACING_PERPENDICULAR   50


/* No special type for attributes */
/* You can only edit text attributes */

/* be sure in o_copy o_move o_delete you maintain the attributes */
/* delete is a bare, because you will have to unattach the other end */
/* and in o_save o_read as well */
/* and in o_select when selecting objects, select the attributes */

/* there needs to be a modifier (in struct.h, such as a flag) which
 * signifies that this is an attribute */

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *  Copy all attributes select to the selection list.
 *
 *  \todo get a better name
 */
void
o_attrib_add_selected (GschemToplevel *w_current,
                       LeptonSelection *selection,
                       LeptonObject *selected)
{
  LeptonObject *a_current;
  GList *a_iter;
  GList *selected_objects = NULL;

  g_assert( selection != NULL );

  for (a_iter = lepton_object_get_attribs (selected);
       a_iter != NULL;
       a_iter = g_list_next (a_iter)) {
    a_current = (LeptonObject*) a_iter->data;

    /* make sure object isn't selected already */
    if (!lepton_object_get_selected (a_current))
    {
      o_selection_add (selection, a_current);
      selected_objects = g_list_prepend (selected_objects, a_current);
    }
  }

  if (selected_objects != NULL) {
    /* Run select-objects-hook */
    g_run_hook_object_list (w_current, "select-objects-hook",
                            selected_objects);
    g_list_free (selected_objects);
  }
}

/*! \brief Remove invisible attributes of an object from the selection list.
 *  \par Function Description
 *
 *  Remove all invisible attributes attached to the given object
 *  from the selection list. If hidden text is being shown, this
 *  function returns immediately.
 *
 *  \param [in]     w_current  The GschemToplevel object.
 *  \param [in,out] selection  The LeptonSelection list to remove from.
 *  \param [in]     object     The LeptonObject whose invisible attributes to remove.
 */
void
o_attrib_deselect_invisible (GschemToplevel *w_current,
                             LeptonSelection *selection,
                             LeptonObject *selected)
{
  LeptonObject *a_current;
  GList *a_iter;
  gboolean show_hidden_text =
    gschem_toplevel_get_show_hidden_text (w_current);

  g_assert( selection != NULL );

  if (show_hidden_text) {
    return;
  }

  for (a_iter = lepton_object_get_attribs (selected);
       a_iter != NULL;
       a_iter = g_list_next (a_iter)) {
    a_current = (LeptonObject*) a_iter->data;

    if (lepton_object_get_selected (a_current)
        && !lepton_text_object_is_visible (a_current))
    {
      o_selection_remove (selection, a_current);
    }
  }
}

/*! \brief Add invisible attributes of an object to the selection list.
 *  \par Function Description
 *
 *  Add all invisible attributes attached to the given object
 *  to the selection list. If hidden text is being shown, this
 *  function returns immediately.
 *
 *  \param [in]     w_current  The GschemToplevel object.
 *  \param [in,out] selection  The LeptonSelection list to add to.
 *  \param [in]     object     The LeptonObject whose invisible attributes to add.
 */
void
o_attrib_select_invisible (GschemToplevel *w_current,
                           LeptonSelection *selection,
                           LeptonObject *selected)
{
  LeptonObject *a_current;
  GList *a_iter;
  gboolean show_hidden_text =
    gschem_toplevel_get_show_hidden_text (w_current);

  g_assert( selection != NULL );

  if (show_hidden_text) {
    return;
  }

  for (a_iter = lepton_object_get_attribs (selected);
       a_iter != NULL;
       a_iter = g_list_next (a_iter)) {
    a_current = (LeptonObject*) a_iter->data;

    if (!lepton_object_get_selected (a_current)
        && !lepton_text_object_is_visible (a_current))
    {
      o_selection_add (selection, a_current);
    }
  }
}

/*! \brief Change visibility status of attribute object.
 *  \par Function Description
 *  This function toggles the visibility status of the attribute \a
 *  object and updates it. The object is erased or redrawn if
 *  necessary.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] object     The attribute object.
 */
void o_attrib_toggle_visibility(GschemToplevel *w_current, LeptonObject *object)
{
  g_return_if_fail (lepton_object_is_text (object));

  gboolean show_hidden_text =
    gschem_toplevel_get_show_hidden_text (w_current);

  if (lepton_text_object_is_visible (object)) {
    /* only erase if we are not showing hidden text */
    if (!show_hidden_text) {
      o_invalidate (w_current, object);
    }

    lepton_text_object_set_visibility (object, INVISIBLE);

    if (show_hidden_text) {
      /* draw text so that little I is drawn */
      o_invalidate (w_current, object);
    }

  } else {
    /* if we are in the special show hidden mode, then erase text first */
    /* to get rid of the little I */
    if (show_hidden_text) {
      o_invalidate (w_current, object);
    }

    lepton_text_object_set_visibility (object, VISIBLE);
    lepton_text_object_recreate (object);
  }

  schematic_window_active_page_changed (w_current);
}

/*! \brief Set what part of an attribute is shown.
 *  \par Function Description
 *  This function changes what part (name, value or both) of an
 *  attribute is shown by its attribute object. The attribute object
 *  is erased, updated and finally redrawn.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] object     The attribute object.
 *  \param [in] show_name_value  The new display flag for attribute.
 */
void o_attrib_toggle_show_name_value(GschemToplevel *w_current,
                                     LeptonObject *object, int show_name_value)
{
  g_return_if_fail (lepton_object_is_text (object));

  o_invalidate (w_current, object);
  lepton_text_object_set_show (object, show_name_value);
  lepton_text_object_recreate (object);

  schematic_window_active_page_changed (w_current);
}


/*! \brief Adds an attribute with given parameters to the active page
 *  \par Function Description
 *  This function creates and returns an attribute object with
 *  given properties from \a text_string and adds it to the canvas
 *  of the #GschemToplevel object \a w_current.  Depending on the
 *  state of the objects on the page (selection, visibility), it
 *  adds an attached or unattached attribute and selects an
 *  appropriate place for it.  The strategy of choosing the place
 *  and other features is rather complicated:
 *
 * - First, the function checks if \a object is not NULL, and if
 *   so, gets its coordinates to learn where to place the
 *   attribute.  For most of primitives, the attribute becomes an
 *   attached one, and its coords are set using the coords of \a
 *   object.  For text, its coords are used in the calculations,
 *   though the attribute gets unattached.
 *
 *   \bug Some objects, like paths or pictures, are not taken
 *   into account in the code.
 *
 * - If there is no object to attach the attrib to, a toplevel
 *   (floating) attribute is created.
 *
 * - If any cooordinate is proposed with \a proposed_coord, \a x,
 *   and \a y, it becomes the coordinate of the new attrib.
 *
 * - Otherwise, if no coord is proposed, and no any visible object
 *   exists on the page, the coordinate is set to the bottom-left
 *   corner of the visible objects.
 *
 * - Then, if there is no visible object, and no coord is proposed,
 *   the value of the center of the current page view becomes the
 *   anchor of the attribute.
 *
 * - Eventually, if no one of the above conditions can be met,
 *   when, for example, the program using this function, be it C
 *   or Scheme code, misses a page view, the last resort is
 *   setting the anchor to a specific value.  Currently, it is
 *   (0,0).
 *
 * After creating a text item for the attribute with the coords
 * and other parameters described above, it gets appended to the
 * currently active page, attached to the given object if it
 * exists, and selected in order to enable further GUI processing.
 *
 * \bug slot= attributes are processed specially in this function,
 * which is wrong.
 *
 * Eventually, two Scheme hooks are evaluated, add-objects-hook()
 * and select-objects-hook().  The user may set up some other
 * script processing of newly added and hence selected attributes
 * there.
 *
 *  \param [in] w_current The GschemToplevel object.
 *  \param [in] text_string The text string of the attribute object.
 *  \param [in] visibility If the attribute should be visible.
 *  \param [in] show_name_value What combination of name-value to show.
 *  \param [in] object The parent object to attach the new attribute to.
 *  \param [in] proposed_coord If the proposed coord values can be used.
 *  \param [in] x The proposed X coordinate.
 *  \param [in] y The proposed Y coordinate.
 *  \return The new attribute object.
 */
LeptonObject*
o_attrib_add_attrib (GschemToplevel *w_current,
                     const char *text_string,
                     int visibility,
                     int show_name_value,
                     LeptonObject *object,
                     gboolean proposed_coord,
                     int x,
                     int y)
{
  LeptonObject *new_obj;
  int world_x = - 1, world_y = -1;
  int align = LOWER_LEFT;
  int angle = 0;
  int color;
  int left, right, top, bottom;
  int found;
  LeptonObject *o_current;
  LeptonPage *active_page = schematic_window_get_active_page (w_current);

  color = DETACHED_ATTRIBUTE_COLOR;

  o_current = object;

  /* Creating an attribute for the given object, if it exists. */
  if (o_current) {
    /* Get coordinates of where to place the text object and
     * parameters of the new attribute. */
    switch (lepton_object_get_type (o_current)) {
      case(OBJ_COMPONENT):
        world_x = lepton_component_object_get_x (o_current);
        world_y = lepton_component_object_get_y (o_current);
        align = LOWER_LEFT;
        angle = 0;
        color = ATTRIBUTE_COLOR;
        break;

      case(OBJ_ARC):
        world_x = lepton_arc_object_get_center_x (o_current);
        world_y = lepton_arc_object_get_center_y (o_current);
        align = LOWER_LEFT;
        angle = 0;
        color = ATTRIBUTE_COLOR;
        break;

      case(OBJ_CIRCLE):
        world_x = lepton_circle_object_get_center_x (o_current);
        world_y = lepton_circle_object_get_center_y (o_current);
        align = LOWER_LEFT;
        angle = 0;
        color = ATTRIBUTE_COLOR;
        break;

      case(OBJ_BOX):
        world_x = lepton_box_object_get_upper_x (o_current);
        world_y = lepton_box_object_get_upper_y (o_current);
        align = LOWER_LEFT;
        angle = 0;
        color = ATTRIBUTE_COLOR;
        break;

      case(OBJ_LINE):
      case(OBJ_NET):
      case(OBJ_PIN):
      case(OBJ_BUS):
        {
          int dx = o_current->line->x[1] - o_current->line->x[0];
          int dy = o_current->line->y[1] - o_current->line->y[0];

          if (dy == 0) {
              if (dx > 0) {
                  world_x = o_current->line->x[0] + SPACING_FROM_END;
                  world_y = o_current->line->y[0] + SPACING_PERPENDICULAR;

                  align = LOWER_LEFT;
                  angle = 0;
              }
              else {
                  world_x = o_current->line->x[0] - SPACING_FROM_END;
                  world_y = o_current->line->y[0] + SPACING_PERPENDICULAR;

                  align = LOWER_RIGHT;
                  angle = 0;
              }
          }
          else if (dx == 0) {
              if (dy > 0) {
                  world_x = o_current->line->x[0] - SPACING_PERPENDICULAR;
                  world_y = o_current->line->y[0] + SPACING_FROM_END;

                  align = LOWER_LEFT;
                  angle = 90;
              }
              else {
                  world_x = o_current->line->x[0] - SPACING_PERPENDICULAR;
                  world_y = o_current->line->y[0] - SPACING_FROM_END;

                  align = LOWER_RIGHT;
                  angle = 90;
              }
          }
          else {
              world_x = o_current->line->x[0];
              world_y = o_current->line->y[0];

              align = LOWER_LEFT;
              angle = 0;
          }

          color = ATTRIBUTE_COLOR;
        }
        break;

        /* We cannot attach an attribute to text, so create a
         * toplevel attribute here. */
      case(OBJ_TEXT):
        world_x = lepton_text_object_get_x (o_current);
        world_y = lepton_text_object_get_y (o_current);
        color = DETACHED_ATTRIBUTE_COLOR;
        align = LOWER_LEFT;
        angle = 0;
        o_current = NULL;
        break;
    }
  }
  else
  {
    /* Creating a toplevel (unattached, floating) attribute. */

    /* If any coordinate is proposed, set the attrib anchor to it.  */
    if (proposed_coord)
    {
      world_x = x;
      world_y = y;
    }
    else
    {
      /* Otherwise, try first to set the coordinate to the
       * bottom-left corner of the visible objects. */
      found =
        world_get_object_glist_bounds (lepton_page_objects (active_page),
                                       /* Don't include hidden objects. */
                                       FALSE,
                                       &left,
                                       &top,
                                       &right,
                                       &bottom);
      if (found)
      {
        world_x = left;
        world_y = top;
      }
      else
      {
        /* No visible object was found on the canvas, and no coord
         * is proposed.  Set the value to the center of the current
         * page view. */
        GschemPageView *view = gschem_toplevel_get_current_page_view (w_current);

        if (view != NULL)
        {
          GschemPageGeometry *geometry = gschem_page_view_get_page_geometry (view);
          world_x = (geometry->viewport_left + geometry->viewport_right) / 2;
          world_y = (geometry->viewport_top + geometry->viewport_bottom) / 2;
        }
        else
        {
          /* Hmm, no page view found?  Ah, the function can be
           * evaluated somewhere in Scheme code when no GUI is
           * available.  The last resort.  Let's set the coord to
           * a specific value. */
          world_x = 0;
          world_y = 0;
        }
      }
    }

    /* printf("%d %d\n", world_x, world_y); */
    align = LOWER_LEFT;
    angle = 0;
    color = DETACHED_ATTRIBUTE_COLOR;
  }

  /* first create text item */
  new_obj = lepton_text_object_new (color,
                                    world_x,
                                    world_y,
                                    align,
                                    angle,
                                    text_string,
                                    w_current->text_size, /* current text size */
                                    visibility,
                                    show_name_value);
  lepton_page_append (active_page, new_obj);

  /* now attach the attribute to the object (if o_current is not NULL) */
  /* remember that o_current contains the object to get the attribute */
  if (o_current) {
    o_attrib_attach (new_obj, o_current, FALSE);
  }

  /* Select the attribute to enable its further processing. */
  o_selection_add (active_page->selection_list, new_obj);

  /* handle slot= attribute, it's a special case */
  if (o_current != NULL &&
      g_ascii_strncasecmp (text_string, "slot=", 5) == 0) {
    o_slot_end (w_current, o_current, text_string);
  }

  /* Call add-objects-hook. */
  g_run_hook_object (w_current, "add-objects-hook", new_obj);
  g_run_hook_object (w_current, "select-objects-hook", new_obj);

  schematic_window_active_page_changed (w_current);

  return new_obj;
}
