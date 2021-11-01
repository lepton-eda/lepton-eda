/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2021 Lepton EDA Contributors
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
    if (!a_current->selected) {
      o_selection_add (selection, a_current);
      selected_objects = g_list_prepend (selected_objects, a_current);
    }
  }

  if (selected_objects != NULL) {
    /* Run select-objects-hook */
    g_run_hook_object_list (w_current, "%select-objects-hook",
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

    if (a_current->selected && !lepton_text_object_is_visible (a_current)) {
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

    if (!a_current->selected && !lepton_text_object_is_visible (a_current)) {
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
  LeptonToplevel *toplevel = gschem_toplevel_get_toplevel (w_current);

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

  gschem_toplevel_page_content_changed (w_current, toplevel->page_current);
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
  LeptonToplevel *toplevel = gschem_toplevel_get_toplevel (w_current);

  g_return_if_fail (lepton_object_is_text (object));

  o_invalidate (w_current, object);
  lepton_text_object_set_show (object, show_name_value);
  lepton_text_object_recreate (object);

  gschem_toplevel_page_content_changed (w_current, toplevel->page_current);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/* This function no longer returns NULL, but will always return the new */
/* text item */
LeptonObject*
o_attrib_add_attrib (GschemToplevel *w_current,
                     const char *text_string,
                     int visibility,
                     int show_name_value,
                     LeptonObject *object)
{
  LeptonToplevel *toplevel = gschem_toplevel_get_toplevel (w_current);
  LeptonObject *new_obj;
  int world_x = - 1, world_y = -1;
  int align = LOWER_LEFT;
  int angle = 0;
  int color;
  int left, right, top, bottom;
  LeptonObject *o_current;

  color = DETACHED_ATTRIBUTE_COLOR;

  o_current = object;

  /* creating a toplevel or unattached attribute */
  if (o_current) {
    /* get coordinates of where to place the text object */
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

      case(OBJ_TEXT):
        world_x = lepton_text_object_get_x (o_current);
        world_y = lepton_text_object_get_y (o_current);
        color = DETACHED_ATTRIBUTE_COLOR;
        align = LOWER_LEFT;
        angle = 0;
        o_current = NULL;
        break;
    }
  } else {
    world_get_object_glist_bounds (lepton_page_objects (toplevel->page_current),
                                   /* Don't include hidden objects. */
                                   FALSE,
                                   &left,
                                   &top,
                                   &right,
                                   &bottom);

    /* this really is the lower left hand corner */
    world_x = left;
    world_y = top;

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
  lepton_page_append (toplevel->page_current, new_obj);

  /* now attach the attribute to the object (if o_current is not NULL) */
  /* remember that o_current contains the object to get the attribute */
  if (o_current) {
    o_attrib_attach (new_obj, o_current, FALSE);
  }

  o_selection_add (toplevel->page_current->selection_list, new_obj);

  /* handle slot= attribute, it's a special case */
  if (o_current != NULL &&
      g_ascii_strncasecmp (text_string, "slot=", 5) == 0) {
    o_slot_end (w_current, o_current, text_string);
  }

  /* Call add-objects-hook. */
  g_run_hook_object (w_current, "%add-objects-hook", new_obj);
  g_run_hook_object (w_current, "%select-objects-hook", new_obj);

  gschem_toplevel_page_content_changed (w_current, toplevel->page_current);

  return new_obj;
}
