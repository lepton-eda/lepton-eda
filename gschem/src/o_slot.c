/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2008 Ales Hvezda
 * Copyright (C) 1998-2008 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
 */
#include <config.h>

#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "gschem.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#define MAX_SLOT_SIZE 10

/*! \todo Finish function documentation!!!
 *  \brief Change slot of selected component
 *  \par Function Description
 *
 */
void o_slot_start (GSCHEM_TOPLEVEL *w_current, OBJECT *object)
{
  char *slot_value;

  /* single object for now */
  if (object->type != OBJ_COMPLEX)
    return;

  slot_value = o_attrib_search_object_attribs_by_name (object, "slot", 0);

  if (slot_value == NULL) {
    /* we didn't find a slot=? attribute, make something up */
    /* for now.. this is an error condition */
    slot_value = g_strdup ("1");
  }

  slot_edit_dialog (w_current, slot_value);
  g_free (slot_value);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_slot_end(GSCHEM_TOPLEVEL *w_current, const char *string)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  OBJECT *new_obj;
  OBJECT *object;
  char *slot_value;
  char *numslots_value;
  OBJECT *o_slot;
  char *value = NULL;
  int numslots;
  int new_slot_number;
  int status;

  status = o_attrib_string_get_name_value (string, NULL, &value);
  if (!status) {
    s_log_message(_("Slot attribute malformed\n"));
    return;
  }

  object = o_select_return_first_object(w_current);

  /* get the parent object if the selection is only a text object */
  if (object != NULL && object->type == OBJ_TEXT) {
    if (object->attached_to != NULL) {
      object = object->attached_to;
    }
  }

  if (object != NULL) {
    numslots_value =
      o_attrib_search_object_attribs_by_name (object, "numslots", 0);

    if (!numslots_value) {
      s_log_message(_("numslots attribute missing\n"));
      s_log_message(_("Slotting not allowed for this component\n"));
      g_free(value);
      return;
    }

    numslots = atoi(numslots_value);
    g_free(numslots_value);

    new_slot_number = atoi(value);

#if DEBUG
    printf("numslots = %d\n", numslots);
#endif

    if (new_slot_number > numslots || new_slot_number <=0 ) {
      s_log_message(_("New slot number out of range\n"));
      g_free(value);
      return;
    }

    /* first see if slot attribute already exists outside
     * complex */
    slot_value = s_slot_search_slot (object, &o_slot);
    g_free (slot_value);

    if (o_slot != NULL && !o_attrib_is_inherited (o_slot)) {
      o_text_set_string (toplevel, o_slot, string);

      if (o_slot->visibility == VISIBLE ||
          (o_slot->visibility == INVISIBLE && toplevel->show_hidden_text)) {
        o_invalidate (w_current, o_slot);
      }

      o_text_recreate(toplevel, o_slot);

      /* this doesn't deal with the selection list
       * item */
      if (o_slot->visibility == VISIBLE ||
          (o_slot->visibility == INVISIBLE && toplevel->show_hidden_text)) {
        o_invalidate (w_current, o_slot);
      }

    } else {
      /* here you need to do the add the slot
         attribute since it doesn't exist */
      new_obj = o_text_new (toplevel, OBJ_TEXT, ATTRIBUTE_COLOR,
                            object->complex->x, object->complex->y,
                            LOWER_LEFT, 0, /* zero is angle */
                            string, 10, INVISIBLE, SHOW_NAME_VALUE);
      s_page_append (toplevel, toplevel->page_current, new_obj);

      /* manually attach attribute */
      o_attrib_attach (toplevel, new_obj, object, FALSE);
    }

    o_invalidate (w_current, object);
    s_slot_update_object (toplevel, object);

    o_invalidate (w_current,object);

    toplevel->page_current->CHANGED = 1;
    g_free(value);

  } else {
    fprintf(stderr,
            _("uggg! you tried to slot edit something that doesn't exist!\n"));
    g_free(value);
    exit(-1);
  }
}
