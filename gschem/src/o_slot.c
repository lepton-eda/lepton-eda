/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2000 Ales V. Hvezda
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

#include <libgeda/libgeda.h>

#include "../include/globals.h"
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#define MAX_SLOT_SIZE 10

/*! \todo Finish function documentation!!!
 *  \brief Change slot of selected component
 *  \par Function Description
 *
 */
void o_slot_start(TOPLEVEL *w_current, OBJECT *list)
{
  OBJECT *object;
  OBJECT *slot_text_object;
  char *default_slot_value;
  char *slot_value;

  /* shouldn't happen */
  if (list == NULL) {
    /* this is an error condition hack */
    w_current->inside_action = 0;
    i_set_state(w_current, SELECT);
    return;
  }

  object = o_select_return_first_object(w_current);

  /* single object for now */
  if (object->type == OBJ_COMPLEX) {
    /* first see if slot attribute already exists outside
     * complex */
    slot_value = o_attrib_search_slot(object, &slot_text_object);

    if (slot_value) {
#if DEBUG
      printf("slot=%s\n", slot_value);
      printf("text string : %s\n",
             slot_text_object->text->string);
#endif
      slot_edit_dialog(w_current,
                       slot_text_object->text->string);
      g_free(slot_value);
    } else {
      /* we didn't find an attached slot=? attribute */

      /* See if there is a default value */
      default_slot_value =
        o_attrib_search_default_slot(object);

      if (default_slot_value) {
				/* two is for null and equals sign */
        slot_value = (char *) g_malloc(sizeof(char)*(
                                                   strlen("slot")+
                                                   strlen(default_slot_value)+
                                                   2));
        sprintf(slot_value, "slot=%s",
                default_slot_value);
      } else {
				/* no default, make something up? */
				/* for now.. this is an error
                                   condition */
        slot_value = g_strdup ("slot=1");
      }

#if DEBUG
      printf("slot value: %s\n", slot_value);
#endif

      slot_edit_dialog(w_current, slot_value);
      g_free(slot_value);
      g_free(default_slot_value);
    }
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_slot_end(TOPLEVEL *w_current, char *string, int len)
{
  OBJECT *object;
  OBJECT *temp;
  char *slot_value;
  char *numslots_value;
  OBJECT *slot_text_object;
  char *name = NULL;
  char *value = NULL;
  int numslots;
  int new_slot_number;
  int status;

  status = o_attrib_get_name_value(string, &name, &value);
  if (!status) {
    s_log_message(_("Slot attribute malformed\n"));
    return;
  }

  object = o_select_return_first_object(w_current);

  /* now find the slot attribute on the outside first */
  if (object != NULL) {
    numslots_value = o_attrib_search_numslots(object, NULL);

    if (!numslots_value) {
      s_log_message(_("numslots attribute missing\n"));
      s_log_message(
                    _("Slotting not allowed for this component\n"));
      if (name) g_free(name);
      if (value) g_free(value);
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
      if (name) g_free(name);
      if (value) g_free(value);
      return;
    }

    /* first see if slot attribute already exists outside
     * complex */
    slot_value = o_attrib_search_slot(object, &slot_text_object);

    if (slot_value) {
      if (slot_text_object->text->string) {
        g_free(slot_text_object->text->string);
      }

      slot_text_object->text->string = g_strdup (string);

      temp = slot_text_object;

      if (temp->visibility == VISIBLE ||
          (temp->visibility == INVISIBLE && w_current->show_hidden_text)) {
        o_erase_single(w_current,temp);
      }

      o_text_recreate(w_current, temp);

      /* this doesn't deal with the selection list
       * item */
      if (temp->visibility == VISIBLE ||
          (temp->visibility == INVISIBLE && w_current->show_hidden_text)) {
        o_redraw_single(w_current,temp);
      }

      g_free(slot_value);

    } else {
      /* here you need to do the add the slot
         attribute since it doesn't exist */
      w_current->page_current->object_tail =
        (OBJECT *) o_text_add(
                              w_current,
                              w_current->page_current->object_tail,
                              OBJ_TEXT, w_current->text_color,
                              object->complex->x, object->complex->y,
                              LOWER_LEFT,
                              0, /* zero is angle */
                              string,
                              10,
                              INVISIBLE, SHOW_NAME_VALUE);

      /* manually attach attribute */

      /* NEWSEL this is okay too, since tail is single obj */
      o_attrib_attach(w_current,
                      w_current->page_current->object_head,
                      w_current->page_current->object_tail,
                      object);

      slot_text_object =
        w_current->page_current->object_tail;
    }

    o_erase_single(w_current, object);
    o_attrib_slot_update(w_current, object);


#if 0 /* NEWSEL */
    /* why? */
    /* erase the selection list */
    o_erase_selected(w_current);

    o_attrib_slot_copy(w_current, object,
                       w_current->page_current->selection_head->next);
    o_redraw_single(w_current,object);
#endif

    o_redraw_single(w_current,object);

    w_current->page_current->CHANGED = 1;
    o_undo_savestate(w_current, UNDO_ALL);
    if (name) g_free(name);
    if (value) g_free(value);

  } else {
    fprintf(stderr,
            _("uggg! you tried to slot edit something that doesn't exist!\n"));
    if (name) g_free(name);
    if (value) g_free(value);
    exit(-1);
  }
}
