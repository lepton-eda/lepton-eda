/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2025 Lepton EDA Contributors
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
#include <ctype.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <liblepton/glib_compat.h>
#include "schematic.h"
#include <gdk/gdkkeysyms.h>

#define GLADE_HOOKUP_OBJECT(component,widget,name) \
  g_object_set_data_full (G_OBJECT (component), name, \
    g_object_ref (widget), (GDestroyNotify) g_object_unref)

/** @brief How many entries to keep in the "Search text" combo box. */
#define HISTORY_LENGTH 15


static SchematicAutonumber *autotext = NULL;

/* Accessors */

/*! \brief Get the value of the \c autotext variable.
 *
 *  \par Function Description
 *   Returns the static \c autotext variable.
 *
 *  \return The current value of the \c autotext variable.
 */
SchematicAutonumber*
schematic_autonumber_get_autotext ()
{
  return autotext;
}

/*! \brief Set the value of the \c autotext variable.
 *
 *  \par Function Description
 *   Set the static \c autotext variable to the given value.
 *
 *  \param [in] val The new value of the \c autotext variable.
 */
void
schematic_autonumber_set_autotext (SchematicAutonumber *val)
{
  autotext = val;
}


/*! \brief Get the \c current_searchtext field of a
 *  #SchematicAutonumber instance.
 *
 *  \par Function Description
 *  Returns the value of the \c current_searchtext field of a
 *  #SchematicAutonumber instance.
 *
 *  \param [in] autotext The #SchematicAutonumber instance.
 *  \return The value of the field.
 */
char*
schematic_autonumber_get_autotext_current_searchtext (SchematicAutonumber *autotext)
{
  g_return_val_if_fail (autotext != NULL, NULL);

  return autotext->current_searchtext;
}


/*! \brief Set the \c current_searchtext field of a
 *  #SchematicAutonumber instance.
 *
 *  \par Function Description
 *  Sets the value of the \c current_searchtext field of a
 *  #SchematicAutonumber instance.
 *
 *  \param [in,out] autotext The #SchematicAutonumber instance.
 *  \param [in] val The new value of the field.
 */
void
schematic_autonumber_set_autotext_current_searchtext (SchematicAutonumber *autotext,
                                                      char *val)
{
  g_return_if_fail (autotext != NULL);

  autotext->current_searchtext = val;
}


/*! \brief Get the dialog widget of a #SchematicAutonumber
 *  instance.
 *
 *  \par Function Description
 *  Returns the value of the \c dialog field of a
 *  #SchematicAutonumber object.
 *
 *  \param [in] autotext The #SchematicAutonumber instance.
 *  \return The dialog widget of the instance.
 */
GtkWidget*
schematic_autonumber_get_autotext_dialog (SchematicAutonumber *autotext)
{
  g_return_val_if_fail (autotext != NULL, NULL);

  return autotext->dialog;
}

/*! \brief Set the dialog widget of a #SchematicAutonumber
 *  instance.
 *
 *  \par Function Description
 *  Sets the value of the \c dialog field of a
 *  #SchematicAutonumber instance.
 *
 *  \param [in,out] autotext The #SchematicAutonumber instance.
 *  \param [in] widget The dialog widget.
 */
void
schematic_autonumber_set_autotext_dialog (SchematicAutonumber *autotext,
                                          GtkWidget *widget)
{
  g_return_if_fail (autotext != NULL);

  autotext->dialog = widget;
}


/*! \brief Get the \c free_slots field of a #SchematicAutonumber
 *  instance.
 *
 *  \par Function Description
 *  Returns the \c free_slots field value of a #SchematicAutonumber
 *  instance.
 *
 *  \param [in] autotext The #SchematicAutonumber instance.
 *  \return The value of the \c free_slots field.
 */
GList*
schematic_autonumber_get_autotext_free_slots (SchematicAutonumber *autotext)
{
  g_return_val_if_fail (autotext != NULL, NULL);

  return autotext->free_slots;
}


/*! \brief Set the \c free_slots field of a #SchematicAutonumber
 *  instance.
 *
 *  \par Function Description
 *  Sets the value of the \c free_slots field of a
 *  #SchematicAutonumber instance.
 *
 *  \param [in,out] autotext The #SchematicAutonumber instance.
 *  \param [in] val The new value of the field.
 */
void
schematic_autonumber_set_autotext_free_slots (SchematicAutonumber *autotext,
                                              GList *val)
{
  g_return_if_fail (autotext != NULL);

  autotext->free_slots = val;
}


/*! \brief Get the \c removenum field of a #SchematicAutonumber
 *  instance.
 *
 *  \par Function Description
 *  Returns the \c removenum field value of a #SchematicAutonumber
 *  instance.
 *
 *  \param [in] autotext The #SchematicAutonumber instance.
 *  \return The value of the \c removenum field.
 */
gboolean
schematic_autonumber_get_autotext_removenum (SchematicAutonumber *autotext)
{
  g_return_val_if_fail (autotext != NULL, FALSE);

  return autotext->removenum;
}

/*! \brief Set the \c removenum field of a #SchematicAutonumber
 *  instance.
 *
 *  \par Function Description
 *  Sets the \c removenum field value of a #SchematicAutonumber
 *  instance.
 *
 *  \param [in,out] autotext The #SchematicAutonumber instance.
 *  \param [in] val The new value of the field.
 */
void
schematic_autonumber_set_autotext_removenum (SchematicAutonumber *autotext,
                                             gboolean val)
{
  g_return_if_fail (autotext != NULL);

  autotext->removenum = val;
}


/*! \brief Get the \c root_page field of a #SchematicAutonumber
 *  instance.
 *
 *  \par Function Description
 *  Returns the \c root_page field value of a #SchematicAutonumber
 *  instance.
 *
 *  \param [in] autotext The #SchematicAutonumber instance.
 *  \return The value of the \c root_page field.
 */
int
schematic_autonumber_get_autotext_root_page (SchematicAutonumber *autotext)
{
  g_return_val_if_fail (autotext != NULL, TRUE);

  return autotext->root_page;
}


/*! \brief Set the \c root_page field of a #SchematicAutonumber
 *  instance.
 *
 *  \par Function Description
 *  Sets the \c root_page field value of a #SchematicAutonumber
 *  instance.
 *
 *  \param [in,out] autotext The #SchematicAutonumber instance.
 *  \param [in] val The new value of the field.
 */
void
schematic_autonumber_set_autotext_root_page (SchematicAutonumber *autotext,
                                             int val)
{
  g_return_if_fail (autotext != NULL);

  autotext->root_page = val;
}


/*! \brief Get the \c scope_number field of a #SchematicAutonumber
 *  instance.
 *
 *  \par Function Description
 *  Returns the \c scope_number field value of a
 *  #SchematicAutonumber instance.
 *
 *  \param [in] autotext The #SchematicAutonumber instance.
 *  \return The value of the \c scope_number field.
 */
int
schematic_autonumber_get_autotext_scope_number (SchematicAutonumber *autotext)
{
  g_return_val_if_fail (autotext != NULL, SCOPE_SELECTED);

  return autotext->scope_number;
}

/*! \brief Set the \c scope_number field of a #SchematicAutonumber
 *  instance.
 *
 *  \par Function Description
 *  Sets the \c scope_number field value of a #SchematicAutonumber
 *  instance.
 *
 *  \param [in,out] autotext The #SchematicAutonumber instance.
 *  \param [in] val The new value of the field.
 */
void
schematic_autonumber_set_autotext_scope_number (SchematicAutonumber *autotext,
                                                int val)
{
  g_return_if_fail (autotext != NULL);

  autotext->scope_number = val;
}


/*! \brief Get the \c scope_overwrite field of a
 *  #SchematicAutonumber instance.
 *
 *  \par Function Description
 *  Returns the value of the \c scope_overwrite field of a
 *  #SchematicAutonumber instance.
 *
 *  \param [in] autotext The #SchematicAutonumber instance.
 *  \return The value of the field.
 */
gboolean
schematic_autonumber_get_autotext_scope_overwrite (SchematicAutonumber *autotext)
{
  g_return_val_if_fail (autotext != NULL, FALSE);

  return autotext->scope_overwrite;
}


/*! \brief Set the \c scope_overwrite field of a
 *  #SchematicAutonumber instance.
 *
 *  \par Function Description
 *  Sets the \c scope_overwrite field value of a
 *  #SchematicAutonumber instance.
 *
 *  \param [in,out] autotext The #SchematicAutonumber instance.
 *  \param [in] val The new value of the field.
 */
void
schematic_autonumber_set_autotext_scope_overwrite (SchematicAutonumber *autotext,
                                                   gboolean val)
{
  g_return_if_fail (autotext != NULL);

  autotext->scope_overwrite = val;
}


/*! \brief Get the \c scope_skip field of a #SchematicAutonumber
 *  instance.
 *
 *  \par Function Description
 *  Returns the value of the \c scope_skip field of a
 *  #SchematicAutonumber instance.
 *
 *  \param [in] autotext The #SchematicAutonumber instance.
 *  \return The value of the \c scope_skip field.
 */
int
schematic_autonumber_get_autotext_scope_skip (SchematicAutonumber *autotext)
{
  g_return_val_if_fail (autotext != NULL, SCOPE_PAGE);

  return autotext->scope_skip;
}


/*! \brief Set the \c scope_skip field of a #SchematicAutonumber
 *  instance.
 *
 *  \par Function Description
 *  Sets the value of the \c scope_skip field of a
 *  #SchematicAutonumber instance.
 *
 *  \param [in,out] autotext The #SchematicAutonumber instance.
 *  \param [in] val The new value of the \c scope_skip field.
 */
void
schematic_autonumber_set_autotext_scope_skip (SchematicAutonumber *autotext,
                                              int val)
{
  g_return_if_fail (autotext != NULL);

  autotext->scope_skip = val;
}


/*! \brief Get the \c scope_text field of a #SchematicAutonumber
 *  instance.
 *
 *  \par Function Description
 *  Returns the value of the \c scope_text field of a
 *  #SchematicAutonumber instance.
 *
 *  \param [in] autotext The #SchematicAutonumber instance.
 *  \return The value of the \c scope_text field.
 */
GList*
schematic_autonumber_get_autotext_scope_text (SchematicAutonumber *autotext)
{
  g_return_val_if_fail (autotext != NULL, NULL);

  return autotext->scope_text;
}


/*! \brief Set the \c scope_text field of a #SchematicAutonumber
 *  instance.
 *
 *  \par Function Description
 *  Sets the value of the \c scope_text field of a
 *  #SchematicAutonumber instance.
 *
 *  \param [in,out] autotext The #SchematicAutonumber instance.
 *  \param [in] val The new value of the \c scope_text field.
 */
void
schematic_autonumber_set_autotext_scope_text (SchematicAutonumber *autotext,
                                              GList *val)
{
  g_return_if_fail (autotext != NULL);

  autotext->scope_text = val;
}


/*! \brief Get the \c slotting field of a #SchematicAutonumber
 *  instance.
 *
 *  \par Function Description
 *  Returns the value of the \c slotting field of a
 *  #SchematicAutonumber instance.
 *
 *  \param [in] autotext The #SchematicAutonumber instance.
 *  \return The value of the \c slotting field.
 */
gboolean
schematic_autonumber_get_autotext_slotting (SchematicAutonumber *autotext)
{
  g_return_val_if_fail (autotext != NULL, FALSE);

  return autotext->slotting;
}


/*! \brief Set the value of \c slotting field of a
 *  #SchematicAutonumber instance.
 *
 *  \par Function Description
 *  Sets the value of the \c slotting field of a
 *  #SchematicAutonumber instance.
 *
 *  \param [in,out] autotext The #SchematicAutonumber instance.
 *  \param [in] val The new value of the \c slotting field.
 */
void
schematic_autonumber_set_autotext_slotting (SchematicAutonumber *autotext,
                                            gboolean val)
{
  g_return_if_fail (autotext != NULL);

  autotext->slotting = val;
}


/*! \brief Set the \c order field of a #SchematicAutonumber
 *  instance.
 *
 *  \par Function Description
 *  Returns the value of the \c order field of a
 *  #SchematicAutonumber instance.
 *
 *  \param [in] autotext The #SchematicAutonumber instance.
 *  \return The value of the \c order field.
 */
int
schematic_autonumber_get_autotext_sort_order (SchematicAutonumber *autotext)
{
  g_return_val_if_fail (autotext != NULL, AUTONUMBER_SORT_DIAGONAL);

  return autotext->order;
}


/*! \brief Set the \c order field of a #SchematicAutonumber
 *  instance.
 *
 *  \par Function Description
 *  Sets the \c order field value of a #SchematicAutonumber
 *  instance.
 *
 *  \param [in,out] autotext The #SchematicAutonumber instance.
 *  \param [in] val The new value of the \c order field.
 */
void
schematic_autonumber_set_autotext_sort_order (SchematicAutonumber *autotext,
                                              int val)
{
  g_return_if_fail (autotext != NULL);

  autotext->order = val;
}


/*! \brief Get the start number field of a #SchematicAutonumber
 *  instance.
 *
 *  \par Function Description
 *  Returns the value of the \c startnum field of a
 *  #SchematicAutonumber instance.
 *
 *  \param [in] autotext The #SchematicAutonumber instance.
 *  \return The valueo of the \c startnum field.
 */
int
schematic_autonumber_get_autotext_startnum (SchematicAutonumber *autotext)
{
  g_return_val_if_fail (autotext != NULL, 1);

  return autotext->startnum;
}


/*! \brief Set the start number field of a #SchematicAutonumber
 *  instance.
 *
 *  \par Function Description
 *  Sets the value of the \c startnum field of a
 *  #SchematicAutonumber instance.
 *
 *  \param [in,out] autotext The #SchematicAutonumber instance.
 *  \param [in] val The new value of the \c startnum field.
 */
void
schematic_autonumber_set_autotext_startnum (SchematicAutonumber *autotext,
                                            int val)
{
  g_return_if_fail (autotext != NULL);

  autotext->startnum = val;
}


/*! \brief Get the \c used_numbers field of a #SchematicAutonumber
 *  instance.
 *
 *  \par Function Description
 *  Returns the value of the \c used_numbers field of a
 *  #SchematicAutonumber instance.
 *
 *  \param [in] autotext The #SchematicAutonumber instance.
 *  \return The value of the field.
 */
GList*
schematic_autonumber_get_autotext_used_numbers (SchematicAutonumber *autotext)
{
  g_return_val_if_fail (autotext != NULL, NULL);

  return autotext->used_numbers;
}


/*! \brief Set the \c used_numbers field of a #SchematicAutonumber
 *  instance.
 *
 *  \par Function Description
 *  Sets the \c used_numbers field value of a #SchematicAutonumber
 *  instance.
 *
 *  \param [in,out] autotext The #SchematicAutonumber instance.
 *  \param [in] val The new value of the field.
 */
void
schematic_autonumber_set_autotext_used_numbers (SchematicAutonumber *autotext,
                                                GList *val)
{
  g_return_if_fail (autotext != NULL);

  autotext->used_numbers = val;
}


/*! \brief Get the \c used_slots field of a #SchematicAutonumber
 *  instance.
 *
 *  \par Function Description
 *  Returns the value of the \c used_slots field of a
 *  #SchematicAutonumber instance.
 *
 *  \param [in] autotext The #SchematicAutonumber instance.
 *  \return The value of the \c used_slots field.
 */
GList*
schematic_autonumber_get_autotext_used_slots (SchematicAutonumber *autotext)
{
  g_return_val_if_fail (autotext != NULL, NULL);

  return autotext->used_slots;
}


/*! \brief Set the \c used_slots field of a #SchematicAutonumber
 *  instance.
 *
 *  \par Function Description
 *  Sets the \c used_slots field value of a #SchematicAutonumber
 *  instance.
 *
 *  \param [in,out] autotext The #SchematicAutonumber instance.
 *  \param [in] val The new value of the field.
 */
void
schematic_autonumber_set_autotext_used_slots (SchematicAutonumber *autotext,
                                              GList *val)
{
  g_return_if_fail (autotext != NULL);

  autotext->used_slots = val;
}


/*! \brief Get the window field of a #SchematicAutonumber instance.
 *
 *  \par Function Description
 *  Returns the window field value of \p autotext.
 *
 *  \param [in] autotext The #SchematicAutonumber instance.
 *  \return The window field of the instance.
 */
SchematicWindow*
schematic_autonumber_get_autotext_window (SchematicAutonumber *autotext)
{
  g_return_val_if_fail (autotext != NULL, NULL);

  return autotext->w_current;
}


/*! \brief Set the window field of a #SchematicAutonumber instance.
 *  \par Function Description
 *  Sets the window field value of \p autotext to \p w_current.
 *
 *  \param [in,out] autotext The #SchematicAutonumber instance.
 *  \param [in] w_current The new value of the window field.
 */
void
schematic_autonumber_set_autotext_window (SchematicAutonumber *autotext,
                                          SchematicWindow *w_current)
{
  g_return_if_fail (autotext != NULL);

  autotext->w_current = w_current;
}


/* ***** BACK-END CODE ***************************************************** */

/********** compare functions for g_list_sort, ... ***********************/
/*! \brief GCompareFunc function to sort a list with g_list_sort().
 *  \par Function Description
 *  Compares the integer values of the gconstpointers a and b.
 *  \return -1 if a<b, 1 if a>b, 0 if a==b
 */
gint autonumber_sort_numbers(gconstpointer a, gconstpointer b) {
  if (GPOINTER_TO_INT(a) < GPOINTER_TO_INT(b))
    return -1;
  if (GPOINTER_TO_INT(a) > GPOINTER_TO_INT(b))
    return 1;
  return 0;
}

/*! \brief GCompareFunc function to sort text objects by there location
 *  \par Function Description
 *  This function takes two <B>LeptonObject*</B> arguments and compares the
 *  location of the two text objects. The first sort criteria is the x location,
 *  the second sort criteria is the y location.
 *  The Function is used as GCompareFunc by g_list_sort().
 */
gint autonumber_sort_xy(gconstpointer a, gconstpointer b) {
  LeptonObject *aa, *bb;
  aa=(LeptonObject *) a;  bb=(LeptonObject *) b;
  if (lepton_text_object_get_x (aa) < lepton_text_object_get_x (bb))
    return -1;
  if (lepton_text_object_get_x (aa) > lepton_text_object_get_x (bb))
    return 1;
  /* x == x */
  if (lepton_text_object_get_y (aa) > lepton_text_object_get_y (bb))
    return -1;
  if (lepton_text_object_get_y (aa) < lepton_text_object_get_y (bb))
    return 1;
  return 0;
}

/*! \brief GCompareFunc function to sort text objects by there location
 *  \par Function Description
 *  This function takes two <B>LeptonObject*</B> arguments and compares the
 *  location of the two text objects. The first sort criteria is the x location,
 *  the second sort criteria is the y location.
 *  This function sorts the objects in reverse order.
 *  The function is used as GCompareFunc by g_list_sort().
 */
gint autonumber_sort_xy_rev(gconstpointer a, gconstpointer b) {
  LeptonObject *aa, *bb;
  aa=(LeptonObject *) a;  bb=(LeptonObject *) b;
  if (lepton_text_object_get_x (aa) < lepton_text_object_get_x (bb))
    return 1;
  if (lepton_text_object_get_x (aa) > lepton_text_object_get_x (bb))
    return -1;
  /* x == x */
  if (lepton_text_object_get_y (aa) < lepton_text_object_get_y (bb))
    return 1;
  if (lepton_text_object_get_y (aa) > lepton_text_object_get_y (bb))
    return -1;
  return 0;
}

/*! \brief GCompareFunc function to sort text objects by there location
 *  \par Function Description
 *  This function takes two <B>LeptonObject*</B> arguments and compares the
 *  location of the two text objects. The first sort criteria is the y location,
 *  the second sort criteria is the x location.
 *  The function is used as GCompareFunc by g_list_sort().
 */
int autonumber_sort_yx(gconstpointer a, gconstpointer b) {
  LeptonObject *aa, *bb;
  aa=(LeptonObject *) a;  bb=(LeptonObject *) b;
  if (lepton_text_object_get_y (aa) > lepton_text_object_get_y (bb))
    return -1;
  if (lepton_text_object_get_y (aa) < lepton_text_object_get_y (bb))
    return 1;
  /* y == y */
  if (lepton_text_object_get_x (aa) < lepton_text_object_get_x (bb))
    return -1;
  if (lepton_text_object_get_x (aa) > lepton_text_object_get_x (bb))
    return 1;
  return 0;
}

/*! \brief GCompareFunc function to sort text objects by there location
 *  \par Function Description
 *  This function takes two <B>LeptonObject*</B> arguments and compares the
 *  location of the two text objects. The first sort criteria is the y location,
 *  the second sort criteria is the x location.
 *  This function sorts the objects in reverse order.
 *  The function is used as GCompareFunc by g_list_sort().
 */
int autonumber_sort_yx_rev(gconstpointer a, gconstpointer b) {
  LeptonObject *aa, *bb;
  aa=(LeptonObject *) a;  bb=(LeptonObject *) b;
  if (lepton_text_object_get_y (aa) > lepton_text_object_get_y (bb))
    return 1;
  if (lepton_text_object_get_y (aa) < lepton_text_object_get_y (bb))
    return -1;
  /* y == y */
  if (lepton_text_object_get_x (aa) > lepton_text_object_get_x (bb))
    return 1;
  if (lepton_text_object_get_x (aa) < lepton_text_object_get_x (bb))
    return -1;
  return 0;
}

/*! \brief GCompareFunc function to sort text objects by there location
 *  \par Function Description
 *  This function takes two <B>LeptonObject*</B> arguments and compares the
 *  location of the two text objects. The sort criteria is the combined x- and the
 *  y-location. The function sorts from top left to bottom right.
 *  The function is used as GCompareFunc by g_list_sort().
 */
int autonumber_sort_diagonal(gconstpointer a, gconstpointer b) {
  LeptonObject *aa, *bb;
  aa=(LeptonObject *) a;  bb=(LeptonObject *) b;
  if (lepton_text_object_get_x (aa) - lepton_text_object_get_y (aa) <
      lepton_text_object_get_x (bb) - lepton_text_object_get_y (bb))
    return -1;
  if (lepton_text_object_get_x (aa) - lepton_text_object_get_y (aa) >
      lepton_text_object_get_x (bb) - lepton_text_object_get_y (bb))
    return 1;
  return 0;
}


/*! \brief Get the \c number field of a slot object.
 *
 * \par Function Description
 * Returns the \c number field of the object \p slot which is
 * usually the refdes number of the symbol it belongs to.
 *
 * \param [in] slot The #SchematicAutonumberSlot instance.
 * \return The number field of the slot object.
 */
int
schematic_autonumber_slot_get_number (SchematicAutonumberSlot *slot)
{
  g_return_val_if_fail (slot != NULL, 0);

  return slot->number;
}

/*! \brief Set the \c number field of a slot object.
 *
 * \par Function Description
 * Sets the \c number field of a #SchematicAutonumberSlot
 * instance.
 *
 * \param [in] slot The #SchematicAutonumberSlot instance.
 * \param [in] number A new number.
 */
void
schematic_autonumber_slot_set_number (SchematicAutonumberSlot *slot,
                                      int number)
{
  g_return_if_fail (slot != NULL);

  slot->number = number;
}


/*! \brief Get the slot number value of a slot object.
 *
 * \par Function Description
 * Returns the slot number value of the object \p slot which is
 * usually the number of the next free slot for the symbol it
 * belongs to.
 *
 * \param [in] slot The #SchematicAutonumberSlot instance.
 * \return The number field of the slot object.
 */
int
schematic_autonumber_slot_get_slot_number (SchematicAutonumberSlot *slot)
{
  g_return_val_if_fail (slot != NULL, 0);

  return slot->slotnr;
}

/*! \brief Set the slot number value for a slot object.
 *
 * \par Function Description
 * Sets the slot number field for a #SchematicAutonumberSlot
 * instance.
 *
 * \param [in] slot The #SchematicAutonumberSlot instance.
 * \param [in] number A new slot number.
 */
void
schematic_autonumber_slot_set_slot_number (SchematicAutonumberSlot *slot,
                                           int number)
{
  g_return_if_fail (slot != NULL);

  slot->slotnr = number;
}


/*! \brief Get the symbol name of a slot object.
 *
 * \par Function Description
 * Returns the name of the symbol the object \p slot belongs to.
 *
 * \param [in] slot The #SchematicAutonumberSlot instance.
 * \return The slot symbol name.
 */
char*
schematic_autonumber_slot_get_symbol_name (SchematicAutonumberSlot *slot)
{
  g_return_val_if_fail (slot != NULL, NULL);

  return slot->symbolname;
}

/*! \brief Set the symbol name for a slot object.
 *
 * \par Function Description
 * Sets the symbol name for the \p slot object.
 *
 * \param [in] slot The #SchematicAutonumberSlot instance.
 * \param [in] name A new symbol name.
 */
void
schematic_autonumber_slot_set_symbol_name (SchematicAutonumberSlot *slot,
                                           char *name)
{
  g_return_if_fail (slot != NULL);

  slot->symbolname = name;
}


/*! \brief GCompareFunc function to access
 *  #SchematicAutonumberSlot object in a GList
 *  \par Function Description

 *  This function takes two #SchematicAutonumberSlot arguments and
 *  compares them.  Sorting criteria is the
 *  #SchematicAutonumberSlot members: first the symbolname, than
 *  the number and last the slotnr.  If the number or the slotnr
 *  is set to zero it acts as a wildcard.  The function is used as
 *  GCompareFunc by GList functions.
 */
gint freeslot_compare(gconstpointer a, gconstpointer b)
{
  SchematicAutonumberSlot *aa, *bb;
  gint res;
  aa = (SchematicAutonumberSlot *) a;
  bb = (SchematicAutonumberSlot *) b;

  if ((res = strcmp(aa->symbolname, bb->symbolname)) != 0)
    return res;

  /* aa->symbolname == bb->symbolname */
  if (aa->number == 0 || bb->number == 0)
    return 0;
  if (aa->number > bb->number)
    return 1;
  if (aa->number < bb->number)
    return -1;

  /* aa->number == bb->number */
  if (aa->slotnr == 0 || bb->slotnr == 0)
    return 0;
  if (aa->slotnr > bb->slotnr)
    return 1;
  if (aa->slotnr < bb->slotnr)
    return -1;

  return 0;
}

/*! \brief Prints a GList of #SchematicAutonumberSlot elements.
 *  \par Function Description
 *  This function prints the elements of a GList that contains
 *  #SchematicAutonumberSlot elements.  It is only used for
 *  debugging purposes.
 */
void freeslot_print(GList *list) {
  GList *item;
  SchematicAutonumberSlot *fs;

  printf("freeslot_print(): symname, number, slot\n");
  for (item = list; item != NULL; item = g_list_next(item)) {
    fs = (SchematicAutonumberSlot*) item ->data;
    printf("  %s, %d, %d\n",fs->symbolname, fs->number, fs->slotnr);
  }
}


/*! \brief Function to clear the databases of used parts
 *  \par Function Descriptions
 *  Just remove the list of used numbers, used slots and free slots.
 */
void autonumber_clear_database (SchematicAutonumber *autotext)
{
  /* cleanup everything for the next searchtext */
  if (autotext->used_numbers != NULL) {
    g_list_free(autotext->used_numbers);
    autotext->used_numbers = NULL;
  }
  if (autotext->free_slots != NULL) {
    g_list_foreach(autotext->free_slots, (GFunc) g_free, NULL);
    g_list_free(autotext->free_slots);
    autotext->free_slots = NULL;
  }
  if (autotext->used_slots != NULL) {
    g_list_foreach(autotext->used_slots, (GFunc) g_free, NULL);
    g_list_free(autotext->used_slots);
    autotext->used_slots = NULL;
  }
}

/*! \brief Function to test, whether the LeptonObject matches the autotext criterias
 *  \par Function Description
 *  The criterias are those of the autonumber text dialog. The function decides
 *  whether the <B>LeptonObject</B> has to be renumberd, ignored or taken care of when
 *  renumbering all other objects.
 *  \return one of these integer values: <B>AUTONUMBER_IGNORE</B>,
 *  <B>AUTONUMBER_RESPECT</B> or <B>AUTONUMBER_RENUMBER</B> and the current number
 *  of the text object in <B>*number</B>.
 */
gint
autonumber_match (SchematicAutonumber *autotext,
                  LeptonObject *o_current,
                  gint *number)
{
  gint i, isnumbered=1;
  size_t len;
  const gchar *str = NULL;

  len = strlen(autotext->current_searchtext);
  /* first find out whether we can ignore that object */
  if (!lepton_object_is_text (o_current))  /* text object */
    return AUTONUMBER_IGNORE;

  str = lepton_text_object_get_string (o_current);

  if (!(strlen(str) - len > 0)
      || !g_str_has_prefix(str, autotext->current_searchtext))
    return AUTONUMBER_IGNORE;

  /* the string object matches with its leading characters to the searchtext */
  /* now look for the extension, either a number or the "?" */
  if (g_str_has_suffix (str,"?")) {
    isnumbered = 0;
    /* There must not be any character between the "?" and the searchtext */
    if (strlen(str) != len+1)
      return AUTONUMBER_IGNORE;
  }
  else {
    if (!isdigit( (int) (str[len]) )) /* has at least one digit */
      return AUTONUMBER_IGNORE;

    for (i=len+1; str[i]; i++) /* and only digits */
      if (!isdigit( (int) (str[i]) ))
        return AUTONUMBER_IGNORE;
  }

  /* we have six cases, 3 from focus multiplied by 2 selection cases */
  if ((autotext->root_page || autotext->scope_number == SCOPE_HIERARCHY)
      && (lepton_object_get_selected (o_current)
          || autotext->scope_number == SCOPE_HIERARCHY || autotext->scope_number == SCOPE_PAGE)
      && (!isnumbered || (autotext->scope_overwrite)))
    return AUTONUMBER_RENUMBER;

  if (isnumbered
      && !(autotext->scope_skip == SCOPE_SELECTED
           && !(lepton_object_get_selected (o_current))
           && autotext->root_page))
  {
    sscanf(&(str[len])," %d", number);
    return AUTONUMBER_RESPECT; /* numbered objects which we don't renumber */
  }
  else
    return AUTONUMBER_IGNORE;  /* unnumbered objects outside the focus */
}


/*! \brief Creates a list of already numbered objects and slots
 *  \par Function Description
 *  This function collects the used numbers of a single schematic
 *  page.  The used element numbers are stored in a GList
 *  container inside the #SchematicAutonumber struct.  The
 *  slotting container is a little bit different. It stores free
 *  slots of multislotted symbols, that were used only partially.
 *  The criterias are derivated from the autonumber dialog
 *  entries.
 */
void
autonumber_get_used (SchematicWindow *w_current,
                     SchematicAutonumber *autotext)
{
  gint number, numslots, slotnr, i;
  LeptonObject *o_current, *o_parent;
  SchematicAutonumberSlot *slot;
  GList *slot_item;
  char *numslot_str, *slot_str;
  const GList *iter;

  LeptonPage *active_page = schematic_window_get_active_page (w_current);

  for (iter = lepton_page_objects (active_page);
       iter != NULL;
       iter = g_list_next (iter)) {
    o_current = (LeptonObject*) iter->data;
    if (autonumber_match(autotext, o_current, &number) == AUTONUMBER_RESPECT) {
      /* check slot and maybe add it to the lists */
      o_parent = lepton_object_get_attached_to (o_current);
      if (autotext->slotting && o_parent != NULL) {
        /* check for slotted symbol */
        numslot_str =
          lepton_attrib_search_object_attribs_by_name (o_parent, "numslots", 0);
        if (numslot_str != NULL) {
          sscanf(numslot_str," %d",&numslots);
          g_free(numslot_str);

          if (numslots > 0) {
            slot_str = lepton_attrib_search_object_attribs_by_name (o_parent, "slot", 0);
            if (slot_str == NULL) {
              g_message (_("slotted object without slot attribute may cause "
                           "problems when autonumbering slots"));
            }
            else {
              sscanf(slot_str, " %d", &slotnr);
              slot = g_new (SchematicAutonumberSlot, 1);
              slot->number = number;
              slot->slotnr = slotnr;
              slot->symbolname = lepton_component_object_get_basename (o_parent);


              slot_item = g_list_find_custom(autotext->used_slots,
                                             slot,
                                             (GCompareFunc) freeslot_compare);
              if (slot_item != NULL) { /* duplicate slot in used_slots */
                g_message (_("duplicate slot may cause problems: "
                             "[symbolname=%1$s, number=%2$d, slot=%3$d]"),
                           slot->symbolname, slot->number, slot->slotnr);
                g_free(slot);
              }
              else {
                autotext->used_slots = g_list_insert_sorted(autotext->used_slots,
                                                            slot,
                                                            (GCompareFunc) freeslot_compare);

                slot_item = g_list_find_custom(autotext->free_slots,
                                               slot,
                                               (GCompareFunc) freeslot_compare);
                if (slot_item == NULL) {
                  /* insert all slots to the list, except of the current one */
                  for (i=1; i <= numslots; i++) {
                    if (i != slotnr) {
                      slot = (SchematicAutonumberSlot*) g_memdup2 (slot, sizeof (SchematicAutonumberSlot));
                      slot->slotnr = i;
                      autotext->free_slots = g_list_insert_sorted(autotext->free_slots,
                                                                  slot,
                                                                  (GCompareFunc) freeslot_compare);
                    }
                  }
                }
                else {
                  g_free(slot_item->data);
                  autotext->free_slots = g_list_delete_link(autotext->free_slots, slot_item);
                }
              }
            }
          }
        }
      }
      /* put number into the used list */
      autotext->used_numbers = g_list_insert_sorted(autotext->used_numbers,
                                                    GINT_TO_POINTER(number),
                                                    (GCompareFunc) autonumber_sort_numbers);
    }
  }
}


/*! \brief Gets or generates free numbers for the autonumbering process.
 *  \par Function Description
 *  This function gets or generates new numbers for the
 *  <B>LeptonObject o_current</B>.  It uses the element numbers
 *  <B>used_numbers</B> and the list of the free slots
 *  <B>free_slots</B> of the #SchematicAutonumber struct.
 *
 *  The new number is returned into the <B>number</B> parameter.
 *  <B>slot</B> is set if autoslotting is active, else it is set to zero.
 *  \param [in,out] autotext The #SchematicAutonumber instance.
 *  \param [in] o_current The \c LeptonObject instance to process.
 *  \param [in,out] number The new number.
 *  \param [in,out] slot The new slot number.
 */
void
autonumber_get_new_numbers (SchematicAutonumber *autotext,
                            LeptonObject *o_current,
                            gint *number,
                            gint *slot)
{
  GList *item;
  gint new_number, numslots, i;
  SchematicAutonumberSlot *freeslot;
  LeptonObject *o_parent = NULL;
  GList *freeslot_item;
  gchar *numslot_str;

  new_number = autotext->startnum;

  /* Check for slots first */
  /* 1. are there any unused slots in the database? */
  o_parent = lepton_object_get_attached_to (o_current);
  if (autotext->slotting && o_parent != NULL) {
    freeslot = g_new (SchematicAutonumberSlot, 1);
    freeslot->symbolname = lepton_component_object_get_basename (o_parent);
    freeslot->number = 0;
    freeslot->slotnr = 0;
    freeslot_item = g_list_find_custom(autotext->free_slots,
                                       freeslot,
                                       (GCompareFunc) freeslot_compare);
    g_free(freeslot);
    /* Yes! -> remove from database, apply it */
    if (freeslot_item != NULL) {
      freeslot = (SchematicAutonumberSlot*) freeslot_item->data;
      *number = freeslot->number;
      *slot = freeslot->slotnr;
      g_free(freeslot);
      autotext->free_slots = g_list_delete_link(autotext->free_slots, freeslot_item);

      return;
    }
  }

  /* get a new number */
  item = autotext->used_numbers;
  while (1) {
    while (item != NULL && GPOINTER_TO_INT(item->data) < new_number)
      item = g_list_next(item);

    if (item == NULL || GPOINTER_TO_INT(item->data) > new_number)
      break;
    else  /* new_number == item->data */
      new_number++;
  }
  *number = new_number;
  *slot = 0;

  /* insert the new number to the used list */
  autotext->used_numbers = g_list_insert_sorted(autotext->used_numbers,
                                                GINT_TO_POINTER(new_number),
                                                (GCompareFunc) autonumber_sort_numbers);

  /* 3. is o_current a slotted object ? */
  if ((autotext->slotting) && o_parent != NULL) {
    numslot_str =
      lepton_attrib_search_object_attribs_by_name (o_parent, "numslots", 0);
    if (numslot_str != NULL) {
      sscanf(numslot_str," %d",&numslots);
      g_free(numslot_str);
      if (numslots > 0) {
        /* Yes! -> new number and slot=1; add the other slots to the database */
        *slot = 1;
        for (i=2; i <=numslots; i++) {
          freeslot = g_new (SchematicAutonumberSlot, 1);
          freeslot->symbolname = lepton_component_object_get_basename (o_parent);
          freeslot->number = new_number;
          freeslot->slotnr = i;
          autotext->free_slots = g_list_insert_sorted(autotext->free_slots,
                                                      freeslot,
                                                      (GCompareFunc) freeslot_compare);
        }
      }
    }
  }
}

/** @brief Removes the number from the element.
 *
 *  This function updates the text content of the \a o_current object.
 *
 *  @param autotext Pointer to the state structure
 *  @param o_current Pointer to the object from which to remove the number
 *
 */
void
autonumber_remove_number (SchematicAutonumber *autotext,
                          LeptonObject *o_current)
{
  LeptonObject *o_parent, *o_slot;
  gchar *slot_str;
  gchar *str = NULL;

  /* replace old text */
  str = g_strdup_printf("%s?", autotext->current_searchtext);
  lepton_text_object_set_string (o_current, str);
  g_free (str);

  /* remove the slot attribute if slotting is active */
  if (autotext->slotting) {
    /* get the slot attribute */
    o_parent = lepton_object_get_attached_to (o_current);
    if (o_parent != NULL) {
      slot_str = lepton_slot_search (o_parent, &o_slot);
      g_free (slot_str);
      /* Only attempt to remove non-inherited slot attributes */
      if (o_slot != NULL && !lepton_attrib_is_inherited (o_slot))
      {
        /* delete the slot attribute */
        schematic_delete (autotext->w_current, o_slot);
      }
    }
  }

  schematic_window_active_page_changed (autotext->w_current);
}

/*! \brief Changes the number <B>LeptonObject</B> element. Changes the slot attribute.
 *  \par Function Description
 *  This function updates the text content of the <B>o_current</B> object.
 *  If the <B>slot</B> value is not zero. It updates the slot attribut of the
 *  component element that is also the parent object of the o_current element.
 */
void
autonumber_apply_new_text (SchematicAutonumber *autotext,
                           LeptonObject *o_current,
                           gint number,
                           gint slot)
{
  char *str;

  if (slot != 0) {
    /* update the slot on the owning object */
    str = g_strdup_printf ("slot=%d", slot);
    o_slot_end (autotext->w_current,
                lepton_object_get_attached_to (o_current),
                str);
    g_free (str);
  }

  /* replace old text */
  str = g_strdup_printf("%s%d", autotext->current_searchtext, number);
  lepton_text_object_set_string (o_current, str);
  g_free (str);

  schematic_window_active_page_changed (autotext->w_current);
}


/*! \brief Handles all the options of the autonumber text dialog
 *  \par Function Description
 *  This function is the master of all autonumber code. It
 *  receives the options of the the autonumber text dialog in an
 *  #SchematicAutonumber structure.  First it collects all pages
 *  of a hierarchical schematic.  Second it gets all matching text
 *  elements for the searchtext.  Then it renumbers all text
 *  elements of all schematic pages. The renumbering follows the
 *  rules of the parameters given in the autonumber text dialog.
 *
 *  \param [in] autotext The #SchematicAutonumber instance.
 */
void
schematic_autonumber_run (SchematicAutonumber *autotext)
{
  GList *pages;
  GList *searchtext_list=NULL;
  GList *text_item, *obj_item, *page_item;
  LeptonObject *o_current;
  SchematicWindow *w_current;
  gchar *searchtext;
  gchar *scope_text;
  gchar *new_searchtext;
  gint number, slot;
  size_t i;
  GList *o_list = NULL;
  const GList *iter;
  LeptonPage *active_page = NULL;
  LeptonToplevel *toplevel = NULL;

  w_current = autotext->w_current;
  toplevel = schematic_window_get_toplevel (w_current);
  active_page = schematic_window_get_active_page (w_current);
  autotext->current_searchtext = NULL;
  autotext->root_page = 1;
  autotext->used_numbers = NULL;
  autotext->free_slots = NULL;
  autotext->used_slots = NULL;

  scope_text = (gchar*) g_list_first(autotext->scope_text)->data;

  /* Step1: get all pages of the hierarchy */
  pages = s_hierarchy_traversepages (w_current,
                                     active_page,
                                     HIERARCHY_NODUPS);

  /*  g_list_foreach(pages, (GFunc) s_hierarchy_print_page, NULL); */

  /* Step2: if searchtext has an asterisk at the end we have to find
     all matching searchtextes.

     Example:  "refdes=*" will match each text that starts with "refdes="
     and has a trailing "?" or a trailing number if the "all"-option is set.
     We get a list of possible prefixes: refdes=R, refdes=C.

     If there is only one search pattern, it becomes a single item
     in the searchtext list */

  if (strlen(scope_text) == 0) {
    g_message (_("No search string given in autonumber text."));
    return; /* error */
  }
  else if (g_str_has_suffix(scope_text,"?") == TRUE) {
    /* single searchtext, strip of the "?" */
    searchtext = g_strndup(scope_text, strlen(scope_text)-1);
    searchtext_list=g_list_append (searchtext_list, searchtext);
  }
  else if (g_str_has_suffix(scope_text,"*") == TRUE) {
    /* strip of the "*" */
    searchtext = g_strndup(scope_text, strlen(scope_text)-1);

    /* collect all the possible searchtexts in all pages of the hierarchy */
    for (page_item = pages; page_item != NULL; page_item = g_list_next(page_item)) {
      lepton_toplevel_goto_page (toplevel, (LeptonPage*) page_item->data);
      schematic_window_page_changed (w_current);
      /* iterate over all objects an look for matching searchtext's */
      for (iter = lepton_page_objects (active_page);
           iter != NULL;
           iter = g_list_next (iter)) {
        o_current = (LeptonObject*) iter->data;
        if (lepton_object_is_text (o_current))
        {
          if (autotext->scope_number == SCOPE_HIERARCHY
              || autotext->scope_number == SCOPE_PAGE
              || ((autotext->scope_number == SCOPE_SELECTED)
                  && (lepton_object_get_selected (o_current))))
          {
            const gchar *str = lepton_text_object_get_string (o_current);
            if (g_str_has_prefix (str, searchtext)) {
              /* the beginnig of the current text matches with the searchtext now */
              /* strip of the trailing [0-9?] chars and add it too the searchtext */
              for (i = strlen (str)-1;
                   (i >= strlen(searchtext))
                     && (str[i] == '?'
                         || isdigit( (int) (str[i]) ));
                   i--)
                ; /* void */

              new_searchtext = g_strndup (str, i+1);
              if (g_list_find_custom(searchtext_list, new_searchtext,
                                     (GCompareFunc) strcmp) == NULL ) {
                searchtext_list = g_list_append(searchtext_list, new_searchtext);
              }
              else {
                g_free(new_searchtext);
              }
            }
          }
        }
      }
      if (autotext->scope_number == SCOPE_SELECTED || autotext->scope_number == SCOPE_PAGE)
        break; /* search only in the first page */
    }
    g_free(searchtext);
  }
  else {
    g_message (_("No '*' or '?' given at the end of the autonumber text."));
    return;
  }

  /* Step3: iterate over the search items in the list */
  for (text_item=searchtext_list; text_item !=NULL; text_item=g_list_next(text_item)) {
    autotext->current_searchtext = (gchar*) text_item->data;
    /* printf("schematic_autonumber_run: searchtext %s\n", autotext->current_searchtext); */
    /* decide whether to renumber page by page or get a global used-list */

    if (autotext->scope_skip == SCOPE_HIERARCHY) {  /* whole hierarchy database */
      /* renumbering all means that no db is required */
      if (!(autotext->scope_number == SCOPE_HIERARCHY
            && autotext->scope_overwrite)) {
        for (page_item = pages; page_item != NULL; page_item = g_list_next(page_item)) {
          autotext->root_page = (pages->data == page_item->data);
          lepton_toplevel_goto_page (toplevel, (LeptonPage*) page_item->data);
          schematic_window_page_changed (w_current);
          autonumber_get_used(w_current, autotext);
        }
      }
    }

    /* renumber the elements */
    for (page_item = pages; page_item != NULL; page_item = g_list_next(page_item)) {
      lepton_toplevel_goto_page (toplevel, (LeptonPage*) page_item->data);
      schematic_window_page_changed (w_current);
      autotext->root_page = (pages->data == page_item->data);
      /* build a page database if we're numbering pagebypage or selection only*/
      if (autotext->scope_skip == SCOPE_PAGE || autotext->scope_skip == SCOPE_SELECTED) {
        autonumber_get_used(w_current, autotext);
      }

      /* RENUMBER CODE FOR ONE PAGE AND ONE SEARCHTEXT*/
      /* 1. get objects to renumber */
      for (iter = lepton_page_objects (active_page);
           iter != NULL;
           iter = g_list_next (iter)) {
        o_current = (LeptonObject*) iter->data;
        if (autonumber_match(autotext, o_current, &number) == AUTONUMBER_RENUMBER) {
          /* put number into the used list */
          o_list = g_list_append(o_list, o_current);
        }
      }

      /* 2. sort object list */
      switch (autotext->order) {
      case AUTONUMBER_SORT_YX:
        o_list=g_list_sort(o_list, autonumber_sort_yx);
        break;
      case AUTONUMBER_SORT_YX_REV:
        o_list=g_list_sort(o_list, autonumber_sort_yx_rev);
        break;
      case AUTONUMBER_SORT_XY:
        o_list=g_list_sort(o_list, autonumber_sort_xy);
        break;
      case AUTONUMBER_SORT_XY_REV:
        o_list=g_list_sort(o_list, autonumber_sort_xy_rev);
        break;
      case AUTONUMBER_SORT_DIAGONAL:
        o_list=g_list_sort(o_list, autonumber_sort_diagonal);
        break;
      default:
        ; /* unsorted file order */
      }

      /* 3. renumber/reslot the objects */
      for(obj_item=o_list; obj_item != NULL; obj_item=g_list_next(obj_item)) {
        o_current = (LeptonObject*) obj_item->data;
        if(autotext->removenum) {
          autonumber_remove_number(autotext, o_current);
        } else {
          /* get valid numbers from the database */
          autonumber_get_new_numbers(autotext, o_current, &number, &slot);
          /* and apply it. TODO: join these two functions */
          autonumber_apply_new_text(autotext, o_current, number, slot);
        }
      }
      g_list_free(o_list);
      o_list = NULL;

      /* destroy the page database */
      if (autotext->scope_skip == SCOPE_PAGE
          || autotext->scope_skip == SCOPE_SELECTED)
        autonumber_clear_database(autotext);

      if (autotext->scope_number == SCOPE_SELECTED
          || autotext->scope_number == SCOPE_PAGE)
        break; /* only renumber the parent page (the first page) */
    }
    autonumber_clear_database(autotext);   /* cleanup */
  }

  /* cleanup and redraw all*/
  g_list_foreach(searchtext_list, (GFunc) g_free, NULL);
  g_list_free(searchtext_list);

  /* Go back to the root page. */
  lepton_toplevel_goto_page (toplevel, (LeptonPage*) pages->data);
  schematic_window_page_changed (w_current);
  schematic_canvas_invalidate_all (schematic_window_get_current_canvas (w_current));
  g_list_free(pages);
  o_undo_savestate_old (w_current);
}

/* ***** UTILITY GUI FUNCTIONS (move to a separate file in the future?) **** */

/** @brief Finds a widget by its name given a pointer to its parent.
 *
 * @param widget Pointer to the parent widget.
 * @param widget_name Name of the widget.
 * @return Pointer to the widget or NULL if not found. */
GtkWidget*
schematic_autonumber_dialog_lookup_widget (GtkWidget *widget,
                                           const gchar *widget_name)
{
  GtkWidget *found_widget;

  found_widget = (GtkWidget*) g_object_get_data(G_OBJECT(widget),
                                                widget_name);

  return found_widget;
}

/*! \brief Put the icons and the text into the sort order
 *  combobox.
 *
 *  \par Function Description
 *  Load all bitmaps for the combobox and store them together with
 *  the label in a GtkListStore.
 *
 *  \param [in,out] sort_order The sort order combobox widget.
 */
void
schematic_autonumber_sort_order_widget_init (GtkWidget *sort_order)
{
  GtkListStore *store;
  GtkTreeIter iter;
  GtkCellRenderer *renderer;
  GdkPixbuf *pixbuf;
  gchar *path;
  GError *error=NULL;

  const gchar *filenames[] =
    {
      "gschem-diagonal.png",
      "gschem-top2bottom.png",
      "gschem-bottom2top.png",
      "gschem-left2right.png",
      "gschem-right2left.png",
      "gschem-fileorder.png",
      NULL
    };
  const gchar *names[] =
    {
      N_("Diagonal"),
      N_("Top to bottom"),
      N_("Bottom to top"),
      N_("Left to right"),
      N_("Right to left"),
      N_("File order"),
      NULL
    };
  gint i;

  store = gtk_list_store_new(2, G_TYPE_STRING, GDK_TYPE_PIXBUF);

  for (i=0; filenames[i] != NULL; i++) {
    path=g_build_filename(BITMAP_DIRECTORY,
                          filenames[i], NULL);
    pixbuf = gdk_pixbuf_new_from_file(path, &error);
    g_free(path);
    gtk_list_store_append(store, &iter);
    gtk_list_store_set(store, &iter,
                       0, _(names[i]),
                       1, pixbuf ? pixbuf : NULL,
                       -1);
    g_clear_error (&error);
  }

  gtk_combo_box_set_model(GTK_COMBO_BOX(sort_order), GTK_TREE_MODEL(store));
  renderer = gtk_cell_renderer_text_new ();

  gtk_cell_layout_pack_start (GTK_CELL_LAYOUT (sort_order),
                              renderer, TRUE);
  gtk_cell_layout_set_attributes (GTK_CELL_LAYOUT (sort_order),
                                  renderer, "text", 0, NULL);
  renderer = gtk_cell_renderer_pixbuf_new();
  g_object_set(G_OBJECT(renderer), "xpad", 5, "ypad", 5, NULL);

  gtk_cell_layout_pack_start (GTK_CELL_LAYOUT (sort_order),
                              renderer, FALSE);
  gtk_cell_layout_set_attributes (GTK_CELL_LAYOUT (sort_order),
                                  renderer, "pixbuf", 1, NULL);
}

/* ***** STATE STRUCT HANDLING (interface between GUI and backend code) **** */

/** @brief Adds a line to the search text history list
 *
 * Function makes sure that: 1) There are no duplicates in the list and 2) the
 * last search text is always at the top of the list.
 */
GList *autonumber_history_add(GList *history, gchar *text)
{
  /* Search for this text in history and delete it (so we don't have
   * duplicate entries) */

  GList *cur;

  cur=history;
  while(cur!=NULL) {
    if (!strcmp (text, (const char*) cur->data)) {
      history=g_list_remove_link(history, cur);

      g_free(cur->data);
      g_list_free(cur);
      break;
    }
    cur=g_list_next(cur);
  }

  /* Add the new text at the beginning of the list */

  history=g_list_prepend(history, text);

  /* Truncate history */
  while(g_list_length(history) > HISTORY_LENGTH) {
    GList *last = g_list_last(history);

    history = g_list_remove_link(history, last);

    g_free(last->data);
    g_list_free(last);
  }

  return history;
}

/** @brief Create and initialize a new #SchematicAutonumber
 *  instance.
 *
 *  @par Function Description
 *  Creates a new #SchematicAutonumber instance and initializes it
 *  with default values.
 *
 *  @return The new #SchematicAutonumber instance.
 */
SchematicAutonumber*
schematic_autonumber_new ()
{
  SchematicAutonumber *autotext;

  /* Default contents of the combo box history */
  const gchar *default_text[] = {
    "refdes=*",
    "refdes=C?",
    "refdes=D?",
    "refdes=I?",
    "refdes=L?",
    "refdes=Q?",
    "refdes=R?",
    "refdes=T?",
    "refdes=U?",
    "refdes=X?",
    "netname=*",
    "netname=A?",
    "netname=D?",
    NULL
  };
  const gchar **t;

  autotext = g_new (SchematicAutonumber, 1);

  if(autotext==NULL) return NULL;

  autotext->scope_text = NULL;
  t = default_text;
  while (*t != NULL) {
    /* t is both an array address and the address of its first
       element, it post-increments after we get its value. */
    autotext->scope_text=g_list_append (autotext->scope_text, g_strdup (*t++));
  }

  autotext->scope_skip = SCOPE_PAGE;
  autotext->scope_number = SCOPE_SELECTED;

  autotext->scope_overwrite = FALSE;
  autotext->order = AUTONUMBER_SORT_DIAGONAL;

  autotext->startnum=1;

  autotext->removenum = FALSE;
  autotext->slotting = FALSE;

  autotext->dialog = NULL;

  return autotext;
}

/** @brief Restore the Autonumber text dialog settings.
 *
 *  @par Function Description
 *  Restores settings of the Autonumber text dialog saved
 *  previously.
 *
 * @sa schematic_autonumber_dialog_save_state()
 *
 * @param autotext Pointer to the state struct.
 */
void
schematic_autonumber_dialog_restore_state (SchematicAutonumber *autotext)
{
  GtkWidget *widget;
  GtkTreeModel *model;
  GList *el;
  /* Scope */

  /* Search text history */
  widget = schematic_autonumber_dialog_lookup_widget (autotext->dialog,
                                                      "scope_text");

  /* Simple way to clear the ComboBox. Owen from #gtk+ says:
   *
   * Yeah, it's just slightly "shady" ... if you want to stick to fully
   * advertised API, you need to remember how many rows you added and
   * use gtk_combo_box_remove_text() */

  model = gtk_combo_box_get_model(GTK_COMBO_BOX(widget));
  gtk_list_store_clear(GTK_LIST_STORE(model));

  for (el= autotext->scope_text; el != NULL; el=g_list_next(el)) {
    gtk_combo_box_text_append_text (GTK_COMBO_BOX_TEXT (widget),
                                    (const gchar*) el->data);
  }

  widget = gtk_bin_get_child(GTK_BIN(widget));
  gtk_entry_set_text(GTK_ENTRY(widget), (const gchar*) g_list_first(autotext->scope_text)->data);

  widget = schematic_autonumber_dialog_lookup_widget (autotext->dialog,
                                                      "scope_skip");
  gtk_combo_box_set_active(GTK_COMBO_BOX(widget),
                           autotext->scope_skip);

  widget = schematic_autonumber_dialog_lookup_widget (autotext->dialog,
                                                      "scope_number");
  gtk_combo_box_set_active(GTK_COMBO_BOX(widget),
                           autotext->scope_number);

  widget = schematic_autonumber_dialog_lookup_widget (autotext->dialog,
                                                      "scope_overwrite");
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(widget),
                               autotext->scope_overwrite);

  /* Options */
  widget = schematic_autonumber_dialog_lookup_widget (autotext->dialog,
                                                      "opt_startnum");
  gtk_spin_button_set_value(GTK_SPIN_BUTTON(widget),
                            autotext->startnum);

  widget = schematic_autonumber_dialog_lookup_widget (autotext->dialog,
                                                      "sort_order");
  gtk_combo_box_set_active(GTK_COMBO_BOX(widget), autotext->order);

  widget = schematic_autonumber_dialog_lookup_widget (autotext->dialog,
                                                      "opt_removenum");
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(widget),
                               autotext->removenum);

  widget = schematic_autonumber_dialog_lookup_widget (autotext->dialog,
                                                      "opt_slotting");
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(widget),
                               autotext->slotting);
}

/** @brief Get the settings from the autonumber text dialog
 *
 * Get the settings from the autonumber text dialog and store it
 * in the #SchematicAutonumber structure.
 *
 * @sa schematic_autonumber_dialog_restore_state()
 *
 * @param autotext Pointer to the state struct.
 */
void
schematic_autonumber_dialog_save_state (SchematicAutonumber *autotext)
{
  GtkWidget *widget;
  gchar *text;

  /* Scope */

  /* Search text history */
  widget = schematic_autonumber_dialog_lookup_widget (autotext->dialog,
                                                      "scope_text");
  widget = gtk_bin_get_child(GTK_BIN(widget));
  text = g_strdup(gtk_entry_get_text( GTK_ENTRY(widget)));

  autotext->scope_text=autonumber_history_add(autotext->scope_text, text);

  widget = schematic_autonumber_dialog_lookup_widget (autotext->dialog,
                                                      "scope_skip");
  autotext->scope_skip = gtk_combo_box_get_active( GTK_COMBO_BOX(widget) );

  widget = schematic_autonumber_dialog_lookup_widget (autotext->dialog,
                                                      "scope_number");
  autotext->scope_number = gtk_combo_box_get_active(GTK_COMBO_BOX(widget) );

  widget = schematic_autonumber_dialog_lookup_widget (autotext->dialog,
                                                      "scope_overwrite");
  autotext->scope_overwrite = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget));

  /* Sort order */
  widget = schematic_autonumber_dialog_lookup_widget (autotext->dialog,
                                                      "sort_order");
  autotext->order= gtk_combo_box_get_active(GTK_COMBO_BOX(widget));

  /* Options */
  widget = schematic_autonumber_dialog_lookup_widget (autotext->dialog,
                                                      "opt_startnum");
  autotext->startnum=gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(widget));

  widget = schematic_autonumber_dialog_lookup_widget (autotext->dialog,
                                                      "opt_removenum");
  autotext->removenum = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget));

  widget = schematic_autonumber_dialog_lookup_widget (autotext->dialog,
                                                      "opt_slotting");
  autotext->slotting = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget));
}


/* ***** DIALOG SET-UP ***************************************************** */

/** @brief Creates the autonumber text dialog.
 *
 * Dialog is not shown. No callbacks are registered. This is basically
 * unmodified code returned by Glade.
 *
 * Only modification was the following substitution:
 *
 * %s/create_pixmap (autonumber_text, "\(.*\)")/autonumber_create_pixmap("gschem-\1", w_current)/
 *
 * and addition of the "w_current" parameter.
 *
 * @param w_current Pointer to the top level struct.
 * @return Pointer to the dialog window.
 */
GtkWidget*
schematic_autonumber_dialog_new (SchematicWindow *w_current)
{
  GtkWidget *autonumber_text;
  GtkWidget *vbox1;
  GtkWidget *vbox3;
  GtkWidget *label4;
  GtkWidget *scope_text;
  GtkWidget *label8;
  GtkWidget *label6;
  GtkWidget *scope_number;
  GtkWidget *scope_skip;
  GtkWidget *scope_overwrite;
  GtkWidget *label1;
  GtkWidget *vbox4;
  GtkWidget *label12;
  GtkWidget *label13;
  GtkWidget *opt_startnum;
  GtkWidget *sort_order;
  GtkWidget *opt_removenum;
  GtkWidget *opt_slotting;
  GtkWidget *label3;


  GtkWidget *main_window =
    schematic_window_get_main_window (w_current);

  autonumber_text =
    schematic_dialog_new_with_buttons (_("Autonumber Text"),
                                       GTK_WINDOW (main_window),
                                       (GtkDialogFlags) 0, /* not modal */
                                       "autonumber", w_current,
                                       _("_Close"), GTK_RESPONSE_REJECT,
                                       _("_Apply"), GTK_RESPONSE_ACCEPT,
                                       NULL);
#ifndef ENABLE_GTK3
  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(autonumber_text),
                                          GTK_RESPONSE_ACCEPT,
                                          GTK_RESPONSE_REJECT,
                                          -1);
#endif

  gtk_window_set_position (GTK_WINDOW (autonumber_text), GTK_WIN_POS_MOUSE);

  gtk_container_set_border_width (GTK_CONTAINER (autonumber_text),
                                  DIALOG_BORDER_SPACING);
  vbox1 = gtk_dialog_get_content_area (GTK_DIALOG (autonumber_text));
  gtk_box_set_spacing(GTK_BOX(vbox1), DIALOG_V_SPACING);

  /* scope section */
  label1 = gtk_label_new (_("<b>Scope</b>"));
  gtk_label_set_use_markup (GTK_LABEL (label1), TRUE);
#ifdef ENABLE_GTK3
  gtk_label_set_xalign (GTK_LABEL (label1), 0.0);
  gtk_label_set_yalign (GTK_LABEL (label1), 0.0);
#else
  gtk_misc_set_alignment (GTK_MISC(label1), 0, 0);
#endif
  gtk_box_pack_start (GTK_BOX(vbox1), label1, TRUE, TRUE, 0);
  gtk_widget_show (label1);

#ifdef ENABLE_GTK3
  vbox3 = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
#else
  GtkWidget *alignment1 = gtk_alignment_new (0, 0, 1, 1);
  gtk_widget_show (alignment1);
  gtk_box_pack_start (GTK_BOX (vbox1), alignment1, TRUE, TRUE, 0);
  gtk_alignment_set_padding (GTK_ALIGNMENT (alignment1),
                             0, 0, DIALOG_INDENTATION, 0);

  vbox3 = gtk_vbox_new (FALSE, 0);
#endif
  gtk_widget_show (vbox3);

#ifdef ENABLE_GTK3
  gtk_box_pack_start (GTK_BOX (vbox1), vbox3, TRUE, TRUE, 0);

  GtkWidget* grid1 = gtk_grid_new ();
  gtk_widget_show (grid1);
  gtk_box_pack_start (GTK_BOX (vbox3), grid1, TRUE, TRUE, 0);
  gtk_grid_set_row_spacing (GTK_GRID (grid1), DIALOG_V_SPACING);
  gtk_grid_set_column_spacing (GTK_GRID (grid1), DIALOG_H_SPACING);

#else /* GTK2 */

  gtk_container_add (GTK_CONTAINER (alignment1), vbox3);

  GtkWidget *table1 = gtk_table_new (3, 2, FALSE);
  gtk_widget_show (table1);
  gtk_box_pack_start (GTK_BOX (vbox3), table1, TRUE, TRUE, 0);
  gtk_table_set_row_spacings (GTK_TABLE (table1), DIALOG_V_SPACING);
  gtk_table_set_col_spacings (GTK_TABLE (table1), DIALOG_H_SPACING);
#endif

  label4 = gtk_label_new (_("Search for:"));
  gtk_widget_show (label4);
#ifdef ENABLE_GTK3
  gtk_grid_attach (GTK_GRID (grid1), label4, 0, 0, 1, 1);
  gtk_label_set_xalign (GTK_LABEL (label4), 0.0);
  gtk_label_set_yalign (GTK_LABEL (label4), 0.5);
#else
  gtk_table_attach (GTK_TABLE (table1), label4, 0, 1, 0, 1,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);
  gtk_misc_set_alignment (GTK_MISC (label4), 0, 0.5);
#endif

  scope_text = gtk_combo_box_text_new_with_entry ();
  gtk_entry_set_activates_default(GTK_ENTRY(gtk_bin_get_child(GTK_BIN(scope_text))), TRUE);
  gtk_widget_show (scope_text);
#ifdef ENABLE_GTK3
  gtk_widget_set_hexpand (GTK_WIDGET (scope_text), TRUE);
  gtk_grid_attach (GTK_GRID (grid1), scope_text, 1, 0, 1, 1);
#else
  gtk_table_attach (GTK_TABLE (table1), scope_text, 1, 2, 0, 1,
                    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                    (GtkAttachOptions) (GTK_FILL), 0, 0);
#endif

  label8 = gtk_label_new (_("Autonumber text in:"));
  gtk_widget_show (label8);
#ifdef ENABLE_GTK3
  gtk_grid_attach (GTK_GRID (grid1), label8, 0, 1, 1, 1);
  gtk_label_set_xalign (GTK_LABEL (label8), 0.0);
  gtk_label_set_yalign (GTK_LABEL (label8), 0.5);
#else
  gtk_table_attach (GTK_TABLE (table1), label8, 0, 1, 1, 2,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);
  gtk_misc_set_alignment (GTK_MISC (label8), 0, 0.5);
#endif

  label6 = gtk_label_new (_("Skip numbers found in:"));
  gtk_widget_show (label6);
#ifdef ENABLE_GTK3
  gtk_grid_attach (GTK_GRID (grid1), label6, 0, 2, 1, 1);
  gtk_label_set_xalign (GTK_LABEL (label6), 0.0);
  gtk_label_set_yalign (GTK_LABEL (label6), 0.5);
#else
  gtk_table_attach (GTK_TABLE (table1), label6, 0, 1, 2, 3,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);
  gtk_misc_set_alignment (GTK_MISC (label6), 0, 0.5);
#endif

  scope_number = gtk_combo_box_text_new ();
  gtk_widget_show (scope_number);
#ifdef ENABLE_GTK3
  gtk_grid_attach (GTK_GRID (grid1), scope_number, 1, 1, 1, 1);
#else
  gtk_table_attach (GTK_TABLE (table1), scope_number, 1, 2, 1, 2,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (GTK_FILL), 0, 0);
#endif
  gtk_combo_box_text_append_text (GTK_COMBO_BOX_TEXT (scope_number), _("Selected objects"));
  gtk_combo_box_text_append_text (GTK_COMBO_BOX_TEXT (scope_number), _("Current page"));
  gtk_combo_box_text_append_text (GTK_COMBO_BOX_TEXT (scope_number), _("Whole hierarchy"));

  scope_skip = gtk_combo_box_text_new ();
  gtk_widget_show (scope_skip);
#ifdef ENABLE_GTK3
  gtk_grid_attach (GTK_GRID (grid1), scope_skip, 1, 2, 1, 1);
#else
  gtk_table_attach (GTK_TABLE (table1), scope_skip, 1, 2, 2, 3,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (GTK_FILL), 0, 0);
#endif
  gtk_combo_box_text_append_text (GTK_COMBO_BOX_TEXT (scope_skip), _("Selected objects"));
  gtk_combo_box_text_append_text (GTK_COMBO_BOX_TEXT (scope_skip), _("Current page"));
  gtk_combo_box_text_append_text (GTK_COMBO_BOX_TEXT (scope_skip), _("Whole hierarchy"));

  scope_overwrite = gtk_check_button_new_with_mnemonic (_("Overwrite existing numbers"));
  gtk_widget_show (scope_overwrite);
  gtk_box_pack_start (GTK_BOX (vbox3), scope_overwrite, FALSE, FALSE, 6);

  /* Options section */
  label3 = gtk_label_new (_("<b>Options</b>"));
  gtk_label_set_use_markup (GTK_LABEL (label3), TRUE);
#ifdef ENABLE_GTK3
  gtk_label_set_xalign (GTK_LABEL (label3), 0.0);
  gtk_label_set_yalign (GTK_LABEL (label3), 0.0);
#else
  gtk_misc_set_alignment(GTK_MISC(label3), 0, 0);
#endif
  gtk_widget_show (label3);
  gtk_box_pack_start(GTK_BOX(vbox1), label3, TRUE, TRUE, 0);

#ifdef ENABLE_GTK3
  vbox4 = gtk_box_new (GTK_ORIENTATION_VERTICAL, 3);
#else
  GtkWidget *alignment3 = gtk_alignment_new (0, 0, 1, 1);
  gtk_widget_show (alignment3);
  gtk_box_pack_start(GTK_BOX(vbox1), alignment3, TRUE, TRUE, 0);
  gtk_alignment_set_padding (GTK_ALIGNMENT (alignment3),
                             0, 0, DIALOG_INDENTATION, 0);

  vbox4 = gtk_vbox_new (FALSE, 3);
#endif
  gtk_widget_show (vbox4);

#ifdef ENABLE_GTK3
  gtk_box_pack_start(GTK_BOX(vbox1), vbox4, TRUE, TRUE, 0);

  GtkWidget *grid3 = gtk_grid_new ();
  gtk_widget_show (grid3);
  gtk_box_pack_start (GTK_BOX (vbox4), grid3, TRUE, TRUE, 0);
  gtk_grid_set_row_spacing (GTK_GRID (grid3), DIALOG_V_SPACING);
  gtk_grid_set_column_spacing (GTK_GRID (grid3), DIALOG_H_SPACING);

#else /* GTK2 */

  gtk_container_add (GTK_CONTAINER (alignment3), vbox4);

  GtkWidget *table3 = gtk_table_new (2, 2, FALSE);
  gtk_widget_show (table3);
  gtk_box_pack_start (GTK_BOX (vbox4), table3, TRUE, TRUE, 0);
  gtk_table_set_row_spacings (GTK_TABLE (table3), DIALOG_V_SPACING);
  gtk_table_set_col_spacings (GTK_TABLE (table3), DIALOG_H_SPACING);
#endif

  label12 = gtk_label_new (_("Starting number:"));
  gtk_widget_show (label12);
#ifdef ENABLE_GTK3
  gtk_grid_attach (GTK_GRID (grid3), label12, 0, 0, 1, 1);
  gtk_label_set_xalign (GTK_LABEL (label12), 0.0);
  gtk_label_set_yalign (GTK_LABEL (label12), 0.5);
#else
  gtk_table_attach (GTK_TABLE (table3), label12, 0, 1, 0, 1,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);
  gtk_misc_set_alignment (GTK_MISC (label12), 0, 0.5);
#endif

  label13 = gtk_label_new (_("Sort order:"));
  gtk_widget_show (label13);
#ifdef ENABLE_GTK3
  gtk_grid_attach (GTK_GRID (grid3), label13, 0, 1, 1, 1);
  gtk_label_set_xalign (GTK_LABEL (label13), 0.0);
  gtk_label_set_yalign (GTK_LABEL (label13), 0.5);
#else
  gtk_table_attach (GTK_TABLE (table3), label13, 0, 1, 1, 2,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);
  gtk_misc_set_alignment (GTK_MISC (label13), 0, 0.5);
#endif


  /* this sets the page size to 10 (step * 10).
   * if different page size is required,
   * use gtk_spin_button_set_increments()
  */
  opt_startnum = gtk_spin_button_new_with_range (0,     /* min */
                                                 10000, /* max */
                                                 1);    /* step */

  gtk_spin_button_set_digits (GTK_SPIN_BUTTON (opt_startnum), 0);
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (opt_startnum), 1);


  gtk_entry_set_activates_default(GTK_ENTRY(opt_startnum), TRUE);
  gtk_widget_show (opt_startnum);
#ifdef ENABLE_GTK3
  gtk_widget_set_hexpand (GTK_WIDGET (opt_startnum), TRUE);
  gtk_grid_attach (GTK_GRID (grid3), opt_startnum, 1, 0, 1, 1);
#else
  gtk_table_attach (GTK_TABLE (table3), opt_startnum, 1, 2, 0, 1,
                    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);
#endif

  sort_order = gtk_combo_box_new();
  gtk_widget_show (sort_order);
#ifdef ENABLE_GTK3
  gtk_grid_attach (GTK_GRID (grid3), sort_order, 1, 1, 1, 1);
#else
  gtk_table_attach (GTK_TABLE (table3), sort_order, 1, 2, 1, 2,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (GTK_FILL), 0, 0);
#endif

  opt_removenum = gtk_check_button_new_with_mnemonic (_("Remove numbers"));
  gtk_widget_show (opt_removenum);
  gtk_box_pack_start (GTK_BOX (vbox4), opt_removenum, FALSE, FALSE, 0);

  opt_slotting = gtk_check_button_new_with_mnemonic (_("Automatic slotting"));
  gtk_widget_show (opt_slotting);
  gtk_box_pack_start (GTK_BOX (vbox4), opt_slotting, FALSE, FALSE, 0);

  /* Store pointers to all widgets, for use by
     schematic_autonumber_dialog_lookup_widget (). */
  GLADE_HOOKUP_OBJECT (autonumber_text, scope_text, "scope_text");
  GLADE_HOOKUP_OBJECT (autonumber_text, scope_number, "scope_number");
  GLADE_HOOKUP_OBJECT (autonumber_text, scope_skip, "scope_skip");
  GLADE_HOOKUP_OBJECT (autonumber_text, scope_overwrite, "scope_overwrite");
  GLADE_HOOKUP_OBJECT (autonumber_text, opt_startnum, "opt_startnum");
  GLADE_HOOKUP_OBJECT (autonumber_text, sort_order, "sort_order");
  GLADE_HOOKUP_OBJECT (autonumber_text, opt_removenum, "opt_removenum");
  GLADE_HOOKUP_OBJECT (autonumber_text, opt_slotting, "opt_slotting");

  return autonumber_text;
}
