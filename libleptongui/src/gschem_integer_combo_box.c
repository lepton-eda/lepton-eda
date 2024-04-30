/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
 * Copyright (C) 2017-2024 Lepton EDA Contributors
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
/*! \file gschem_integer_combo_box.c
 *
 *  \brief A GtkComboBox with and entry for integer values.
 *
 *  This widget allows the user to type in an integer values or select a common
 *  integer value from a drop down menu.
 */
#include <config.h>
#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#include "gschem.h"



#if GTK_CHECK_VERSION (2, 24, 0)
G_DEFINE_TYPE (GschemIntegerComboBox,
               gschem_integer_combo_box,
               GTK_TYPE_COMBO_BOX);
#else
G_DEFINE_TYPE (GschemIntegerComboBox,
               gschem_integer_combo_box,
               GTK_TYPE_COMBO_BOX_ENTRY);
#endif


static void
add_widget (GschemIntegerComboBox *combo, GtkWidget *widget, gpointer unused);

static gboolean
focus_out_event (GtkEntry *entry, GdkEvent *event, GschemIntegerComboBox *combo);

static void
gschem_integer_combo_box_class_init (GschemIntegerComboBoxClass *klass);

static void
gschem_integer_combo_box_init (GschemIntegerComboBox *combo);

static void
notify_active (GschemIntegerComboBox *combo, GParamSpec *pspec, gpointer unused);

static void
remove_widget (GschemIntegerComboBox *combo, GtkWidget *widget, gpointer unused);

static void
value_changed (GschemIntegerComboBox *combo, gpointer unused);



/*! \brief The entry widget is added to the combo box.
 *
 *  The combo box must keep a signal handler connected for the focus
 *  out event for the instant apply functionality.
 *
 *  \param [in] combo  This combo box
 *  \param [in] widget The entry widget added to the combo box
 *  \param [in] unused
 */
static void
add_widget (GschemIntegerComboBox *combo, GtkWidget *widget, gpointer unused)
{
  g_return_if_fail (combo != NULL);
  g_return_if_fail (widget != NULL);

  g_signal_connect(G_OBJECT (widget),
                   "focus-out-event",
                   G_CALLBACK (focus_out_event),
                   combo);
}



/*! \brief The entry widget inside the combo box lost focus.
 *
 *  \param [in] entry  The entry inside the combo box
 *  \param [in] event  The focus out event
 *  \param [in] combo  The combo box
 */
static gboolean
focus_out_event (GtkEntry *entry, GdkEvent *event, GschemIntegerComboBox *combo)
{
  g_return_val_if_fail (entry != NULL, FALSE);
  g_return_val_if_fail (combo != NULL, FALSE);

  if (combo->changed) {
    g_signal_emit_by_name (combo, "apply");
    combo->changed = FALSE;
  }

  return FALSE;
}



/*! \brief Initialize GschemIntegerComboBoxClass class
 *
 *  \param [in] klass The class for the GschemIntegerComboBoxClass
 */
static void
gschem_integer_combo_box_class_init (GschemIntegerComboBoxClass *klass)
{
  g_return_if_fail (klass != NULL);

  g_signal_new ("apply",                          /* signal_name  */
                G_OBJECT_CLASS_TYPE (klass),      /* itype        */
                (GSignalFlags) 0,                 /* signal_flags */
                0,                                /* class_offset */
                NULL,                             /* accumulator  */
                NULL,                             /* accu_data    */
                g_cclosure_marshal_VOID__VOID,    /* c_marshaller */
                G_TYPE_NONE,                      /* return_type  */
                0                                 /* n_params     */
                );
}



/*! \brief Get the entry associated with this combo box
 *
 *  \param [in] widget The integer combo box
 *  \return The entry
 */
GtkEntry*
gschem_integer_combo_box_get_entry (GtkWidget *widget)
{
  g_return_val_if_fail (widget != NULL, NULL);

  return GTK_ENTRY (gtk_bin_get_child (GTK_BIN (widget)));
}


/*! \brief Get the integer value
 *
 *  \param [in,out] widget  The integer combo box
 *  \return The integer. If the value is invalid, this function returns -1.
 */
int
gschem_integer_combo_box_get_value (GtkWidget *widget)
{
  GtkWidget *entry = GTK_WIDGET (gschem_integer_combo_box_get_entry (widget));
  int size = -1;
  const char *text0 = gtk_entry_get_text (GTK_ENTRY (entry));

  if (text0 != NULL)
  {
    long temp;
    char *text1;

    errno = 0;

    temp = strtol (text0, &text1, 0);

    if ((errno == 0) && (text1 != NULL) && (*text1 == '\0') && (temp >= 0)) {
      size = temp;
    }
  }

  return size;
}



/*! \brief Initialize a GschemIntegerComboBox
 *
 *  \param [in] combo The instance of a #GschemIntegerComboBox.
 */
static void
gschem_integer_combo_box_init (GschemIntegerComboBox *combo)
{
  g_return_if_fail (combo != NULL);

  combo->changed = FALSE;

  g_signal_connect(G_OBJECT (combo),
                   "notify::active",
                   G_CALLBACK (notify_active),
                   NULL);

  g_signal_connect(G_OBJECT (combo),
                   "add",
                   G_CALLBACK (add_widget),
                   NULL);

  g_signal_connect(G_OBJECT (combo),
                   "changed",
                   G_CALLBACK (value_changed),
                   NULL);

  g_signal_connect(G_OBJECT (combo),
                   "remove",
                   G_CALLBACK (remove_widget),
                   NULL);
}



/*! \brief Create a ComboBox with an entry for integer values.
 *
 *  \return A GtkWidget for entering integer values
 */
GtkWidget*
gschem_integer_combo_box_new ()
{
#if GTK_CHECK_VERSION (2, 24, 0)
  return GTK_WIDGET (g_object_new (GSCHEM_TYPE_INTEGER_COMBO_BOX, "has-entry", TRUE, NULL));
#else
  return GTK_WIDGET (g_object_new (GSCHEM_TYPE_INTEGER_COMBO_BOX, NULL));
#endif
}



/*! \brief Set the list store containing the common values
 *
 *  \param [in,out] widget  The integer combo box
 *  \param [in]     store   The list containing the common values
 */
void
gschem_integer_combo_box_set_model (GtkWidget *widget, GtkListStore *store)
{
  g_return_if_fail (widget != NULL);

  gtk_combo_box_set_model (GTK_COMBO_BOX (widget), GTK_TREE_MODEL (store));

  if (store != NULL) {
#if GTK_CHECK_VERSION (2, 24, 0)
    gtk_combo_box_set_entry_text_column (GTK_COMBO_BOX (widget), x_integerls_get_value_column ());
#else
    gtk_combo_box_entry_set_text_column (GTK_COMBO_BOX_ENTRY (widget), x_integerls_get_value_column ());
#endif
  }
}



/*! \brief Set the integer value
 *
 *  \param [in,out] widget  The integer combo box
 *  \param [in]     value   The value
 */
void
gschem_integer_combo_box_set_value (GtkWidget *widget, int value)
{
  GtkWidget *entry;
  g_return_if_fail (widget != NULL);
  entry = gtk_bin_get_child (GTK_BIN (widget));
  g_return_if_fail (entry != NULL);

  if (value >= 0) {
    GString *string;

    string = g_string_new (NULL);
    g_string_printf (string, "%d", value);
    gtk_entry_set_text (GTK_ENTRY (entry), string->str);

    g_string_free (string, TRUE);
  } else {
    gtk_entry_set_text (GTK_ENTRY (entry), "");
  }
}


/*! \brief The active item in the combo box has changed
 */
static void
notify_active (GschemIntegerComboBox *combo, GParamSpec *pspec, gpointer unused)
{
  g_return_if_fail (combo != NULL);

  g_signal_emit_by_name (combo, "apply");
  combo->changed = FALSE;
}



/*! \brief The entry widget is removed from the combo box.
 *
 *  The combo box must keep a signal handler connected for the focus
 *  out event for the instant apply functionality.
 *
 *  \param [in] combo  This combo box
 *  \param [in] widget The netry widget added to the combo box
 *  \param [in] unused
 */
static void
remove_widget (GschemIntegerComboBox *combo, GtkWidget *widget, gpointer unused)
{
  g_return_if_fail (combo != NULL);
  g_return_if_fail (widget != NULL);

  g_signal_handlers_disconnect_by_func(G_OBJECT (widget),
                                       (gpointer) focus_out_event,
                                       combo);
}



/*! \brief Respond to a change in the value
 *
 *  The function checks the focus to determine the source of the event.
 *  If the entry has focus, the user changed the value, possibly by one
 *  keystroke, and the dialog box should not update the selection. If the
 *  entry does not have focus, the user changed the value in the dropdown
 *  menu, and the dialog box must update the selection.
 *
 *  \param [in] combo  This widget
 *  \param [in] unused
 */
static void
value_changed (GschemIntegerComboBox *combo, gpointer unused)
{
  g_return_if_fail (combo != NULL);

  if (gtk_widget_is_focus (GTK_WIDGET (gschem_integer_combo_box_get_entry (GTK_WIDGET (combo))))) {
    combo->changed = TRUE;
  }

  /* This code was getting called before the value changed. The signal handler
   * was fetching data from one selection in the past.
   *
   * This code is here in case of compatibility issues pre and post 2.24, that
   * there will be some info for addressing the issue.
   */

#if 0
  else {
    g_signal_emit_by_name (combo, "apply");
    combo->changed = FALSE;
  }
#endif
}
