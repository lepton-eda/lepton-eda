/* Lepton EDA Schematic Capture
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
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "gschem.h"

#define MIME_TYPE_SCHEMATIC "application/x-lepton-schematic"
#define CLIP_TYPE_SCHEMATIC 1

/* \brief Callback for handling system clipboard owner change.
 * \par Function Description
 *
 */
static void
clip_handle_owner_change (GtkClipboard *cb, GdkEvent *event,
                          gpointer user_data)
{
  GschemToplevel *w_current = (GschemToplevel *) user_data;

  i_update_menus (w_current);
}

static void
clip_get (GtkClipboard *cb, GtkSelectionData *selection_data,
          guint info, gpointer user_data_or_owner)
{
  GschemToplevel *w_current = (GschemToplevel *) user_data_or_owner;
  GdkAtom type = gdk_atom_intern (MIME_TYPE_SCHEMATIC, FALSE);
  gchar *buf;
  if (info != CLIP_TYPE_SCHEMATIC) return;
  /* Convert the objects in the clipboard buffer to gEDA schematic
   * format */
  buf = lepton_object_list_to_buffer (w_current->clipboard_buffer);
  /* Set the selection appropriately */
  gtk_selection_data_set (selection_data, type,
                          8, /* 8-bit data (UTF-8) */
                          (guchar *) buf,
                          (gint) strlen(buf));
  g_free (buf);
}

static void
clip_clear (GtkClipboard *cb, gpointer user_data_or_owner)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (user_data_or_owner);

  /* Free the objects in the clipboard buffer */
  lepton_object_list_delete (w_current->clipboard_buffer);
  w_current->clipboard_buffer = NULL;
}

/* \brief Initialises system clipboard support
 * \par Function Description
 * Registers a signal handler to detect if the clipboard has changed
 * and update the menu item sensitivity if necessary.
 */
void
x_clipboard_init (GschemToplevel *w_current)
{
  GtkClipboard *cb = gtk_clipboard_get (GDK_SELECTION_CLIPBOARD);
  g_signal_connect (G_OBJECT (cb),
                    "owner-change",
                    G_CALLBACK (clip_handle_owner_change),
                    w_current);
}

/* \brief Initialises system clipboard support
 * \par Function Description
 * Registers a signal handler to detect if the clipboard has changed
 * and update the menu item sensitivity if necessary.
 */
void
x_clipboard_finish (GschemToplevel *w_current)
{
  GtkClipboard *cb = gtk_clipboard_get (GDK_SELECTION_CLIPBOARD);
  g_signal_handlers_disconnect_by_func (cb,
                                        (gpointer) clip_handle_owner_change,
                                        w_current);
  if (w_current->clipboard_buffer)
    gtk_clipboard_store (cb);
}

struct query_usable {
  void (*callback) (int, void *);
  void *userdata;
};


/* \brief Callback for determining if any clipboard targets are pastable
 * \par Function Description
 *
 * Checks if the clipboard targets match any format we recognise, then
 * calls back into a supplied callback function which is interested in
 * the TRUE / FALSE answer to whether we can paste from the clipboard.
 */
static void
query_usable_targets_cb (GtkClipboard *clip, GdkAtom *targets, gint ntargets,
                         gpointer data)
{
  struct query_usable *cbinfo = (struct query_usable*) data;
  int i;
  int is_usable = FALSE;

  for (i = 0; i < ntargets; i++) {
    if (strcmp (gdk_atom_name (targets[i]), MIME_TYPE_SCHEMATIC) == 0) {
      is_usable = TRUE;
      break;
    }
  }

  cbinfo->callback (is_usable, cbinfo->userdata);
  g_free (cbinfo);
}


/* \brief Checks if the system clipboard contains schematic data.
 * \par Function Description
 * Checks whether the current owner of the system clipboard is
 * advertising gEDA schematic data.
 *
 * The check is performed asynchronously. When a response is
 * recieved, the provided callback is called with a TRUE / FALSE
 * result.
 *
 * \param [in] w_current   The current GschemToplevel.
 * \param [in] callback    The callback to recieve the response.
 * \param [in] userdata    Arbitrary data to pass the callback.
 */
void
x_clipboard_query_usable (GschemToplevel *w_current,
                          void (*callback) (int, void *), void *userdata)
{
  GtkClipboard *clip = gtk_clipboard_get (GDK_SELECTION_CLIPBOARD);
  struct query_usable *cbinfo;

  cbinfo = g_new (struct query_usable, 1);
  cbinfo->callback = callback;
  cbinfo->userdata = userdata;

  gtk_clipboard_request_targets (clip, query_usable_targets_cb, cbinfo);
}

/* \brief Set the contents of the system clipboard.
 * \par Function Description
 * Set the system clipboard to contain the gschem objects listed in \a
 * object_list.
 *
 * \param [in,out] w_current   The current GschemToplevel.
 * \param [in]     object_list The objects to put in the clipboard.
 *
 * \return TRUE if the clipboard is successfully set.
 */
gboolean
x_clipboard_set (GschemToplevel *w_current, const GList *object_list)
{
  GtkClipboard *cb = gtk_clipboard_get (GDK_SELECTION_CLIPBOARD);
  GtkTargetEntry target = { (gchar*) MIME_TYPE_SCHEMATIC, 0,
                            CLIP_TYPE_SCHEMATIC };
  gboolean result;

  /* Clear the clipboard buffer */
  if (w_current->clipboard_buffer)
    gtk_clipboard_clear (cb);

  /* Copy the objects to the clipboard buffer */
  w_current->clipboard_buffer =
    o_glist_copy_all (object_list, w_current->clipboard_buffer);

  /* Advertise that the data is available */
  result = gtk_clipboard_set_with_data (cb, &target, 1,
                                        clip_get, clip_clear, w_current);

  /* Hint that the data can be stored to be accessed after the program
   * has quit. */
  gtk_clipboard_set_can_store (cb, NULL, 0);

  return result;
}

/* \brief Get the contents of the system clipboard.
 * \par Function Description
 * If the system clipboard contains schematic data, retrieve it.
 *
 * \param [in,out] w_current   The current GschemToplevel.
 *
 * \returns Any LeptonObjects retrieved from the system clipboard,
 *          or NULL if none were available.
 */
GList *
x_clipboard_get (GschemToplevel *w_current)
{
  GtkClipboard *cb = gtk_clipboard_get (GDK_SELECTION_CLIPBOARD);
  GdkAtom type = gdk_atom_intern (MIME_TYPE_SCHEMATIC, FALSE);
  GtkSelectionData *selection_data;
  GList *object_list = NULL;
  const guchar *buf;
  GError * err = NULL;
  LeptonPage *active_page = schematic_window_get_active_page (w_current);

  /* Try to get the contents of the clipboard */
  selection_data = gtk_clipboard_wait_for_contents (cb, type);
  if (selection_data == NULL) return FALSE;

  /* Convert the data buffer to LeptonObjects */
#if GTK_CHECK_VERSION(2,14,0)
  buf = gtk_selection_data_get_data (selection_data);
#else
  buf = selection_data->data;
#endif

  active_page = schematic_window_get_active_page (w_current);
  object_list = o_read_buffer (active_page, object_list,
                               (gchar *) buf, -1, "Clipboard", &err);

  if (err) {
    GtkWidget * dialog = gtk_message_dialog_new_with_markup
      (GTK_WINDOW (w_current->main_window),
       GTK_DIALOG_DESTROY_WITH_PARENT,
       GTK_MESSAGE_ERROR,
       GTK_BUTTONS_OK,
       _("<b>Invalid schematic on clipboard.</b>\n\nAn error occurred while inserting clipboard data: %s."),
       err->message);
    gtk_window_set_title (GTK_WINDOW (dialog), _("Clipboard insertion failed"));

     gtk_dialog_run (GTK_DIALOG (dialog));
     gtk_widget_destroy (dialog);
     g_error_free(err);
  }
  gtk_selection_data_free (selection_data);
  return object_list;
}
