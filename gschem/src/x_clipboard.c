/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2009 gEDA Contributors (see ChangeLog for details)
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
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "gschem.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#define MIME_TYPE_SCHEMATIC "application/x-geda-schematic"
#define CLIP_TYPE_SCHEMATIC 1

/* \brief Callback for handling system clipboard owner change.
 * \par Function Description
 *
 */
static void
clip_handle_owner_change (GtkClipboard *cb, GdkEvent *event,
                          gpointer user_data)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL *) user_data;

  i_update_menus (w_current);
}

static void
clip_get (GtkClipboard *cb, GtkSelectionData *selection_data,
          guint info, gpointer user_data_or_owner)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL *) user_data_or_owner;
  TOPLEVEL *toplevel = w_current->toplevel;
  GdkAtom type = gdk_atom_intern (MIME_TYPE_SCHEMATIC, FALSE);
  gchar *buf;
  if (info != CLIP_TYPE_SCHEMATIC) return;
  /* Convert the objects in the clipboard buffer to gEDA schematic
   * format */
  buf = o_save_buffer (toplevel, w_current->clipboard_buffer);
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
  /* Do nothing for now */
}

/* \brief Initialises system clipboard support
 * \par Function Description
 * Registers a signal handler to detect if the clipboard has changed
 * and update the menu item sensitivity if necessary.
 */
void
x_clipboard_init (GSCHEM_TOPLEVEL *w_current)
{
  GtkClipboard *cb = gtk_clipboard_get (GDK_SELECTION_CLIPBOARD);
  g_signal_connect (G_OBJECT (cb),
                    "owner-change",
                    G_CALLBACK (clip_handle_owner_change),
                    w_current);
}

/* \brief Checks if the system clipboard contains schematic data.
 * \par Function Description
 * Checks whether the current owner of the system clipboard is
 * advertising gEDA schematic data.
 *
 * \param [in,out] w_current   The current GSCHEM_TOPLEVEL.
 *
 * \return TRUE if the clipboard has schematic data.
 */
gboolean
x_clipboard_usable (GSCHEM_TOPLEVEL *w_current)
{
  GtkClipboard *cb = gtk_clipboard_get (GDK_SELECTION_CLIPBOARD);
  GdkAtom *targets;
  gint ntargets;
  int i;

  gtk_clipboard_wait_for_targets (cb, &targets, &ntargets);
  for (i = 0; i < ntargets; i++) {
    if (strcmp (gdk_atom_name (targets[i]), MIME_TYPE_SCHEMATIC) == 0)
      return TRUE;
  }
  return FALSE;
}

/* \brief Set the contents of the system clipboard.
 * \par Function Description
 * Set the system clipboard to contain the gschem objects listed in \a
 * object_list.
 *
 * \param [in,out] w_current   The current GSCHEM_TOPLEVEL.
 * \param [in]     object_list The objects to put in the clipboard.
 *
 * \return TRUE if the clipboard is successfully set.
 */
gboolean
x_clipboard_set (GSCHEM_TOPLEVEL *w_current, const GList *object_list)
{
  GtkClipboard *cb = gtk_clipboard_get (GDK_SELECTION_CLIPBOARD);
  GtkTargetEntry target = { MIME_TYPE_SCHEMATIC, 0,
                            CLIP_TYPE_SCHEMATIC };
  TOPLEVEL *toplevel = w_current->toplevel;
  gboolean result;

  /* Clear the clipboard buffer */
  s_delete_object_glist(toplevel, w_current->clipboard_buffer);
  w_current->clipboard_buffer = NULL;

  /* Copy the objects to the clipboard buffer */
  w_current->clipboard_buffer =
    o_glist_copy_all (toplevel, object_list, w_current->clipboard_buffer,
                      SELECTION_FLAG);

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
 * \param [in,out] w_current   The current GSCHEM_TOPLEVEL.
 *
 * \returns Any OBJECTs retrieved from the system clipboard, or NULL
 *          if none were available.
 */
GList *
x_clipboard_get (GSCHEM_TOPLEVEL *w_current)
{
  GtkClipboard *cb = gtk_clipboard_get (GDK_SELECTION_CLIPBOARD);
  TOPLEVEL *toplevel = w_current->toplevel;
  GdkAtom type = gdk_atom_intern (MIME_TYPE_SCHEMATIC, FALSE);
  GtkSelectionData *selection_data;
  GList *object_list = NULL;
  const guchar *buf;

  /* Try to get the contents of the clipboard */
  selection_data = gtk_clipboard_wait_for_contents (cb, type);
  if (selection_data == NULL) return FALSE;

  /* Convert the data buffer to OBJECTs */
  buf = gtk_selection_data_get_data (selection_data);

  toplevel->ADDING_SEL = 1; /* HACK: Avoid adding objects to the tile system */
  object_list = o_read_buffer (toplevel, object_list,
                               (gchar *) buf, -1, "Clipboard");
  toplevel->ADDING_SEL = 0;

  gtk_selection_data_free (selection_data);
  return object_list;
}
