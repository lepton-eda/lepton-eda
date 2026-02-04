/* Lepton EDA Schematic Capture
 * Copyright (C) 2023-2026 Lepton EDA Contributors
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

/*!
 * \file gtk_helper.c
 *
 * \brief GTK helper functions.
 *
 */

#include <config.h>

#include "schematic.h"

/*! \brief Transform a GTK response id value to string.
 *
 * \par Function Description
 * Given a GTK response type id \p response, returns the string
 * corresponding to it.  This is mainly intended to be used for
 * value conversion in Scheme FFI functions.
 *
 * \param [in] response The response id.
 * \return The string corresponding to the id.
 */
const char*
gtk_response_to_string (int response)
{
  const char *result = "unknown";

  switch (response)
  {
  case GTK_RESPONSE_NONE: result = "none"; break;
  case GTK_RESPONSE_REJECT: result = "reject"; break;
  case GTK_RESPONSE_ACCEPT: result = "accept"; break;
  case GTK_RESPONSE_DELETE_EVENT: result = "delete-event"; break;
  case GTK_RESPONSE_OK: result = "ok"; break;
  case GTK_RESPONSE_CANCEL: result = "cancel"; break;
  case GTK_RESPONSE_CLOSE: result = "close"; break;
  case GTK_RESPONSE_YES: result = "yes"; break;
  case GTK_RESPONSE_NO: result = "no"; break;
  case GTK_RESPONSE_APPLY: result = "apply"; break;
  case GTK_RESPONSE_HELP: result = "help"; break;
  default: break;
  }

  return result;
}


/*! \brief Cast \c GtkWidget to \c GtkWindow.
 *
 *  \par Function Description
 *  The helper function casts a \c GtkWidget instance to \c
 *  GtkWindow and returns the result.  Its only purpose is to be
 *  used in Scheme FFI which doesn't support C macros.  While not
 *  strongly necessary, it is an additional check, just in case.
 *
 *  \param [in] widget The \c GtkWidget object.
 *  \return The resulting \c GtkWindow object.
 */
GtkWindow*
gtk_widget_get_gtk_window (GtkWidget *widget)
{
  return GTK_WINDOW (widget);
}


void
gtk_widget_pack_child (GtkWidget *parent_widget,
                       GtkWidget *child_widget)
{
  gtk_box_pack_start (GTK_BOX (parent_widget), child_widget, FALSE, FALSE, 0);
}



/*! \brief Transform a string into GTK response id value.
 *
 * \par Function Description
 * Given a string naming a GTK response type id, return the enum
 * value corresponding to it.  This is mainly intended to be used
 * for value conversion in Scheme FFI functions.
 *
 * \param [in] s The string.
 * \return The GTK response id value.
 */
int
gtk_string_to_response (char *s)
{
  int result = GTK_RESPONSE_NONE;

  if (strcmp (s, "none") == 0) {result = GTK_RESPONSE_NONE; }
  else if (strcmp (s, "reject") == 0) {result = GTK_RESPONSE_REJECT; }
  else if (strcmp (s, "accept") == 0) {result = GTK_RESPONSE_ACCEPT; }
  else if (strcmp (s, "delete-event") == 0) {result = GTK_RESPONSE_DELETE_EVENT; }
  else if (strcmp (s, "ok") == 0) {result = GTK_RESPONSE_OK; }
  else if (strcmp (s, "cancel") == 0) {result = GTK_RESPONSE_CANCEL; }
  else if (strcmp (s, "close") == 0) {result = GTK_RESPONSE_CLOSE; }
  else if (strcmp (s, "yes") == 0) {result = GTK_RESPONSE_YES; }
  else if (strcmp (s, "no") == 0) {result = GTK_RESPONSE_NO; }
  else if (strcmp (s, "apply") == 0) {result = GTK_RESPONSE_APPLY; }
  else if (strcmp (s, "help") == 0) {result = GTK_RESPONSE_HELP; }

  return result;
}


/*! \brief Return an event scroll direction enum value from string.
 *
 *  \par Function Description
 *  Returns the \c GdkScrollDirection enum value corresponding to
 *  the string \p s.  This is mainly intended to be used for value
 *  conversion in Scheme FFI functions.
 *
 *  \param [in] s The string.
 *  \return The \c GdkScrollDirection value corresponding to the
 *          string.
 */
GdkScrollDirection
gdk_event_scroll_direction_from_string (char *s)
{
  GdkScrollDirection result = GDK_SCROLL_UP;

  if      (strcmp (s, "gdk-scroll-up") == 0) {result = GDK_SCROLL_UP; }
  else if (strcmp (s, "gdk-scroll-down") == 0) {result = GDK_SCROLL_DOWN; }
  else if (strcmp (s, "gdk-scroll-left") == 0) {result = GDK_SCROLL_LEFT; }
  else if (strcmp (s, "gdk-scroll-right") == 0) {result = GDK_SCROLL_RIGHT; }
#ifdef GTK3
  else if (strcmp (s, "gdk-scroll-smooth") == 0) {result = GDK_SCROLL_SMOOTH; }
#endif

  return result;
}


/*! \brief Return a string holding the representation of \c
 *  GdkScrollDirection value.
 *
 * \par Function Description
 * Returns the external representation of a \c GdkScrollDirection
 * value as a string.  This is mainly intended to be used for
 * value conversion in Scheme FFI functions.
 *
 * \param [in] mode The \c GdkScrollDirection value.
 * \return The string representing the \c GdkScrollDirection
 *         value.
 */
const char*
gdk_event_scroll_direction_to_string (GdkScrollDirection mode)
{
  const char *result = NULL;

  switch (mode)
  {
  case GDK_SCROLL_UP: result = "gdk-scroll-up"; break;
  case GDK_SCROLL_DOWN: result = "gdk-scroll-down"; break;
  case GDK_SCROLL_LEFT: result = "gdk-scroll-left"; break;
  case GDK_SCROLL_RIGHT: result = "gdk-scroll-right"; break;
#ifdef GTK3
  case GDK_SCROLL_SMOOTH: result = "gdk-scroll-smooth"; break;
#endif
  default: break;
  }

  return result;
}
