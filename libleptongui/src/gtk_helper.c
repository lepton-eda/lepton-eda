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


/*! \brief Transform a GTK policy id value to string.
 *
 * \par Function Description
 * Given a \c GtkPolicyType id \p policy, returns the string
 * corresponding to it.  This is mainly intended to be used for
 * value conversion in Scheme FFI functions.
 *
 * \param [in] policy The policy id.
 * \return The string corresponding to the id.
 */
const char*
gtk_policy_to_string (int policy)
{
  const char *result = "unknown";

  switch (policy)
  {
  case GTK_POLICY_ALWAYS: result = "always"; break;
  case GTK_POLICY_AUTOMATIC: result = "automatic"; break;
  case GTK_POLICY_NEVER: result = "never"; break;
#ifdef ENABLE_GTK3
  case GTK_POLICY_EXTERNAL: result = "external"; break;
#endif
  default: break;
  }

  return result;
}


/*! \brief Transform a string into GTK policy id value.
 *
 * \par Function Description
 * Given a string naming a \c GtkPolicyType id, return the enum
 * value corresponding to it.  This is mainly intended to be used
 * for value conversion in Scheme FFI functions.
 *
 * \param [in] s The string.
 * \return The GTK policy id value.
 */
int
gtk_string_to_policy (char *s)
{
  int result = GTK_POLICY_ALWAYS;

  if (strcmp (s, "always") == 0) {result = GTK_POLICY_ALWAYS; }
  else if (strcmp (s, "automatic") == 0) {result = GTK_POLICY_AUTOMATIC; }
  else if (strcmp (s, "never") == 0) {result = GTK_POLICY_NEVER; }
#ifdef ENABLE_GTK3
  else if (strcmp (s, "external") == 0) {result = GTK_POLICY_EXTERNAL; }
#endif

  return result;
}


/* GdkWindowTypeHint enum helpers.
 *
 * GTK2 source:
 * - /usr/include/gtk-2.0/gdk/gdkwindow.h
 * GTK3 source:
 * - /usr/include/gtk-3.0/gdk/gdktypes.h
 */

/*! \brief Transform a \c GdkWindowTypeHint value to string.
 *
 * \par Function Description

 * Returns a string corresponding to the given \c
 * GdkWindowTypeHint value.  This is mainly intended to be used
 * for value conversion in Scheme FFI functions.
 *
 * \param [in] hint The \c GdkWindowTypeHint value.
 * \return The string corresponding to the value.
 */
const char*
gdk_window_type_hint_to_string (int hint)
{
  const char *result = "normal";

  switch (hint)
  {
  case GDK_WINDOW_TYPE_HINT_NORMAL: result = "normal"; break;
  case GDK_WINDOW_TYPE_HINT_DIALOG: result = "dialog"; break;
  case GDK_WINDOW_TYPE_HINT_MENU: result = "menu"; break;
  case GDK_WINDOW_TYPE_HINT_TOOLBAR: result = "toolbar"; break;
  case GDK_WINDOW_TYPE_HINT_SPLASHSCREEN: result = "splashscreen"; break;
  case GDK_WINDOW_TYPE_HINT_UTILITY: result = "utility"; break;
  case GDK_WINDOW_TYPE_HINT_DOCK: result = "dock"; break;
  case GDK_WINDOW_TYPE_HINT_DESKTOP: result = "desktop"; break;
  case GDK_WINDOW_TYPE_HINT_DROPDOWN_MENU: result = "dropdown-menu"; break;
  case GDK_WINDOW_TYPE_HINT_POPUP_MENU: result = "popup-menu"; break;
  case GDK_WINDOW_TYPE_HINT_TOOLTIP: result = "tooltip"; break;
  case GDK_WINDOW_TYPE_HINT_NOTIFICATION: result = "notification"; break;
  case GDK_WINDOW_TYPE_HINT_COMBO: result = "combo"; break;
  case GDK_WINDOW_TYPE_HINT_DND: result = "dnd"; break;
  default: break;
  }

  return result;
}


/*! \brief Transform a string into a \c GdkWindowTypeHint value.
 *
 * \par Function Description
 * Returns a \c GdkWindowTypeHint enum value corresponding to the
 * given string.  This is mainly intended to be used for value
 * conversion in Scheme FFI functions.
 *
 * \param [in] s The string.
 * \return The \c GdkWindowTypeHint value.
 */
int
gdk_string_to_window_type_hint (char *s)
{
  int result = GDK_WINDOW_TYPE_HINT_NORMAL;

  if (strcmp (s, "normal") == 0) {result = GDK_WINDOW_TYPE_HINT_NORMAL; }
  else if (strcmp (s, "dialog") == 0) {result = GDK_WINDOW_TYPE_HINT_DIALOG; }
  else if (strcmp (s, "menu") == 0) {result = GDK_WINDOW_TYPE_HINT_MENU; }
  else if (strcmp (s, "toolbar") == 0) {result = GDK_WINDOW_TYPE_HINT_TOOLBAR; }
  else if (strcmp (s, "splashscreen") == 0) {result = GDK_WINDOW_TYPE_HINT_SPLASHSCREEN; }
  else if (strcmp (s, "utility") == 0) {result = GDK_WINDOW_TYPE_HINT_UTILITY; }
  else if (strcmp (s, "dock") == 0) {result = GDK_WINDOW_TYPE_HINT_DOCK; }
  else if (strcmp (s, "desktop") == 0) {result = GDK_WINDOW_TYPE_HINT_DESKTOP; }
  else if (strcmp (s, "dropdown-menu") == 0) {result = GDK_WINDOW_TYPE_HINT_DROPDOWN_MENU; }
  else if (strcmp (s, "popup-menu") == 0) {result = GDK_WINDOW_TYPE_HINT_POPUP_MENU; }
  else if (strcmp (s, "tooltip") == 0) {result = GDK_WINDOW_TYPE_HINT_TOOLTIP; }
  else if (strcmp (s, "notification") == 0) {result = GDK_WINDOW_TYPE_HINT_NOTIFICATION; }
  else if (strcmp (s, "combo") == 0) {result = GDK_WINDOW_TYPE_HINT_COMBO; }
  else if (strcmp (s, "dnd") == 0) {result = GDK_WINDOW_TYPE_HINT_DND; }

  return result;
}


/*! \brief Transform a GTK message type id value to string.
 *
 * \par Function Description
 *
 * Given a GTK message type id \p type, returns the string
 * corresponding to it.  This is mainly intended to be used for
 * value conversion in Scheme FFI functions.
 *
 * \param [in] type The message type id.
 * \return The string corresponding to the id.
 */
const char*
gtk_message_type_to_string (int type)
{
  const char *result = "unknown";

  switch (type)
  {
  case GTK_MESSAGE_INFO: result = "info"; break;
  case GTK_MESSAGE_WARNING: result = "warning"; break;
  case GTK_MESSAGE_QUESTION: result = "question"; break;
  case GTK_MESSAGE_ERROR: result = "error"; break;
  case GTK_MESSAGE_OTHER: result = "other"; break;
  default: break;
  }

  return result;
}


/*! \brief Transform a string into GTK message type id value.
 *
 * \par Function Description
 *
 * Given a string naming a GTK message type id, return the enum
 * value corresponding to it.  This is mainly intended to be used
 * for value conversion in Scheme FFI functions.
 *
 * \param [in] s The string.
 * \return The GTK message type id value.
 */
int
gtk_string_to_message_type (char *s)
{
  int result = GTK_MESSAGE_INFO;

  if (strcmp (s, "info") == 0) {result = GTK_MESSAGE_INFO; }
  else if (strcmp (s, "warning") == 0) {result = GTK_MESSAGE_WARNING; }
  else if (strcmp (s, "question") == 0) {result = GTK_MESSAGE_QUESTION; }
  else if (strcmp (s, "error") == 0) {result = GTK_MESSAGE_ERROR; }
  else if (strcmp (s, "other") == 0) {result = GTK_MESSAGE_OTHER; }

  return result;
}


/*! \brief Transform a GTK buttons type id value to string.
 *
 * \par Function Description
 *
 * Given a GTK buttons type id \p type, returns the string
 * corresponding to it.  This is mainly intended to be used for
 * value conversion in Scheme FFI functions.
 *
 * \param [in] type The buttons type id.
 * \return The string corresponding to the id.
 */
const char*
gtk_buttons_type_to_string (int type)
{
  const char *result = "unknown";

  switch (type)
  {
  case GTK_BUTTONS_NONE: result = "none"; break;
  case GTK_BUTTONS_OK: result = "ok"; break;
  case GTK_BUTTONS_CLOSE: result = "close"; break;
  case GTK_BUTTONS_CANCEL: result = "cancel"; break;
  case GTK_BUTTONS_YES_NO: result = "yes-no"; break;
  case GTK_BUTTONS_OK_CANCEL: result = "ok-cancel"; break;
  default: break;
  }

  return result;
}


/*! \brief Transform a string into GTK buttons type id value.
 *
 * \par Function Description
 *
 * Given a string naming a GTK buttons type id, return the enum
 * value corresponding to it.  This is mainly intended to be used
 * for value conversion in Scheme FFI functions.
 *
 * \param [in] s The string.
 * \return The GTK buttons type id value.
 */
int
gtk_string_to_buttons_type (char *s)
{
  int result = GTK_BUTTONS_NONE;

  if (strcmp (s, "none") == 0) {result = GTK_BUTTONS_NONE; }
  else if (strcmp (s, "ok") == 0) {result = GTK_BUTTONS_OK; }
  else if (strcmp (s, "close") == 0) {result = GTK_BUTTONS_CLOSE; }
  else if (strcmp (s, "cancel") == 0) {result = GTK_BUTTONS_CANCEL; }
  else if (strcmp (s, "yes-no") == 0) {result = GTK_BUTTONS_YES_NO; }
  else if (strcmp (s, "ok-cancel") == 0) {result = GTK_BUTTONS_OK_CANCEL; }

  return result;
}


/*! \brief Transform a GTK file chooser action id value to string.
 *
 * \par Function Description
 *
 * Given a GTK file chooser action id \p action, returns the
 * string corresponding to it.  This is mainly intended to be used
 * for value conversion in Scheme FFI functions.
 *
 * \param [in] action The file chooser action id.
 * \return The string corresponding to the id.
 */
const char*
gtk_file_chooser_action_to_string (int action)
{
  const char *result = "unknown";

  switch (action)
  {
  case GTK_FILE_CHOOSER_ACTION_OPEN: result = "open"; break;
  case GTK_FILE_CHOOSER_ACTION_SAVE: result = "save"; break;
  case GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER: result = "select-folder"; break;
  case GTK_FILE_CHOOSER_ACTION_CREATE_FOLDER: result = "create-folder"; break;
  default: break;
  }

  return result;
}


/*! \brief Transform a string into GTK file chooser action id
 *  value.
 *
 * \par Function Description
 *
 * Given a string naming a GTK file chooser action id, return the
 * enum value corresponding to it.  This is mainly intended to be
 * used for value conversion in Scheme FFI functions.
 *
 * \param [in] s The string.
 * \return The GTK file chooser action id value.
 */
int
gtk_string_to_file_chooser_action (char *s)
{
  int result = GTK_FILE_CHOOSER_ACTION_OPEN;

  if (strcmp (s, "open") == 0) {result = GTK_FILE_CHOOSER_ACTION_OPEN; }
  else if (strcmp (s, "save") == 0) {result = GTK_FILE_CHOOSER_ACTION_SAVE; }
  else if (strcmp (s, "select-folder") == 0) {result = GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER; }
  else if (strcmp (s, "create-folder") == 0) {result = GTK_FILE_CHOOSER_ACTION_CREATE_FOLDER; }

  return result;
}


/*! \brief Transform a \c GtkJustification value to string.
 *
 * \par Function Description
 *
 * Given a \c GtkJustification value, returns the string
 * corresponding to it.  This is mainly intended to be used for
 * value conversion in Scheme FFI functions.
 *
 * \param [in] val The justification value.
 * \return The string corresponding to the value.
 */
const char*
schematic_gtk_justification_to_string (int val)
{
  const char *result = "unknown";

  switch (val)
  {
  case GTK_JUSTIFY_LEFT: result = "left"; break;
  case GTK_JUSTIFY_RIGHT: result = "right"; break;
  case GTK_JUSTIFY_CENTER: result = "center"; break;
  case GTK_JUSTIFY_FILL: result = "fill"; break;
  default: break;
  }

  return result;
}


/*! \brief Transform a string into \c GtkJustification value.
 *
 * \par Function Description
 *
 * Given a string naming a \c GtkJustification value, return the
 * enum value corresponding to it.  This is mainly intended to be
 * used for value conversion in Scheme FFI functions.
 *
 * \param [in] s The string.
 * \return The \c GtkJustification value.
 */
int
schematic_gtk_justification_from_string (char *s)
{
  int result = GTK_JUSTIFY_LEFT;

  if (strcmp (s, "left") == 0) {result = GTK_JUSTIFY_LEFT; }
  else if (strcmp (s, "right") == 0) {result = GTK_JUSTIFY_RIGHT; }
  else if (strcmp (s, "center") == 0) {result = GTK_JUSTIFY_CENTER; }
  else if (strcmp (s, "fill") == 0) {result = GTK_JUSTIFY_FILL; }

  return result;
}
