/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2013 gEDA Contributors (see ChangeLog for details)
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
/*! \todo STILL NEED to clean up line lengths in aa and tr */
#include <config.h>
#include <version.h>

#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "gschem.h"

#define GLADE_HOOKUP_OBJECT(component,widget,name) \
  g_object_set_data_full (G_OBJECT (component), name, \
    gtk_widget_ref (widget), (GDestroyNotify) g_object_unref)


/***************** Start of help/about dialog box ********************/

/*! \brief Create the about dialog and show it
 *  \par Function Description
 *  This function creates the about dialog.
 */
void about_dialog (GschemToplevel *w_current)
{
  char *version_string;
  char *logo_file;
  GdkPixbuf *logo;
  GError *error = NULL;

  version_string = g_strdup_printf (_("%s (g%.7s)"),
                                    PACKAGE_DOTTED_VERSION,
                                    PACKAGE_GIT_COMMIT);

  logo_file = g_strconcat (w_current->toplevel->bitmap_directory,
                           G_DIR_SEPARATOR_S, "gschem-about-logo.png", NULL);

  logo = gdk_pixbuf_new_from_file (logo_file, &error);
  g_free (logo_file);

  if (error != NULL) {
    g_assert (logo == NULL);
    s_log_message ("Could not load image at file: %s\n%s\n",
                   logo_file, error->message);
    g_error_free (error);
  }

  gtk_show_about_dialog (
      GTK_WINDOW (w_current->main_window),
      "version",        version_string,
      "logo",           logo,
      "title",          _("About gschem"),
      "comments",       _("gEDA: GPL Electronic Design Automation"),
      "copyright",
      /* TRANSLATORS: "ChangeLog" is a literal filename; please don't translate it. */
      _("Copyright © 1998-2012 Ales Hvezda"
        " <ahvezda@geda.seul.org>\n"
        "Copyright © 1998-2012 gEDA Contributors"
        " (see ChangeLog for details)"),
      "website",        "http://geda-project.org/",
      NULL);

  g_free (version_string);
  g_object_unref (logo);
}

/***************** End of help/about dialog box *********************/
