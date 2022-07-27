/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2022 Lepton EDA Contributors
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
#include <version.h>

#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <glib.h>

#include "gschem.h"

#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif



void
set_verbose_mode () {
  verbose_mode = TRUE;
  quiet_mode = FALSE;
}

void
set_quiet_mode () {
  quiet_mode = TRUE;
  verbose_mode = FALSE;
}


#ifdef ENABLE_GTK3
static GtkApplication *app = NULL;
#endif

gpointer
lepton_schematic_app ()
{
#ifdef ENABLE_GTK3
  return app;
#else
  return NULL;
#endif
}


/*! \brief Start lepton-schematic.
 *
 * The function initializes the structures of the program and runs
 * main gtk loop.
 */
int
lepton_schematic_run (gpointer activate)
{
#ifdef ENABLE_GTK3
  int status;

  app = gtk_application_new ("com.github.lepton_eda.lepton_schematic",
                             G_APPLICATION_FLAGS_NONE);

  g_signal_connect (app, "activate", G_CALLBACK (activate), NULL);

  status = g_application_run (G_APPLICATION (app), 0, NULL);
  g_object_unref (app);
  return status;
#else
  /* Run main GTK loop. */
  gtk_main ();
  return 0;
#endif
}
