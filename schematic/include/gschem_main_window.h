/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
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
 * \file gschem_main_window.h
 *
 * \brief
 */

#define GSCHEM_TYPE_MAIN_WINDOW           (gschem_main_window_get_type())
#define GSCHEM_MAIN_WINDOW(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), GSCHEM_TYPE_MAIN_WINDOW, GschemMainWindow))
#define GSCHEM_MAIN_WINDOW_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  GSCHEM_TYPE_MAIN_WINDOW, GschemMainWindowClass))
#define GSCHEM_IS_MAIN_WINDOW(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GSCHEM_TYPE_MAIN_WINDOW))
#define GSCHEM_MAIN_WINDOW_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), GSCHEM_TYPE_MAIN_WINDOW, GschemMainWindowClass))

typedef struct _GschemMainWindowClass GschemMainWindowClass;
typedef struct _GschemMainWindow GschemMainWindow;

struct _GschemMainWindowClass
{
  GtkWindowClass parent_class;
};

struct _GschemMainWindow
{
  GtkWindow parent;
};

GType
gschem_main_window_get_type();

GschemMainWindow*
gschem_main_window_new ();
