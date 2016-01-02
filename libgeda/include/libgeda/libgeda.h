/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's Library
 * Copyright (C) 1998-2012 Ales Hvezda
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA.
 */

#ifndef LIBGEDA_H
#define LIBGEDA_H

#include <glib.h>

#include <stdio.h>
#include <libguile.h>
#include <gdk-pixbuf/gdk-pixbuf.h>

#include <libgeda/defines.h>

#include <libgeda/geda_color.h>
#include <libgeda/geda_color_map.h>
#include <libgeda/geda_fill_type.h>
#include <libgeda/geda_line_type.h>
#include <libgeda/geda_line_cap_type.h>
#include <libgeda/geda_point.h>
#include <libgeda/geda_string.h>

#include <libgeda/geda_angle.h>
#include <libgeda/geda_arc.h>
#include <libgeda/geda_bezier.h>
#include <libgeda/geda_bounds.h>
#include <libgeda/geda_box.h>
#include <libgeda/geda_circle.h>
#include <libgeda/geda_complex.h>
#include <libgeda/geda_coord.h>
#include <libgeda/geda_line.h>
#include <libgeda/geda_path.h>
#include <libgeda/geda_picture.h>
#include <libgeda/geda_text.h>
#include <libgeda/geda_transform.h>

#include <libgeda/geda_forward.h>

#include <libgeda/struct.h>
#include <libgeda/geda_object.h>
#include <libgeda/geda_object_list.h>
#include <libgeda/geda_page.h>
#include <libgeda/geda_toplevel.h>
#include <libgeda/geda_undo.h>

#include <libgeda/geda_arc_object.h>
#include <libgeda/geda_box_object.h>
#include <libgeda/geda_bus_object.h>
#include <libgeda/geda_circle_object.h>
#include <libgeda/geda_complex_object.h>
#include <libgeda/geda_line_object.h>
#include <libgeda/geda_net_object.h>
#include <libgeda/geda_path_object.h>
#include <libgeda/geda_picture_object.h>
#include <libgeda/geda_pin_object.h>
#include <libgeda/geda_text_object.h>

#include <libgeda/globals.h>
#include <libgeda/o_types.h>
#include <libgeda/funcs.h>
#include <libgeda/prototype.h>
#include <libgeda/edaconfig.h>
#include <libgeda/edaerrors.h>
#include <libgeda/geda_list.h>

#endif
