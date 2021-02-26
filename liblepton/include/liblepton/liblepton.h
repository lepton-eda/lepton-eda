/* Lepton EDA library
 * Copyright (C) 1998-2012 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
 * Copyright (C) 2017-2021 Lepton EDA Contributors
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#ifndef LIBLEPTON_H
#define LIBLEPTON_H

#include <glib.h>

#include <stdio.h>
#include <libguile.h>
#include <gdk-pixbuf/gdk-pixbuf.h>

G_BEGIN_DECLS

#include <liblepton/defines.h>

#include <liblepton/geda_color.h>
#include <liblepton/geda_fill_type.h>
#include <liblepton/geda_line_type.h>
#include <liblepton/geda_line_cap_type.h>
#include <liblepton/point.h>
#include <liblepton/geda_string.h>

#include <liblepton/angle.h>
#include <liblepton/arc.h>
#include <liblepton/geda_bezier.h>
#include <liblepton/bounds.h>
#include <liblepton/box.h>
#include <liblepton/geda_circle.h>
#include <liblepton/geda_component.h>
#include <liblepton/geda_coord.h>
#include <liblepton/geda_line.h>
#include <liblepton/geda_path.h>
#include <liblepton/geda_picture.h>
#include <liblepton/geda_text.h>
#include <liblepton/geda_transform.h>

#include <liblepton/geda_forward.h>

#include <liblepton/struct.h>
#include <liblepton/object.h>
#include <liblepton/object_list.h>
#include <liblepton/geda_page.h>
#include <liblepton/geda_toplevel.h>
#include <liblepton/geda_undo.h>

#include <liblepton/arc_object.h>
#include <liblepton/box_object.h>
#include <liblepton/bus_object.h>
#include <liblepton/circle_object.h>
#include <liblepton/component_object.h>
#include <liblepton/line_object.h>
#include <liblepton/net_object.h>
#include <liblepton/path_object.h>
#include <liblepton/picture_object.h>
#include <liblepton/pin_object.h>
#include <liblepton/text_object.h>

#include <liblepton/globals.h>
#include <liblepton/o_types.h>
#include <liblepton/funcs.h>
#include <liblepton/prototype.h>

G_END_DECLS

#include <liblepton/edaconfig.h>
#include <liblepton/edaerrors.h>
#include <liblepton/edapaths.h>
#include <liblepton/geda_list.h>

#endif
