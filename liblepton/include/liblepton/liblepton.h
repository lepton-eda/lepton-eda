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

#include <liblepton/color.h>
#include <liblepton/fill.h>
#include <liblepton/stroke.h>
#include <liblepton/point.h>
#include <liblepton/str.h>

#include <liblepton/angle.h>
#include <liblepton/arc.h>
#include <liblepton/bezier.h>
#include <liblepton/bounds.h>
#include <liblepton/box.h>
#include <liblepton/circle.h>
#include <liblepton/component.h>
#include <liblepton/coord.h>
#include <liblepton/line.h>
#include <liblepton/path.h>
#include <liblepton/picture.h>
#include <liblepton/text.h>
#include <liblepton/transform.h>

#include <liblepton/forward.h>

#include <liblepton/struct.h>
#include <liblepton/object.h>
#include <liblepton/object_list.h>
#include <liblepton/page.h>
#include <liblepton/toplevel.h>
#include <liblepton/undo.h>

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

#include <liblepton/export.h>

G_END_DECLS

#include <liblepton/edaconfig.h>
#include <liblepton/edaerrors.h>
#include <liblepton/edapaths.h>
#include <liblepton/list.h>

#endif
