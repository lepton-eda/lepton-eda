/* Lepton EDA Schematic Capture
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors (see ChangeLog for details)
 * Copyright (C) 2017 dmn <graahnul.grom@gmail.com>
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
 * \file x_widgets.h
 *
 * \brief Widgets management
 */

#ifndef GSCHEM_INCLUDE_X_WIDGETS_H_
#define GSCHEM_INCLUDE_X_WIDGETS_H_


void x_widgets_show_options (GschemToplevel* w_current);
void x_widgets_show_text_properties (GschemToplevel* w_current);
void x_widgets_show_object_properties (GschemToplevel* w_current);
void x_widgets_show_log (GschemToplevel* w_current);
void x_widgets_show_find_text_state (GschemToplevel* w_current);

#endif /* GSCHEM_INCLUDE_X_WIDGETS_H_ */
