/* Lepton EDA Schematic Capture
 * Copyright (C) 2018 dmn <graahnul.grom@gmail.com>
 * Copyright (C) 2018-2022 Lepton EDA Contributors
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

#ifndef __X_TABS_H__
#define __X_TABS_H__

/* --------------------------------------------------------
 *
 * Tab data:
 *
 */

/*!
 *  \struct _TabInfo
 *  \brief Represents relationship between page, page view, tab widget
 */
struct _TabInfo
{

  gint            ndx_;   /* just for debugging; will be removed */

  LeptonPage*     page_;
  GschemPageView* pview_;
  GtkWidget*      wtab_;  /* tab widget, i.e. scrolled wnd, parent of pview_ */

  GschemToplevel* tl_;

};

typedef struct _TabInfo TabInfo;

#endif /* __X_TABS_H__ */
