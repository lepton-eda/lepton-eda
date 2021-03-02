/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
 * Copyright (C) 2017-2021 Lepton EDA Contributors
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
/*! \file line_type.h
 */

/*! \brief The line type of objects such as arcs, boxes, circles, and lines
 *
 *  The numeric values of this enumeration are used inside files and must be
 *  preserved for compatibility.
 */
enum _LeptonLineType
{
  TYPE_SOLID,
  TYPE_DOTTED,
  TYPE_DASHED,
  TYPE_CENTER,
  TYPE_PHANTOM,
  TYPE_ERASE
};

typedef enum _LeptonLineType LeptonLineType;
