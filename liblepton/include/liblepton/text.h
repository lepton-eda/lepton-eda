/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2017 gEDA Contributors
 * Copyright (C) 2021-2025 Lepton EDA Contributors
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
/*! \file text.h
 */

typedef struct _LeptonText LeptonText;

struct _LeptonText
{
  int x, y;             /* World origin */

  char *string;         /* String to be displayed */
  int length;           /* The length of the string */
  int size;             /* Text character height in points */
  int alignment;        /* Position around the origin (see defines.h) */
  int angle;            /* Rotation angle */

  /* Attribute stuff. */
  const gchar *name;    /* Attribute name, not owned by _LeptonText */
  char *value;          /* Attribute value */
  int show;             /* What to display in GUI: name, value, or both */
  int visibility;       /* Whether the text is visible or hidden in GUI */
};

void
lepton_text_free (LeptonText *text);
