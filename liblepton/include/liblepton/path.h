/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2021 Lepton EDA Contributors
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
/*! \file path.h
 */

G_BEGIN_DECLS

typedef struct st_path_section LeptonPathSection;

typedef struct st_path LeptonPath;

typedef enum
{
    PATH_MOVETO,
    PATH_MOVETO_OPEN,
    PATH_CURVETO,
    PATH_LINETO,
    PATH_END
} PATH_CODE;

struct st_path_section
{
  PATH_CODE code;
  int x1;
  int y1;
  int x2;
  int y2;
  int x3;
  int y3;
};

struct st_path
{
  LeptonPathSection *sections; /* Bezier path segments  */
  int num_sections;       /* Number with data      */
  int num_sections_max;   /* Number allocated      */
};

void
lepton_path_free (LeptonPath *path);


PATH_CODE
lepton_path_section_get_code (LeptonPathSection *section);

int
lepton_path_section_get_x1 (LeptonPathSection *section);

int
lepton_path_section_get_y1 (LeptonPathSection *section);

int
lepton_path_section_get_x2 (LeptonPathSection *section);

int
lepton_path_section_get_y2 (LeptonPathSection *section);

int
lepton_path_section_get_x3 (LeptonPathSection *section);

int
lepton_path_section_get_y3 (LeptonPathSection *section);

int
lepton_path_section_code_from_string (char *s);

const char*
lepton_path_section_code_to_string (int code);

LeptonPath*
lepton_path_parse (const char *path_str);

double
lepton_path_shortest_distance (LeptonPath *path,
                               int x,
                               int y,
                               int solid);
char*
lepton_path_string_from_path (const LeptonPath *path);

int
lepton_path_to_polygon (LeptonPath *path,
                        GArray *points);

G_END_DECLS
