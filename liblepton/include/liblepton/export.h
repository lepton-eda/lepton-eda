/* Lepton EDA library
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

G_BEGIN_DECLS

int
cmd_export (int argc, char **argv);

void
export_config (void);

void
export_png ();

void
export_ps  ();

void
export_eps ();

void
export_pdf ();

void
export_svg ();

gboolean
export_parse_align (const gchar *scale);

gboolean
export_parse_scale (const gchar *scale);

gboolean
export_parse_layout (const gchar *layout);

gboolean
export_parse_margins (const gchar *margins);

gboolean
export_parse_paper (const gchar *paper);

gboolean
export_parse_size (const gchar *size);

void
export_list_paper_size_names();

G_END_DECLS
