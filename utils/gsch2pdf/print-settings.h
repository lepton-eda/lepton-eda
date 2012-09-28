/* gEDA - GPL Electronic Design Automation
 * gschlas - gEDA Load and Save
 * Copyright (C) 2002-2012 Ales Hvezda
 * Copyright (C) 2002-2012 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

enum PrintOrientation
{
  PRINT_ORIENTATION_LANDSCAPE,
  PRINT_ORIENTATION_PORTRAIT,
  PRINT_ORIENTATION_OTHER
};

typedef enum PrintOrientation PrintOrientation;

#define DEFAULT_PAPER_WIDTH  11.0
#define DEFAULT_PAPER_HEIGHT  8.5

#define DEFAULT_PRINT_ORIENTATION PRINT_ORIENTATION_LANDSCAPE


typedef struct PrintSettings PrintSettings;



char*
print_settings_get_font(const PrintSettings *settings);

double
print_settings_get_junction_size_bus(const PrintSettings *settings);

double
print_settings_get_junction_size_net(const PrintSettings *settings);

double
print_settings_get_page_align_horizontal(const PrintSettings *settings);

double
print_settings_get_page_align_vertical(const PrintSettings *settings);

double
print_settings_get_page_margin_bottom(const PrintSettings *settings);

double
print_settings_get_page_margin_left(const PrintSettings *settings);

double
print_settings_get_page_margin_right(const PrintSettings *settings);

double
print_settings_get_page_margin_top(const PrintSettings *settings);

double
print_settings_get_page_height(const PrintSettings *settings);

double
print_settings_get_page_width(const PrintSettings *settings);

double
print_settings_get_paper_height(const PrintSettings *settings);

double
print_settings_get_paper_width(const PrintSettings *settings);

double
print_settings_get_print_height(const PrintSettings *settings);

PrintOrientation
print_settings_get_print_orientation(const PrintSettings *settings);

double
print_settings_get_print_width(const PrintSettings *settings);

PrintSettings*
print_settings_new();

void
print_settings_set_font(PrintSettings *settings, const char* string);

void
print_settings_set_junction_size_bus(PrintSettings *settings, double size);

void
print_settings_set_junction_size_net(PrintSettings *settings, double size);

void
print_settings_set_page_align_horizontal(PrintSettings *settings, double align);

void
print_settings_set_page_align_vertical(PrintSettings *settings, double align);

void
print_settings_set_page_margin_bottom(PrintSettings *settings, double margin);

void
print_settings_set_page_margin_left(PrintSettings *settings, double margin);

void
print_settings_set_page_margin_right(PrintSettings *settings, double margin);

void
print_settings_set_page_margin_top(PrintSettings *settings, double margin);

void
print_settings_set_paper_height(PrintSettings *settings, double height);

void
print_settings_set_paper_width(PrintSettings *settings, double width);

void
print_settings_set_print_orientation(PrintSettings *settings, PrintOrientation orientation);

