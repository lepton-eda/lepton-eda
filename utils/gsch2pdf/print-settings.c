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

#include <config.h>

#include <stdio.h>
#include <sys/stat.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <libgeda/libgeda.h>

#include "../include/globals.h"
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#include "print-settings.h"



struct PrintSettings
{
	double           paper_width;
	double           paper_height;

	double           margin_bottom;
	double           margin_left;
	double           margin_right;
	double           margin_top;

	double           align_horizontal;
	double           align_vertical;

	char             *font_string;

        double           junction_size_bus;
        double           junction_size_net;

	PrintOrientation orientation;
};



/*! \brief Get a string representation of the selected font
 *
 *  \param [in] The settings containing the selected font
 *  \return The font string. Do not free this string.
 */
char*
print_settings_get_font(const PrintSettings *settings)
{
  char *string = "Sans";

  if ((settings != NULL) && (settings->font_string != NULL)) {
    string = settings->font_string;
  }

  return string;
}



/*! \brief Get the diameter of a junction used for a bus
 *
 *  \param [in] settings The settings containing the junction size
 *  \return The junction size for busses
 */
double
print_settings_get_junction_size_bus(const PrintSettings *settings)
{
  double size = JUNCTION_CUE_SIZE_BUS;

  if (settings != NULL) {
    size = settings->junction_size_bus;
  }

  return size;
}


/*! \brief Get the diameter of a junction used for nets
 *
 *  \param [in] settings The settings containing the junction size
 *  \return The junction size for nets
 */
double
print_settings_get_junction_size_net(const PrintSettings *settings)
{
  double size = JUNCTION_CUE_SIZE_NET;

  if (settings != NULL) {
    size = settings->junction_size_net;
  }

  return size;
}


/*! \brief Get the horizontal alignment
 *
 *  If the schematic is smaller than the print area horizontaly, this
 *  setting controls justification of the schematic on the prined page.
 *
 *  Place the printed area against the left margin = 0.0
 *  Center the printed area between the left and right margins = 0.5
 *  Place the printed area against the right margin = 1.0
 *
 *  \param [in] settings The settings containing the horizontal alignment
 *  \return The horizontal alignment.
 */
double
print_settings_get_page_align_horizontal(const PrintSettings *settings)
{
  double align = 0.5;

  if (settings != NULL) {
    align = settings->align_horizontal;
  }

  return align;
}


/*! \brief Get the vertical alignment
 *
 *  If the schematic is smaller than the print area vertically, this
 *  setting controls justification of the schematic on the prined page.
 *
 *  Place the printed area against the bottom margin = 0.0
 *  Center the printed area between the top and bottom margins = 0.5
 *  Place the printed area against the top margin = 1.0
 *
 *  \param [in] settings The settings containing the vertical alignment
 *  \return The new vertical alignment.
 */
double
print_settings_get_page_align_vertical(const PrintSettings *settings)
{
  double align = 0.5;

  if (settings != NULL) {
    align = settings->align_vertical;
  }

  return align;
}



/*! \brief Get the bottom margin
 *
 *  \param [in] settings The settings containing the bottom margin
 *  \return The bottom margin
 */
double
print_settings_get_page_margin_bottom(const PrintSettings *settings)
{
  double margin = 0.75;

  if (settings != NULL) {
    margin = settings->margin_bottom;
  }

  return margin;
}



/*! \brief Get the left margin
 *
 *  \param [in] settings The settings containing the left margin
 *  \return The left margin
 */
double
print_settings_get_page_margin_left(const PrintSettings *settings)
{
  double margin = 0.75;

  if (settings != NULL) {
    margin = settings->margin_left;
  }

  return margin;
}



/*! \brief Get the right margin
 *
 *  \param [in] settings The settings containing the right margin
 *  \return The right margin
 */
double
print_settings_get_page_margin_right(const PrintSettings *settings)
{
  double margin = 0.75;

  if (settings != NULL) {
    margin = settings->margin_right;
  }

  return margin;
}



/*! \brief Get the top margin
 *
 *  \param [in] settings The settings containing the top margin
 *  \return The top margin
 */
double
print_settings_get_page_margin_top(const PrintSettings *settings)
{
  double margin = 0.75;

  if (settings != NULL) {
    margin = settings->margin_top;
  }

  return margin;
}



/*! \brief Get the height of the printed page
 *
 *  Unlike the paper dimensions, this dimension takes the orientation of
 *  the paper into account. If portrait, it will be the longer
 *  paper dimension. In landscape, it will be the shorter paper dimension.
 *  For others, it will match the physical paper height.
 *
 *  \param [in] settings The settings
 *  \return The width of the printed page
 */
double
print_settings_get_page_height(const PrintSettings *settings)
{
  double height = DEFAULT_PAPER_HEIGHT;

  if (settings != NULL) {
    if (settings->orientation == PRINT_ORIENTATION_LANDSCAPE) {
      if (settings->paper_width > settings->paper_height) {
        height = settings->paper_height;
      } else {
        height = settings->paper_width;
      }
    } else if (settings->orientation == PRINT_ORIENTATION_PORTRAIT) {
      if (settings->paper_width > settings->paper_height) {
        height = settings->paper_width;
      } else {
        height = settings->paper_height;
      }
    } else {
      height = settings->paper_height;
    }
  }

  return height;
}



/*! \brief Get the width of the printed page
 *
 *  Unlike the paper dimensions, this dimension takes the orientation of
 *  the paper into account. If portrait, it will be the shorter
 *  paper dimension. In landscape, it will be the longer paper dimension.
 *  For others, it will match the physical paper width.
 *
 *  \param [in] settings The settings
 *  \return The width of the printed page
 */
double
print_settings_get_page_width(const PrintSettings *settings)
{
  double width = DEFAULT_PAPER_WIDTH;

  if (settings != NULL) {
    if (settings->orientation == PRINT_ORIENTATION_LANDSCAPE) {
      if (settings->paper_width > settings->paper_height) {
        width = settings->paper_width;
      } else {
        width = settings->paper_height;
      }
    } else if (settings->orientation == PRINT_ORIENTATION_PORTRAIT) {
      if (settings->paper_width > settings->paper_height) {
        width = settings->paper_height;
      } else {
        width = settings->paper_width;
      }
    } else {
      width = settings->paper_width;
    }
  }

  return width;
}



/*! \brief Get the height of the physical paper
 *
 *  \param [in] The settings containing the paper height
 *  \return The height of the physical paper
 */
double
print_settings_get_paper_height(const PrintSettings *settings)
{
  double height = DEFAULT_PAPER_HEIGHT;

  if (settings != NULL) {
    height = settings->paper_height;
  }

  return height;
}



/*! \brief Get the width of the physical paper
 *
 *  \param [in] The settings containing the paper width
 *  \return The width of the physical paper.
 */
double
print_settings_get_paper_width(const PrintSettings *settings)
{
  double width = DEFAULT_PAPER_WIDTH;

  if (settings != NULL) {
    width = settings->paper_width;
  }

  return width;
}



/*! \brief Get the height of the printable area
 *
 *  \param [in] settings The settings
 *  \return The height of the printable area.
 */
double
print_settings_get_print_height(const PrintSettings *settings)
{
  double height = print_settings_get_page_height(settings);

  height -= print_settings_get_page_margin_top(settings);
  height -= print_settings_get_page_margin_bottom(settings);

  if (height < 0.0) {
    height = 0.0;
  }

  return height;
}



/*! \brief Get the print orientation
 *
 *  \param [in] The settings containing the print orientation
 *  \return The print orientation
 */
PrintOrientation
print_settings_get_print_orientation(const PrintSettings *settings)
{
  PrintOrientation orientation = DEFAULT_PRINT_ORIENTATION;

  if (settings != NULL) {
    orientation = settings->orientation;
  }

  return orientation;
}



/*! \brief Get the width of the printable area
 *
 *  \param [in] settings The settings
 *  \return The width of the printable area.
 */
double
print_settings_get_print_width(const PrintSettings *settings)
{
  double width = print_settings_get_page_width(settings);

  width -= print_settings_get_page_margin_left(settings);
  width -= print_settings_get_page_margin_right(settings);

  if (width < 0.0) {
    width = 0.0;
  }

  return width;
}



/*! \brief Create a new print settings object
 *
 *  \return The new print settings object.
 */
PrintSettings*
print_settings_new()
{
  PrintSettings *settings = g_new0(PrintSettings, 1);

  settings->paper_height = DEFAULT_PAPER_HEIGHT;
  settings->paper_width  = DEFAULT_PAPER_WIDTH;
  settings->orientation  = DEFAULT_PRINT_ORIENTATION;

  settings->margin_left = 0.75;
  settings->margin_top = 0.75;
  settings->margin_right = 0.75;
  settings->margin_bottom = 0.75;

  settings->align_horizontal = 0.5;
  settings->align_vertical = 0.5;

  settings->orientation = PRINT_ORIENTATION_LANDSCAPE;

  settings->junction_size_bus = JUNCTION_CUE_SIZE_BUS;
  settings->junction_size_net = JUNCTION_CUE_SIZE_NET;

  return settings;
}



/*! \brief Set the font
 *
 *  \param [in] settings The settings containing the font string
 *  \param [in] string The font string
 */
void
print_settings_set_font(PrintSettings *settings, const char *string)
{
  if (settings != NULL) {
    if (settings->font_string != NULL) {
      g_free(settings->font_string);
    }
    settings->font_string = g_strdup(string);
  }
}



/*! \brief Set the diameter of a junction used for a bus
 *
 *  \param [in,out] settings The settings containing the junction size
 *  \param [in] size The junction size for busses
 */
void
print_settings_set_junction_size_bus(PrintSettings *settings, double size)
{
  if (settings != NULL) {
    if (size > 0.0) {
      settings->junction_size_bus = size;
    } else {
      settings->junction_size_bus = 0.0;
    }
  }
}


/*! \brief Set the diameter of a junction used for nets
 *
 *  \param [in,out] settings The settings containing the junction size
 *  \param [in] size The junction size for nets
 */
void
print_settings_set_junction_size_net(PrintSettings *settings, double size)
{
  if (settings != NULL) {
    if (size > 0.0) {
      settings->junction_size_net = size;
    } else {
      settings->junction_size_net = 0.0;
    }
  }
}



/*! \brief Set the horizontal alignment
 *
 *  If the schematic is smaller than the print area horizontaly, this
 *  setting controls justification of the schematic on the prined page.
 *
 *  Place the printed area against the left margin = 0.0
 *  Center the printed area between the left and right margins = 0.5
 *  Place the printed area against the right margin = 1.0
 *
 *  \param [in] settings The settings containing the horizontal alignment
 *  \param [in] align The vertical alignment.
 */
void
print_settings_set_page_align_horizontal(PrintSettings *settings, double align)
{
  if (settings != NULL) {
    if (align < 0.0) {
      settings->align_horizontal = 0.0;
    } else if (align > 1.0) {
      settings->align_horizontal = 1.0;
    } else {
      settings->align_horizontal = align;
    }
  }
}



/*! \brief Set the vertical alignment
 *
 *  If the schematic is smaller than the print area vertically, this
 *  setting controls justification of the schematic on the prined page.
 *
 *  Place the printed area against the bottom margin = 0.0
 *  Center the printed area between the top and bottom margins = 0.5
 *  Place the printed area against the top margin = 1.0
 *
 *  \param [in, out] settings The settings to contain the vertical alignment
 *  \param [in] align The new vertical alignment.
 */
void
print_settings_set_page_align_vertical(PrintSettings *settings, double align)
{
  if (settings != NULL) {
    if (align < 0.0) {
      settings->align_vertical = 0.0;
    } else if (align > 1.0) {
      settings->align_vertical = 1.0;
    } else {
      settings->align_vertical = align;
    }
  }
}



/*! \brief Set the bottom margin
 *
 *  \param [in,out] settings The settings to contain the bottom margin
 *  \param [in] margin The new bottom margin of the page, in inches.
 */
void
print_settings_set_page_margin_bottom(PrintSettings *settings, double margin)
{
  if (settings != NULL) {
    if (margin < 0.0) {
      settings->margin_bottom = 0.0;
    } else {
      settings->margin_bottom = margin;
    }
  }
}



/*! \brief Set the left margin
 *
 *  \param [in,out] settings The settings to contain the left margin
 *  \param [in] margin The new left margin of the page, in inches.
 */
void
print_settings_set_page_margin_left(PrintSettings *settings, double margin)
{
  if (settings != NULL) {
    if (margin < 0.0) {
      settings->margin_left = 0.0;
    } else {
      settings->margin_left = margin;
    }
  }
}



/*! \brief Set the right margin
 *
 *  \param [in,out] settings The settings to contain the right margin
 *  \param [in] margin The new right margin of the page, in inches.
 */
void
print_settings_set_page_margin_right(PrintSettings *settings, double margin)
{
  if (settings != NULL) {
    if (margin < 0.0) {
      settings->margin_right = 0.0;
    } else {
      settings->margin_right = margin;
    }
  }
}



/*! \brief Set the top margin
 *
 *  \param [in,out] settings The settings to contain the top margin
 *  \param [in] margin The new top margin of the page, in inches.
 */
void
print_settings_set_page_margin_top(PrintSettings *settings, double margin)
{
  if (settings != NULL) {
    if (margin < 0.0) {
      settings->margin_top = 0.0;
    } else {
      settings->margin_top = margin;
    }
  }
}


/*! \brief Set the physical height of the paper
 *
 *  \param [in,out] settings The settings to contain the paper height
 *  \param [in] height The new physical height of the paper, in inches.
 */
void
print_settings_set_paper_height(PrintSettings *settings, double height)
{
  if (settings != NULL) {
    if (height < 0.0) {
      settings->paper_height = 0.0;
    } else {
      settings->paper_height = height;
    }
  }
}



/*! \brief Set the physical width of the paper
 *
 *  \param [in,out] settings The settings to contain the paper width
 *  \param [in] width The new physical width of the paper, in inches.
 */
void
print_settings_set_paper_width(PrintSettings *settings, double width)
{
  if (settings != NULL) {
    if (width < 0.0) {
      settings->paper_width = 0.0;
    } else {
      settings->paper_width = width;
    }
  }
}



/*! \brief Set the orientation of the print
 *
 *  Sets how to orient the physical paper for printing. The page width and height will
 *  reflect the value of this setting.
 *
 *  \param [in,out] settings The settings contain the print orientation
 *  \param [in] orientation The new print orientation
 */
void
print_settings_set_print_orientation(PrintSettings *settings, PrintOrientation orientation)
{
  if (settings != NULL) {
    settings->orientation = orientation;
  }
}
