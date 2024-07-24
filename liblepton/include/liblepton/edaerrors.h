/* Lepton EDA library
 * Copyright (C) 2011 gEDA Contributors
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

/*! Domain for GErrors originating in liblepton. */
#define EDA_ERROR eda_error_quark ()

/*! Error numbers for errors originating in liblepton. */
typedef enum {
  EDA_ERROR_SCHEME,   /* A Scheme error occurred */
  EDA_ERROR_RC_TWICE, /* Attempted to read a configuration file twice */
  EDA_ERROR_PARSE,    /* Schematic data could not be parsed. */
  EDA_ERROR_NOLIB,    /* A requested library resource was missing. */
  EDA_ERROR_LOOP,     /* The data model contains a circular dependency. */
  EDA_ERROR_UNKNOWN_ENCODING, /* Schematic data was not UTF-8-encoded. */
  EDA_ERROR_NUM_ERRORS
} EdaError;

GQuark eda_error_quark (void);

G_END_DECLS
