/* Lepton EDA Schematic Load and Save utility
 * Copyright (C) 2002-2010 Ales Hvezda
 * Copyright (C) 2002-2015 gEDA Contributors
 * Copyright (C) 2017-2019 Lepton EDA Contributors
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

/*! \file
 *  \brief Global declarations
 *
 * Global declarations
 */

#include <config.h>

#include <stdio.h>

#include <liblepton/liblepton.h>

#include "../include/globals.h"
#include "../include/prototype.h"

/* command line arguments */
int verbose_mode=FALSE;
int quiet_mode=FALSE;
int embed_mode=FALSE;
int unembed_mode=FALSE;
