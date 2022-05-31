/* Lepton EDA library - Scheme API
 * Copyright (C) 2010-2014 Peter Brett <peter@peter-b.co.uk>
 * Copyright (C) 2017-2022 Lepton EDA Contributors
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

/*!
 * \file libleptonguile.h
 * \ingroup guile_c_iface
 * Scheme API public declarations and definitions.
 * \warning Don't include from liblepton.h: should only be included
 * by source files that need to use the Scheme API.
 */

/* Initialize #LeptonToplevel fluid. */
void
lepton_init_toplevel_fluid (SCM fluid);

/* Set the #LeptonToplevel fluid in the current dynamic context. */
void edascm_dynwind_toplevel (LeptonToplevel *toplevel);

G_END_DECLS
