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

/* Initialise the Scheme API. */
void edascm_init ();

/* Initialize #LeptonToplevel fluid. */
void
lepton_init_toplevel_fluid (SCM fluid);

/* Get the value of the #LeptonToplevel fluid in C. */
LeptonToplevel *edascm_c_current_toplevel ();

/* Set the #LeptonToplevel fluid in the current dynamic context. */
void edascm_dynwind_toplevel (LeptonToplevel *toplevel);

/* Get the value of the #LeptonToplevel fluid. */
SCM
edascm_with_toplevel (SCM toplevel, SCM thunk);

/* Create a Guile value from a page structure. */
SCM edascm_from_page (LeptonPage *page);

/* Create a Guile value from an object structure. */
SCM edascm_from_object (LeptonObject *object);

/* Retrieve a page structure from a Guile value. */
LeptonPage *edascm_to_page (SCM smob);

/* Retrieve an object structure from a Guile value. */
LeptonObject *edascm_to_object (SCM smob);

/* Test if smob is a gEDA page. */
int edascm_is_page (SCM smob);

/* Test if smob is a gEDA object. */
int edascm_is_object (SCM smob);

/* Set whether a gEDA object may be garbage collected. */
void edascm_c_set_gc (SCM smob, int gc);

G_END_DECLS
