/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library - Scheme API
 * Copyright (C) 2010 Peter Brett <peter@peter-b.co.uk>
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
 */

/*!
 * \file libgedaguile.h
 * Scheme API public declarations and definitions.
 * \warning Don't include from libgeda.h: should only be included
 * by source files that need to use the Scheme API.
 */

/* Initialise the Scheme API. */
void edascm_init ();

/* Get the value of the #TOPLEVEL fluid. */
TOPLEVEL *edascm_c_current_toplevel ();

/* Set the #TOPLEVEL fluid in the current dynamic context. */
void edascm_dynwind_toplevel (TOPLEVEL *toplevel);

/* Set the current #TOPLEVEL temporarily. */
SCM edascm_c_with_toplevel (TOPLEVEL *toplevel, SCM (*func)(void *),
                            void *user_data);

/* Create a Guile value from a page structure. */
SCM edascm_from_page (PAGE *page);

/* Create a Guile value from an object structure. */
SCM edascm_from_object (OBJECT *object);

/* Retrieve a page structure from a Guile value. */
PAGE *edascm_to_page (SCM smob);

/* Retrieve an object structure from a Guile value. */
OBJECT *edascm_to_object (SCM smob);
