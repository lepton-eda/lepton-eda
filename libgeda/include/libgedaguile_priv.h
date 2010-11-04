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
 * \file libgedaguile_priv.h
 * Scheme API private declarations and definitions.
 * \warning Don't include from libgeda_priv.h: should only be included
 * by Scheme API source files.
 */

#include <libgeda/libgedaguile.h>

void edascm_init_smob ();
void edascm_init_toplevel ();

/* ---------------------------------------- */

/* Macros and constants for working with the geda smob type. These are
 * for the convenience of the other C functions used by the Scheme
 * API. */

/*! The tag used to identify gEDA data structures in Scheme. */
extern scm_t_bits geda_smob_tag;

/*! The flags used to determine which C structure a smob contains. */
enum geda_smob_flags {
  GEDA_SMOB_TOPLEVEL = 0,
  GEDA_SMOB_PAGE = 1,
  GEDA_SMOB_OBJECT = 2,
  GEDA_SMOB_TYPE_MASK = 0xf,
  GEDA_SMOB_GC_FLAG = 0x100,
};

/*! Retrieve the type flags for a gEDA smob. */
#define EDASCM_SMOB_TYPE(x) (SCM_SMOB_FLAGS (x) & GEDA_SMOB_TYPE_MASK)

/*! \brief Test the type of a gEDA smob.
 * \par Macro Description
 * Returns non-zero if \a x is a gEDA smob and the type flags of \a x
 * match \a type.
 */
#define EDASCM_SMOB_TYPEP(x, type) \
  (SCM_SMOB_PREDICATE (geda_smob_tag, x) && (EDASCM_SMOB_TYPE (x) == type))

/*! \brief Test whether a gEDA smob is valid.
 * \par Macro Description
 * Returns non-zero if \a x is a gEDA smob and the pointer it contains
 * is valid.
 */
#define EDASCM_SMOB_VALIDP(x) \
  (SCM_SMOB_PREDICATE (geda_smob_tag, x) && ((void *)SCM_SMOB_DATA (x) != NULL))

/*! \brief Assert that a gEDA smob is valid.
 * \par Macro Description
 * Throw an error if assertions are enabled and \a x is invalid.
 */
#ifdef NDEBUG
#  define EDASCM_ASSERT_SMOB_VALID(x)
#else
#  define EDASCM_ASSERT_SMOB_VALID(x) \
  do { if (!EDASCM_SMOB_VALIDP(x)) {                                    \
      scm_misc_error (NULL, "Found invalid gEDA smob ~S", scm_list_1 (x)); \
    } } while (0)
#endif

/* Create a Guile value from a TOPLEVEL structure. */
SCM edascm_from_toplevel (TOPLEVEL *toplevel);

/*! Tests whether a Scheme value is a TOPLEVEL smob. */
#define EDASCM_TOPLEVELP(x) EDASCM_SMOB_TYPEP(x, GEDA_SMOB_TOPLEVEL)

/*! Tests whether a Scheme value is a PAGE smob. */
#define EDASCM_PAGEP(x) EDASCM_SMOB_TYPEP(x, GEDA_SMOB_PAGE)

/*! Tests whether a Scheme value is an OBJECT smob. */
#define EDASCM_OBJECTP(x) EDASCM_SMOB_TYPEP(x, GEDA_SMOB_OBJECT)

/*!
 * \brief Test whether a structure may be garbage-collected
 * \par Macro Description
 * Tests whether the C structure contained by the smob \a x is only
 * referenced by Scheme code, and thus can be free()'d when \a x is
 * garbage-collected.
 */
#define EDASCM_SMOB_GCP(x) \
  (SCM_SMOB_PREDICATE (geda_smob_tag, x) && ((SCM_SMOB_FLAGS (x) & GEDA_SMOB_GC_FLAG) != 0))

/*!
 * \brief Set whether a structure may be garbage-collected
 * \par Macro Description
 * Set whether the structure contained by the smob \a x is only
 * referenced by Scheme code, and thus should be free()'d when \a x is
 * garbage-collected.
 *
 * \param x  Smob to modify.
 * \param gc Non-zero if \a x should be garbage-collected.
 */
#define EDASCM_SMOB_SET_GC(x, gc) \
  SCM_SET_SMOB_FLAGS (x, gc ? (SCM_SMOB_FLAGS (x) | GEDA_SMOB_GC_FLAG) \
                            : (SCM_SMOB_FLAGS (x) & ~GEDA_SMOB_GC_FLAG))

/* Set whether a gEDA object may be garbage collected. */
void edascm_c_set_gc (SCM smob, int gc);

/* ---------------------------------------- */

GList *edascm_to_object_glist (SCM objs, const char *subr);
SCM edascm_from_object_glist (const GList *objs);
