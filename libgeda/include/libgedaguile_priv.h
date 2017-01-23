/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library - Scheme API
 * Copyright (C) 2010-2013 Peter Brett <peter@peter-b.co.uk>
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

/*! \defgroup guile_c_iface gEDA Scheme API: C interface
 * \brief Interface to the gEDA Scheme API for programs written in C.
 *
 * This module contains a variety of functions for use in applications
 * that use libgeda and which need to make use of or extend the gEDA
 * Scheme API.
 *
 * To initialise the API, edascm_init() needs to be called before any
 * Scheme code is executed or any of the other functions listed in
 * this module are called.  Normally, this will be called
 * automatically by libgeda_init().
 *
 * The Scheme API requires a libgeda #TOPLEVEL context to be available
 * at any given time.  The #TOPLEVEL can be set on a per-thread basis
 * using the edascm_dynwind_toplevel() or edascm_c_with_toplevel()
 * functions.  For example:
 *
 * \code
 * static SCM worker (void *user_data)
 * {
 *   // ...run Scheme code and/or call Scheme API C functions...
 * }
 *
 * void myfunc(TOPLEVEL *toplevel)
 * {
 *   void *mydata;
 *
 *   // ...set up mydata... //
 *
 *   // Set current toplevel using edascm_c_with_toplevel()
 *   edascm_c_with_toplevel (toplevel, worker, mydata);
 *
 *   // Set current toplevel using dynamic wind
 *   scm_dynwind_begin (0);
 *   edascm_dynwind_toplevel (toplevel);
 *   worker (mydata);
 *   // ...run Scheme code and/or call Scheme API C functions...
 *   scm_dynwind_end ();
 * }
 * \endcode
 *
 * For more information on dynamic wind, see the Guile Reference
 * Manual.
 *
 * The remaining functions in this module allow you to convert gEDA
 * #OBJECT and #PAGE structures to and from Scheme values ("smobs").
 *
 * When an #OBJECT is created by Scheme code, it is permitted to be
 * garbage-collected if all references to it are lost; this is an
 * important part of allowing Scheme programmers to write efficient
 * code.  However, because #OBJECT instances are not reference
 * counted, each Scheme object that contains an #OBJECT has a flag
 * that indicates whether it is wholly owned by Scheme or whether
 * there are any remaining references to it from C code.  If you use
 * edascm_from_object() to create a Scheme value for an #OBJECT that
 * has no remaining references from other C structures, you should use
 * edascm_c_set_gc() to mark it as garbage-collectable.
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
void edascm_init_object ();
void edascm_init_complex ();
void edascm_init_page ();
void edascm_init_attrib ();
void edascm_init_os ();
void edascm_init_config ();
void edascm_init_closure (void);
void edascm_init_log (void);
void edascm_init_deprecated ();

/* ---------------------------------------- */

/*! Test whether a Scheme value has a defined value */
#define edascm_is_defined(x) (!scm_is_eq((x), SCM_UNDEFINED))

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
  GEDA_SMOB_CONFIG = 3,
  GEDA_SMOB_CLOSURE = 4,
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

/*! Tests whether a Scheme value is an EdaConfig smob. */
#define EDASCM_CONFIGP(x) EDASCM_SMOB_TYPEP(x, GEDA_SMOB_CONFIG)

/*! Tests whether a Scheme value is a C closure smob. */
#define EDASCM_CLOSUREP(x) EDASCM_SMOB_TYPEP(x, GEDA_SMOB_CLOSURE)

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

/* ---------------------------------------- */

GList *edascm_to_object_glist (SCM objs, const char *subr)
  G_GNUC_WARN_UNUSED_RESULT;
SCM edascm_from_object_glist (const GList *objs);
int edascm_is_object_type (SCM smob, int type);


/*! \brief Flag an object's page as having been changed. */
extern void o_page_changed (TOPLEVEL *t, OBJECT *o);

/* ---------------------------------------- */

extern SCM edascm_object_state_sym;

/* ---------------------------------------- */

SCM edascm_from_closure (SCM (*func)(SCM, gpointer), gpointer user_data);
