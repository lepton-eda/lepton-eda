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
 * \file scheme_smob.c
 * \brief Scheme representations of gEDA C structures
 *
 * In order for Scheme code to be able to manipulate libgeda data
 * structures, it is convenient for it to be able to get handles to
 * several of the different C structures that libgeda uses, in
 * particular #TOPLEVEL, #PAGE and #OBJECT.
 *
 * A particular issue is that, in principle, Guile can stash a
 * variable somewhere and only try and access it much later, possibly
 * after the underlying C structure has already been freed.
 *
 * In order to avoid this situation causing a segmentation fault, weak
 * references are used. In the case of #PAGE and #TOPLEVEL handles,
 * the usage is quite straightforward; Scheme code can never create or
 * destroy a #TOPLEVEL; and although a #PAGE can be created by Scheme
 * code, it must explicitly be destroyed if the Scheme code doesn't
 * want the #PAGE to hang around after it returns.
 *
 * #OBJECT handles are a more complex case. It's possible that Scheme
 * code may legitimately want to create an #OBJECT and do something
 * with it (or, similarly, pull an #OBJECT out of a #PAGE), without
 * needing to carefully keep track of the #OBJECT to avoid dropping it
 * on the floor. In that case, users should be able to rely on the
 * garbage collector.
 *
 * For that reason, an #OBJECT is marked to be destroyed by
 * garbage-collection in two cases:
 *
 * -# If they have been created by Scheme code, but not yet added to a
 *    PAGE.
 * -# If they have been removed from a #PAGE by Scheme code, but not
 *    yet re-added to a #PAGE.
 *
 * \sa weakref.c
 */

#include <config.h>

#include "libgeda_priv.h"
#include "libgedaguile_priv.h"

scm_t_bits geda_smob_tag;

/*! \brief Weak reference notify function for gEDA smobs.
 * \par Function Description
 * Clears a gEDA smob's pointer when the target object is destroyed.
 */
static void
smob_weakref_notify (void *target, void *smob) {
  SCM s = (SCM) smob;
  SCM_SET_SMOB_DATA (s, NULL);
}

/*! \brief Free a gEDA smob.
 * \par Function Description
 * Finalizes a gEDA smob for deletion, removing the weak reference.
 *
 * Used internally to Guile.
 */
static size_t
smob_free (SCM smob)
{
  void *data;

  /* If the weak reference has already been cleared, do nothing */
  if (!EDASCM_SMOB_VALIDP(smob)) return 0;

  data = (void *) SCM_SMOB_DATA (smob);

  /* Otherwise, clear the weak reference */
  switch (EDASCM_SMOB_TYPE (smob)) {
  case GEDA_SMOB_TOPLEVEL:
    s_toplevel_weak_unref ((TOPLEVEL *) data, smob_weakref_notify, smob);
    break;
  case GEDA_SMOB_PAGE:
    s_page_weak_unref ((PAGE *) data, smob_weakref_notify, smob);
    break;
  case GEDA_SMOB_OBJECT:
    s_object_weak_unref ((OBJECT *) data, smob_weakref_notify, smob);
    break;
  default:
    /* This should REALLY definitely never be run */
    g_critical ("%s: received bad smob flags.", __FUNCTION__);
  }

  /* If the smob is marked as garbage-collectable, destroy its
   * contents.
   *
   * Because PAGEs and TOPLEVELs should never be garbage collected,
   * emit critical warnings if the GC tries to free them.
   */
  if (EDASCM_SMOB_GCP (smob)) {
    switch (EDASCM_SMOB_TYPE (smob)) {
    case GEDA_SMOB_TOPLEVEL:
      g_critical ("%s: Blocked garbage-collection of TOPLEVEL %p",
                 __FUNCTION__, data);
      break;
    case GEDA_SMOB_PAGE:
      g_critical ("%s: Blocked garbage-collection of PAGE %p",
                 __FUNCTION__, data);
      break;
    case GEDA_SMOB_OBJECT:
      s_delete_object (edascm_c_current_toplevel(), (OBJECT *) data);
      break;
    default:
      /* This should REALLY definitely never be run */
      g_critical ("%s: received bad smob flags.", __FUNCTION__);
    }
  }
  return 0;
}

/*! \brief Print a representation of a gEDA smob.
 * \par Function Description
 * Outputs a string representing the gEDA \a smob to a Scheme output
 * \a port. The format used is "#<geda-TYPE b7ef65d0>", where TYPE is
 * a string describing the C structure represented by the gEDA smob.
 *
 * Used internally to Guile.
 */
static int
smob_print (SCM smob, SCM port, scm_print_state *pstate)
{
  gchar *hexstring;

  scm_puts ("#<geda-", port);

  switch (EDASCM_SMOB_TYPE (smob)) {
  case GEDA_SMOB_TOPLEVEL:
    scm_puts ("toplevel", port);
    break;
  case GEDA_SMOB_PAGE:
    scm_puts ("page", port);
    break;
  case GEDA_SMOB_OBJECT:
    scm_puts ("object", port);
    break;
  default:
    g_critical ("%s: received bad smob flags.", __FUNCTION__);
    scm_puts ("unknown", port);
  }

  scm_dynwind_begin (0);
  hexstring = g_strdup_printf (" %zx", SCM_SMOB_DATA (smob));
  scm_dynwind_unwind_handler (g_free, hexstring, SCM_F_WIND_EXPLICITLY);
  scm_puts (hexstring, port);
  scm_dynwind_end ();

  scm_puts (">", port);

  /* Non-zero means success */
  return 1;
}

/*! \brief Check gEDA smobs for equality.
 * \par Function description
 * Returns SCM_BOOL_T if \a obj1 represents the same gEDA structure as
 * \a obj2 does. Otherwise, returns SCM_BOOL_F.
 *
 * Used internally to Guile.
 */
static SCM
smob_equalp (SCM obj1, SCM obj2)
{
  EDASCM_ASSERT_SMOB_VALID (obj1);
  EDASCM_ASSERT_SMOB_VALID (obj2);

  if (SCM_SMOB_DATA (obj1) == SCM_SMOB_DATA (obj2)) {
    return SCM_BOOL_T;
  } else {
    return SCM_BOOL_F;
  }
}

/*! \brief Get the smob for a TOPLEVEL.
 * \par Function Description
 * Create a new smob representing \a toplevel.
 *
 * \param toplevel #TOPLEVEL to create a smob for.
 * \return a smob representing \a toplevel.
 */
SCM
edascm_from_toplevel (TOPLEVEL *toplevel)
{
  SCM smob;

  SCM_NEWSMOB (smob, geda_smob_tag, toplevel);
  SCM_SET_SMOB_FLAGS (smob, GEDA_SMOB_TOPLEVEL);

  /* Set weak reference */
  s_toplevel_weak_ref (toplevel, smob_weakref_notify, smob);

  return smob;
}

/*! \brief Get a smob for a page.
 * \par Function Description
 * Create a new smob representing \a page.
 *
 * \param page #PAGE to create a smob for.
 * \return a smob representing \a page.
 */
SCM
edascm_from_page (PAGE *page)
{
  SCM smob;

  SCM_NEWSMOB (smob, geda_smob_tag, page);
  SCM_SET_SMOB_FLAGS (smob, GEDA_SMOB_PAGE);

  /* Set weak reference */
  s_page_weak_ref (page, smob_weakref_notify, smob);

  return smob;
}

/* \brief Get a page from a smob.
 * \par Function Description
 * Return the #PAGE represented by \a smob.
 *
 * \param [in] smob Guile value to retrieve #PAGE from.
 * \return the #PAGE represented by \a smob.
 */
PAGE *
edascm_to_page (SCM smob)
{
#ifndef NDEBUG
  SCM_ASSERT (EDASCM_PAGEP (smob), smob,
              SCM_ARG1, "edascm_to_page");
#endif
  EDASCM_ASSERT_SMOB_VALID (smob);

  return (PAGE *) SCM_SMOB_DATA (smob);
}

/*! \brief Get a smob for a schematic object.
 * \par Function Description
 * Create a new smob representing \a object.
 *
 * \warning The returned smob is initially marked as owned by the C
 *   code. If it should be permitted to be garbage-collected, you
 *   should set the garbage-collectable flag by calling:
 *
 * \code
 *   SCM x = edascm_c_make_object (object);
 *   edascm_c_set_gc (x, 1);
 * \endcode
 *
 * \param object #OBJECT to create a smob for.
 * \return a smob representing \a object.
 */
SCM
edascm_from_object (OBJECT *object)
{
  SCM smob;

  SCM_NEWSMOB (smob, geda_smob_tag, object);
  SCM_SET_SMOB_FLAGS (smob, GEDA_SMOB_OBJECT);

  /* Set weak reference */
  s_object_weak_ref (object, smob_weakref_notify, smob);

  return smob;
}

/* \brief Get a schematic object from a smob.
 * \par Function Description
 * Return the #OBJECT represented by \a smob.
 *
 * \param [in] smob Guile value to retrieve #OBJECT from.
 * \return the #OBJECT represented by \a smob.
 */
OBJECT *
edascm_to_object (SCM smob)
{
#ifndef NDEBUG
  SCM_ASSERT (EDASCM_OBJECTP (smob), smob,
              SCM_ARG1, "edascm_to_object");
#endif
  EDASCM_ASSERT_SMOB_VALID (smob);

  return (OBJECT *) SCM_SMOB_DATA (smob);
}

/*! \brief Set whether a gEDA object may be garbage collected.
 * \par Function Description
 * If \a gc is non-zero, allow the structure represented by \a smob to
 * be destroyed when \a smob is garbage-collected.
 *
 * \param [in,out] smob Smob for which to set garbage-collection
 *                      permission.
 * \param [in]     x    If non-zero, permit garbage collection.
 */
void
edascm_c_set_gc (SCM smob, int gc)
{
  EDASCM_ASSERT_SMOB_VALID (smob);
  EDASCM_SMOB_SET_GC (smob, gc);
}

/*! \brief Initialise gEDA smob types etc
 * \par Function Description
 * Carry out initial setup of gEDA smob types, modules, procedures etc.
 * Should only be called by edascm_init().
 */
void
edascm_init_smob ()
{
  geda_smob_tag = scm_make_smob_type ("geda", 0);
  scm_set_smob_free (geda_smob_tag, smob_free);
  scm_set_smob_print (geda_smob_tag, smob_print);
  scm_set_smob_equalp (geda_smob_tag, smob_equalp);
}
