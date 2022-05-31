/* Lepton EDA library - Scheme API
 * Copyright (C) 2010-2013, 2016 Peter Brett <peter@peter-b.co.uk>
 * Copyright (C) 2010-2016 gEDA Contributors
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

/*!
 * \file scheme_smob.c
 * \brief Scheme representations of Lepton EDA C structures
 *
 * In order for Scheme code to be able to manipulate liblepton data
 * structures, it is convenient for it to be able to get handles to
 * several of the different C structures that liblepton uses, in
 * particular #LeptonToplevel, #LeptonPage and #LeptonObject.
 *
 * A particular issue is that, in principle, Guile can stash a
 * variable somewhere and only try and access it much later, possibly
 * after the underlying C structure has already been freed.
 *
 * In order to avoid this situation causing a segmentation fault,
 * weak references are used. In the case of #LeptonPage and
 * #LeptonToplevel handles, the usage is quite straightforward;
 * Scheme code can never create or destroy a #LeptonToplevel; and
 * although a #LeptonPage can be created by Scheme code, it must
 * explicitly be destroyed if the Scheme code doesn't want the
 * #LeptonPage to hang around after it returns.
 *
 * #LeptonObject handles are a more complex case. It's possible
 * that Scheme code may legitimately want to create an
 * #LeptonObject and do something with it (or, similarly, pull an
 * #LeptonObject out of a #LeptonPage), without needing to carefully
 * keep track of the #LeptonObject to avoid dropping it on the
 * floor. In that case, users should be able to rely on the
 * garbage collector.
 *
 * For that reason, an #LeptonObject is marked to be destroyed by
 * garbage-collection in two cases:
 *
 * -# If they have been created by Scheme code, but not yet added to a
 *    LeptonPage.
 * -# If they have been removed from a #LeptonPage by Scheme code, but not
 *    yet re-added to a #LeptonPage.
 *
 * The cost of allocating a Scheme value is quite high.  While a C
 * structure is not garbage-collectable, its Scheme value is protected
 * from the Guile garbage collector and cached.  As soon as the C
 * structure becomes "owned" by Guile and eligible for garbage
 * collection, it is dropped from the cache to allow it to be cleaned
 * up as normal.
 *
 * \sa weakref.c
 *
 * This file also provides support for a variety of GObject-based
 * Lepton EDA types, including EdaConfig instances.
 */

#include <config.h>

#include "liblepton_priv.h"
#include "libleptonguile_priv.h"

scm_t_bits geda_smob_tag;

G_STATIC_ASSERT (sizeof (void*) == sizeof (SCM));

/*! Unsafely convert a Scheme value into a pointer. */
static inline gpointer
unpack_as_pointer (SCM s)
{
  return (void *) SCM_UNPACK (s);
}

/*! Unsafely convert a pointer into a Scheme value. */
static inline SCM
pack_from_pointer (gpointer p)
{
  return SCM_PACK ((scm_t_bits) p);
}

/*! Cache for non-garbage-collectable Scheme values. */
static GHashTable *smob_cache = NULL;

/*! \brief Cache entry for Scheme values.
 *
 * While a #SmobCacheEntry is live, the Scheme value it contains is
 * protected from garbage collection.
 */
typedef struct _SmobCacheEntry SmobCacheEntry;

struct _SmobCacheEntry
{
  SCM smob;
};

/*! \brief Allocate a cache entry for Scheme values.
 * \par Function Description
 * Allocate and return a new cache entry for the Scheme value \a smob,
 * which is protected from garbage collection until the cache entry is
 * destroyed with smob_cache_entry_destroy().
 *
 * \param smob Scheme value to be cached.
 * \return a newly allocated cache entry structure.
 */
static SmobCacheEntry *
smob_cache_entry_new (SCM smob)
{
  SmobCacheEntry *entry = g_slice_new (SmobCacheEntry);
  entry->smob = scm_gc_protect_object (smob);
  return entry;
}

/*! \brief Destroy a cache entry for Scheme values.
 * \par Function Description
 * Destroy \a entry, removing garbage collection protection from the
 * Scheme value it contains.
 *
 * \param entry The cache entry structure to be destroyed.
 */
static void
smob_cache_entry_destroy (SmobCacheEntry *entry)
{
  scm_gc_unprotect_object (entry->smob);
  g_slice_free (SmobCacheEntry, entry);
}

/*! \brief Initialise the Scheme value cache
 *
 * Should only be called by edascm_init_smob().
 */
static void
smob_cache_init (void)
{
  smob_cache = g_hash_table_new_full (NULL, NULL, NULL,
                   (GDestroyNotify) smob_cache_entry_destroy);
}

/*! \brief Add a Scheme value to the cache
 * \par Function Description
 * Cache a Scheme value \a smob for the C value \a target.  The \a
 * smob will be protected from garbage collection until it is removed
 * with smob_cache_remove(), or until smob_cache_add() is called again
 * for the same \a target.
 *
 * \param target The C value that the Scheme value belongs to
 * \param smob The Scheme value to cache
 */
static void
smob_cache_add (void *target, SCM smob)
{
  g_hash_table_insert (smob_cache, target,
                       smob_cache_entry_new (smob));
}

/*! \brief Remove a Scheme value from the cache
 * \par Function Description
 * Remove and unprotect the Scheme value corresponding to \a target
 * from the cache.
 *
 * \warning This must be called whenever the \a target becomes owned
 * by Scheme, or it may be leaked.
 *
 * \param target The C value for which to uncache Scheme values.
 */
static void
smob_cache_remove (void *target)
{
  g_hash_table_remove (smob_cache, target);
}

/*! \brief Check whether a Scheme value is cached
 * \par Function Description
 * Lookup \a target in the Scheme value, and return #TRUE iff it has a
 * Scheme value associated with it.
 *
 * \param target C value to check for in the Scheme value cache.
 * \return #TRUE if \a target has a cached Scheme value.
 */
G_GNUC_UNUSED static gboolean
smob_cache_contains (void *target)
{
  gboolean result = g_hash_table_contains (smob_cache, target);
  return result;
}

/*! \brief Free a Lepton EDA smob.
 * \par Function Description
 * Finalizes a Lepton EDA smob for deletion, removing the weak reference.
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

  /* If the smob is being finalized, it must not be in the cache! */
  g_warn_if_fail (!smob_cache_contains (data));

  /* Otherwise, clear the weak reference */
  switch (EDASCM_SMOB_TYPE (smob)) {
  case GEDA_SMOB_OBJECT:
    break;
  default:
    /* This should REALLY definitely never be run */
    g_critical ("%s: received bad smob flags.", __FUNCTION__);
  }

  /* If the smob is marked as garbage-collectable, destroy its
   * contents.
   *
   * Because LeptonPages and LeptonToplevels should never be garbage collected,
   * emit critical warnings if the GC tries to free them.
   */
  if (EDASCM_SMOB_GCP (smob)) {
    switch (EDASCM_SMOB_TYPE (smob)) {
    case GEDA_SMOB_OBJECT:
      break;
    default:
      /* This should REALLY definitely never be run */
      g_critical ("%s: received bad smob flags.", __FUNCTION__);
    }
  }
  return 0;
}

/*! \brief Print a representation of a Lepton EDA smob.
 * \par Function Description
 * Outputs a string representing the Lepton EDA \a smob to a Scheme output
 * \a port. The format used is "#<geda-TYPE b7ef65d0>", where TYPE is
 * a string describing the C structure represented by the Lepton EDA smob.
 *
 * Used internally to Guile.
 */
static int
smob_print (SCM smob, SCM port, scm_print_state *pstate)
{
  gchar *hexstring;

  scm_puts ("#<geda-", port);

  switch (EDASCM_SMOB_TYPE (smob)) {
  case GEDA_SMOB_OBJECT:
    scm_puts ("object", port);
    break;
  default:
    g_critical ("%s: received bad smob flags.", __FUNCTION__);
    scm_puts ("unknown", port);
  }

  if (SCM_SMOB_DATA (smob) != 0) {
    scm_dynwind_begin ((scm_t_dynwind_flags) 0);
    hexstring = g_strdup_printf (" %p", (void *) SCM_SMOB_DATA (smob));
    scm_dynwind_unwind_handler (g_free, hexstring, SCM_F_WIND_EXPLICITLY);
    scm_puts (hexstring, port);
    scm_dynwind_end ();
  } else {
    scm_puts (" (null)", port);
  }

  scm_puts (">", port);

  /* Non-zero means success */
  return 1;
}

/*! \brief Check Lepton EDA smobs for equality.
 * \par Function description
 * Returns SCM_BOOL_T if \a obj1 represents the same Lepton EDA structure as
 * \a obj2 does. Otherwise, returns SCM_BOOL_F.
 *
 * Used internally to Guile.
 */
static SCM
smob_equalp (SCM obj1, SCM obj2)
{
  g_debug ("smob_equalp()\n");
  EDASCM_ASSERT_SMOB_VALID (obj1);
  EDASCM_ASSERT_SMOB_VALID (obj2);

  if (SCM_SMOB_DATA (obj1) == SCM_SMOB_DATA (obj2)) {
    return SCM_BOOL_T;
  } else {
    return SCM_BOOL_F;
  }
}



/*! \brief Set whether a Lepton EDA object may be garbage collected.
 * \ingroup guile_c_iface
 * \par Function Description
 * If \a gc is non-zero, allow the structure represented by \a smob to
 * be destroyed when \a smob is garbage-collected.
 *
 * \param [in,out] smob Smob for which to set garbage-collection
 *                      permission.
 * \param [in]     gc    If non-zero, permit garbage collection.
 */
void
edascm_c_set_gc (SCM smob, int gc)
{
  g_debug ("edascm_c_set_gc()\n");
  EDASCM_ASSERT_SMOB_VALID (smob);
  int current = EDASCM_SMOB_GCP (smob);

  /* Ensure that when smob becomes garbage-collectible, it's removed
   * from the Scheme value cache, and that when it stops being
   * garbage-collectible it's cached for re-use. */
  if (gc && !current)
    smob_cache_remove ((void *) SCM_SMOB_DATA (smob));
  if (!gc && current)
    smob_cache_add ((void *) SCM_SMOB_DATA (smob), smob);

  EDASCM_SMOB_SET_GC (smob, gc);
}

/*!
 * \brief Initialise the basic Lepton EDA smob types.
 * \par Function Description
 * Registers the Lepton EDA core smob types and some procedures
 * acting on them.  Lepton EDA only uses a single Guile smob, and
 * uses the flags field to multiplex the several different
 * underlying C structures that may be represented by that
 * smob. Should only be called by edascm_init().
 */
void
edascm_init_smob ()
{
  /* Initialize smob cache */
  smob_cache_init ();

  /* Register Lepton EDA smob type */
  geda_smob_tag = scm_make_smob_type ("geda", 0);
  scm_set_smob_free (geda_smob_tag, smob_free);
  scm_set_smob_print (geda_smob_tag, smob_print);
  scm_set_smob_equalp (geda_smob_tag, smob_equalp);
}
