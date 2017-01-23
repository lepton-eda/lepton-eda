/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library - Scheme API
 * Copyright (C) 2010-2013, 2016 Peter Brett <peter@peter-b.co.uk>
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
 * The cost of allocating a Scheme value is quite high.  While a C
 * structure is not garbage-collectable, its Scheme value is protected
 * from the Guile garbage collector and cached.  As soon as the C
 * structure becomes "owned" by Guile and eligible for garbage
 * collection, it is dropped from the cache to allow it to be cleaned
 * up as normal.
 *
 * \sa weakref.c
 *
 * This file also provides support for a variety of GObject-based gEDA
 * types, including EdaConfig instances.
 */

#include <config.h>

#include "libgeda_priv.h"
#include "libgedaguile_priv.h"

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

/*! \brief Fetch a Scheme value from the cache
 * \par Function Description
 * Lookup \a target in the Scheme value cache, and if it is present,
 * return the corresponding Scheme value.  If \a target is not in the
 * cache, return #SCM_BOOL_F.
 *
 * \param target C value for which to fetch a cached Scheme value.
 * \return The cached Scheme value for \a target, or #SCM_BOOL_F.
 */
static SCM
smob_cache_lookup (void *target)
{
  SmobCacheEntry *entry = g_hash_table_lookup (smob_cache, target);
  return entry ? entry->smob : SCM_BOOL_F;
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

/*! \brief Weak reference notify function for gEDA smobs.
 * \par Function Description
 * Clears a gEDA smob's pointer when the target object is destroyed.
 */
static void
smob_weakref_notify (void *target, void *smob) {
  SCM s = pack_from_pointer (smob);
  SCM_SET_SMOB_DATA (s, NULL);
  smob_cache_remove (target);
}

/*! \brief Weak reference notify function for double-length gEDA smobs.
 * \par Function Description
 * Clears a gEDA smob's second pointer when the target object is
 * destroyed.
 *
 * \see edascm_from_object().
 */
static void
smob_weakref2_notify (void *target, void *smob) {
  SCM s = pack_from_pointer (smob);
  SCM_SET_SMOB_DATA (s, NULL);
  SCM_SET_SMOB_DATA_2 (s, NULL);
  smob_cache_remove (target);
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

  /* If the smob is being finalized, it must not be in the cache! */
  g_warn_if_fail (!smob_cache_contains (data));

  /* Otherwise, clear the weak reference */
  switch (EDASCM_SMOB_TYPE (smob)) {
  case GEDA_SMOB_TOPLEVEL:
    s_toplevel_weak_unref ((TOPLEVEL *) data, smob_weakref_notify,
                           unpack_as_pointer (smob));
    break;
  case GEDA_SMOB_PAGE:
    s_page_weak_unref ((PAGE *) data, smob_weakref_notify,
                       unpack_as_pointer (smob));
    break;
  case GEDA_SMOB_OBJECT:
    /* See edascm_from_object() for an explanation of why OBJECT
     * smobs store a TOPLEVEL in the second data word */
    s_object_weak_unref ((OBJECT *) data, smob_weakref2_notify,
                         unpack_as_pointer (smob));
    break;
  case GEDA_SMOB_CONFIG:
    g_object_unref (G_OBJECT (data));
    break;
  case GEDA_SMOB_CLOSURE:
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
      /* See edascm_from_object() for an explanation of why OBJECT
       * smobs store a TOPLEVEL in the second data word */
      s_delete_object ((TOPLEVEL *) SCM_SMOB_DATA_2 (smob), (OBJECT *) data);
      break;
    case GEDA_SMOB_CONFIG:
      /* These are reference counted, so the structure will have
       * already been destroyed above if appropriate. */
      break;
    case GEDA_SMOB_CLOSURE:
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
  case GEDA_SMOB_CONFIG:
    scm_puts ("config", port);
    break;
  case GEDA_SMOB_CLOSURE:
    scm_puts ("closure", port);
    break;
  default:
    g_critical ("%s: received bad smob flags.", __FUNCTION__);
    scm_puts ("unknown", port);
  }

  if (SCM_SMOB_DATA (smob) != 0) {
    scm_dynwind_begin (0);
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
 * \ingroup guile_c_iface
 * \par Function Description
 * Create a new smob representing \a toplevel.
 *
 * \param toplevel #TOPLEVEL to create a smob for.
 * \return a smob representing \a toplevel.
 */
SCM
edascm_from_toplevel (TOPLEVEL *toplevel)
{
  SCM smob = smob_cache_lookup (toplevel);

  if (EDASCM_TOPLEVELP (smob)) {
    return smob;
  }

  SCM_NEWSMOB (smob, geda_smob_tag, toplevel);
  SCM_SET_SMOB_FLAGS (smob, GEDA_SMOB_TOPLEVEL);

  /* Set weak reference */
  s_toplevel_weak_ref (toplevel, smob_weakref_notify,
                       unpack_as_pointer (smob));

  smob_cache_add (toplevel, smob);

  return smob;
}

/*! \brief Get a smob for a page.
 * \ingroup guile_c_iface
 * \par Function Description
 * Create a new smob representing \a page.
 *
 * \param page #PAGE to create a smob for.
 * \return a smob representing \a page.
 */
SCM
edascm_from_page (PAGE *page)
{
  SCM smob = smob_cache_lookup (page);

  if (EDASCM_PAGEP (smob)) {
    return smob;
  }

  SCM_NEWSMOB (smob, geda_smob_tag, page);
  SCM_SET_SMOB_FLAGS (smob, GEDA_SMOB_PAGE);

  /* Set weak reference */
  s_page_weak_ref (page, smob_weakref_notify,
                   unpack_as_pointer (smob));

  smob_cache_add (page, smob);

  return smob;
}

/*! \brief Get a page from a smob.
 * \ingroup guile_c_iface
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
 * \ingroup guile_c_iface
 * \par Function Description
 * Create a new smob representing \a object.
 *
 * \warning The returned smob is initially marked as owned by the C
 *   code. If it should be permitted to be garbage-collected, you
 *   should set the garbage-collectable flag by calling:
 *
 * \code
 *   SCM x = edascm_from_object (object);
 *   edascm_c_set_gc (x, 1);
 * \endcode
 *
 * \note We currently have to bake a TOPLEVEL pointer into the smob,
 * so that if the object becomes garbage-collectable we can obtain a
 * TOPLEVEL to use for deleting the smob without accessing the
 * TOPLEVEL fluid and potentially causing a race condition (see bug
 * 909358).
 *
 * \param object #OBJECT to create a smob for.
 * \return a smob representing \a object.
 */
SCM
edascm_from_object (OBJECT *object)
{
  SCM smob = smob_cache_lookup (object);

  if (EDASCM_OBJECTP (smob)) {
    return smob;
  }

  TOPLEVEL *toplevel = edascm_c_current_toplevel ();

  SCM_NEWSMOB2 (smob, geda_smob_tag, object, toplevel);
  SCM_SET_SMOB_FLAGS (smob, GEDA_SMOB_OBJECT);

  /* Set weak references */
  s_object_weak_ref (object, smob_weakref2_notify,
                     unpack_as_pointer (smob));

  smob_cache_add (object, smob);

  return smob;
}

/*! \brief Get a schematic object from a smob.
 * \ingroup guile_c_iface
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

/*! \brief Get a smob for a configuration context.
 * \ingroup guile_c_iface
 * \par Function Description
 * Create a new smob representing \a cfg.
 *
 * \param cfg Configuration context to create a smob for.
 * \return a smob representing \a cfg.
 */
SCM
edascm_from_config (EdaConfig *cfg)
{
  SCM smob = smob_cache_lookup (cfg);

  if (EDASCM_CONFIGP (smob)) {
    return smob;
  }

  SCM_NEWSMOB (smob, geda_smob_tag, g_object_ref (cfg));
  SCM_SET_SMOB_FLAGS (smob, GEDA_SMOB_CONFIG);
  return smob;
}

/*! \brief Get a configuration context from a smob.
 * \ingroup guile_c_iface
 * \par Function Description
 * Return the #EdaConfig represented by \a smob.
 *
 * \param [in] smob Guile value to retrieve #EdaConfig from.
 * \return the #EdaConfig represented by \a smob.
 */
EdaConfig *
edascm_to_config (SCM smob)
{
#ifndef NDEBUG
  SCM_ASSERT (EDASCM_CONFIGP (smob), smob,
              SCM_ARG1, "edascm_to_object");
#endif
  EDASCM_ASSERT_SMOB_VALID (smob);

  return EDA_CONFIG (SCM_SMOB_DATA (smob));
}

/*! \brief Get a smob for a C closure.
 * \par Function Description
 * Create a new smob representing a C closure.
 *
 * \warning Do not call this function from user code; use
 * edascm_c_make_closure() instead.
 *
 * \param func C function to make closure around.
 * \param user_data User data for function.
 * \return a C closure smob.
 */
SCM
edascm_from_closure (SCM (*func)(SCM, gpointer), gpointer user_data)
{
  SCM smob;
  SCM_NEWSMOB2 (smob, geda_smob_tag, func, user_data);
  SCM_SET_SMOB_FLAGS (smob, GEDA_SMOB_CLOSURE);
  return smob;
}

/*! \brief Set whether a gEDA object may be garbage collected.
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

/*! \brief Test whether a smob is a #OBJECT instance
 * \ingroup guile_c_iface
 * \par Function Description
 * If \a smob is a #OBJECT instance, returns non-zero. Otherwise,
 * returns zero.
 *
 * \param [in] smob Guile value to test.
 *
 * \return non-zero iff \a smob is a #OBJECT instance.
 */
int
edascm_is_object (SCM smob)
{
  return EDASCM_OBJECTP (smob);
}

/*! \brief Test whether a smob is a #PAGE instance
 * \ingroup guile_c_iface
 * \par Function Description
 * If \a smob is a #PAGE instance, returns non-zero. Otherwise,
 * returns zero.
 *
 * \param [in] smob Guile value to test.
 *
 * \return non-zero iff \a smob is a #PAGE instance.
 */
int
edascm_is_page (SCM smob)
{
  return EDASCM_PAGEP (smob);
}

/*! \brief Test whether a smob is an #EdaConfig instance.
 * \ingroup guile_c_iface
 * \par Function Description
 * If \a smob is a configuration context, returns non-zero. Otherwise,
 * returns zero.
 *
 * \param [in] smob Guile value to test.
 * \return non-zero iff \a smob is an #EdaConfig instance.
 */
int
edascm_is_config (SCM smob)
{
  return EDASCM_CONFIGP (smob);
}

/*! \brief Test whether a smob is a #PAGE instance.
 * \par Function Description
 * If \a page_smob is a #PAGE instance, returns \b SCM_BOOL_T;
 * otherwise returns \b SCM_BOOL_F.
 *
 * \note Scheme API: Implements the %page? procedure in the (geda
 * core smob) module.
 *
 * \param [in] page_smob Guile value to test.
 *
 * \return SCM_BOOL_T iff \a page_smob is a #PAGE instance.
 */
SCM_DEFINE (page_p, "%page?", 1, 0, 0,
            (SCM page_smob),
            "Test whether the value is a gEDA PAGE instance.")
{
  return (EDASCM_PAGEP (page_smob) ? SCM_BOOL_T : SCM_BOOL_F);
}

/*! \brief Test whether a smob is an #OBJECT instance.
 * \par Function Description
 * If \a object_smob is an #OBJECT instance, returns \b SCM_BOOL_T;
 * otherwise returns \b SCM_BOOL_F.
 *
 * \note Scheme API: Implements the %object? procedure in the (geda
 * core smob) module.
 *
 * \param [in] object_smob Guile value to test.
 *
 * \return SCM_BOOL_T iff \a object_smob is an #OBJECT instance.
 */
SCM_DEFINE (object_p, "%object?", 1, 0, 0,
            (SCM object_smob),
            "Test whether the value is a gEDA OBJECT instance.")
{
  return (EDASCM_OBJECTP (object_smob) ? SCM_BOOL_T : SCM_BOOL_F);
}

/*! \brief Test whether a smob is an #EdaConfig instance.
 * \par Function Description
 * If \a config_smob is a configuration context, returns \b
 * SCM_BOOL_T; otherwise returns \b SCM_BOOL_F.
 *
 * \note Scheme API: Implements the %config? procedure in the (geda
 * core smob) module.
 *
 * \param [in] config_smob Guile value to test.
 *
 * \return SCM_BOOL_T iff \a config_smob is an #EdaConfig instance.
 */
SCM_DEFINE (config_p, "%config?", 1, 0, 0,
            (SCM config_smob),
            "Test whether the value is a gEDA configuration context.")
{
  return (EDASCM_CONFIGP (config_smob) ? SCM_BOOL_T : SCM_BOOL_F);
}

/*!
 * \brief Create the (geda core smob) Scheme module.
 * \par Function Description
 * Defines procedures in the (geda core smob) module. The module can
 * be accessed using (use-modules (geda core smob)).
 */
static void
init_module_geda_core_smob ()
{
  /* Register the functions. */
  #include "scheme_smob.x"

  /* Add them to the module's public definitions. */ 
  scm_c_export (s_page_p, s_object_p, s_config_p, NULL);
}

/*!
 * \brief Initialise the basic gEDA smob types.
 * \par Function Description
 * Registers the gEDA core smob types and some procedures acting on
 * them.  gEDA only uses a single Guile smob, and uses the flags field
 * to multiplex the several different underlying C structures that may
 * be represented by that smob. Should only be called by
 * edascm_init().
 */
void
edascm_init_smob ()
{
  /* Initialize smob cache */
  smob_cache_init ();

  /* Register gEDA smob type */
  geda_smob_tag = scm_make_smob_type ("geda", 0);
  scm_set_smob_free (geda_smob_tag, smob_free);
  scm_set_smob_print (geda_smob_tag, smob_print);
  scm_set_smob_equalp (geda_smob_tag, smob_equalp);

  /* Define the (geda core smob) module */
  scm_c_define_module ("geda core smob",
                       init_module_geda_core_smob,
                       NULL);
}
