/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library - Scheme API
 * Copyright (C) 2013-2014 Peter Brett <peter@peter-b.co.uk>
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

#include <config.h>

#include "libgeda_priv.h"
#include "libgedaguile_priv.h"

#include <gobject/gvaluecollector.h>

/* ---------------------------------------------------------------- */

/* Internal functions used by GValue.  Note that we store the SCM in
 * the GValue in unpacked form, as an unsigned long.  Comments below
 * are copied from GTypeValueTable documentation.  Also, make sure to
 * read the edascm_scm_get_type() function, where these functions are
 * used. */

/*   "Default initialize values contents by poking values directly
 *   into the value->data array. The data array of the GValue passed
 *   into this function was zero-filled with memset(), so no care has
 *   to be taken to free any old contents."
 *
 * The default SCM value is SCM_UNDEFINED (i.e. no value).
 */
static void
value_init_scm (GValue *value) {
  value->data[0].v_long = SCM_UNPACK (SCM_UNDEFINED);
}

/*   "Free any old contents that might be left in the data array of
 *   the passed in value. No resources may remain allocated through
 *   the GValue contents after this function returns."
 *
 * If a SCM object is actually present, remove a layer of GC
 * protection (equivalent to decreasing ref count).
 */
static void
value_free_scm (GValue *value) {
  SCM val = SCM_PACK (value->data[0].v_long);
  if (edascm_is_defined (val))
    scm_gc_unprotect_object (val);
}

/*   "dest_value is a GValue with zero-filled data section and
 *   src_value is a properly setup GValue of same or derived type. The
 *   purpose of this function is to copy the contents of src_value
 *   into dest_value in a way, that even after src_value has been
 *   freed, the contents of dest_value remain valid."
 *
 * There are two possible cases.  Firstly, the source might not
 * contain a SCM object, in which case we just copy it over.  If it
 * does contain an object, we need to add a layer of GC protection
 * first (equivalent to increasing ref count).
 */
static void
value_copy_scm (const GValue *src, GValue *dest) {
  SCM val = SCM_PACK (src->data[0].v_long);
  if (edascm_is_defined (val)) {
    scm_gc_protect_object (val);
  }
  dest->data[0].v_long = SCM_UNPACK (val);
}

/*   "The collect_value() function is responsible for converting the
 *   values collected from a variable argument list into contents
 *   suitable for storage in a GValue. This function should setup
 *   value similar to value_init(); e.g. for a string value that does
 *   not allow NULL pointers, it needs to either spew an error, or do
 *   an implicit conversion by storing an empty string. The value
 *   passed in to this function has a zero-filled data array, so just
 *   like for value_init() it is guaranteed to not contain any old
 *   contents that might need freeing. n_collect_values is exactly the
 *   string length of collect_format, and collect_values is an array
 *   of unions GTypeCValue with length n_collect_values, containing
 *   the collected values according to collect_format. collect_flags
 *   is an argument provided as a hint by the caller. It may contain
 *   the flag G_VALUE_NOCOPY_CONTENTS indicating, that the collected
 *   value contents may be considered "static" for the duration of the
 *   value lifetime. Thus an extra copy of the contents stored in
 *   collect_values is not required for assignment to value.
 *
 *   "It should be noted, that it is generally a bad idea to follow
 *   the G_VALUE_NOCOPY_CONTENTS hint for reference counted types. Due
 *   to reentrancy requirements and reference count assertions
 *   performed by the signal emission code, reference counts should
 *   always be incremented for reference counted contents stored in
 *   the value->data array."
 *
 * Yay, wall of text.
 */
static gchar *
value_collect_scm (GValue *value,
                   guint n_collect_values,
                   GTypeCValue *collect_values,
                   guint collect_flags)
{
  SCM val = SCM_PACK (collect_values[0].v_long);

  if (edascm_is_defined (val)) {
    /* never honour G_VALUE_NOCOPY_CONTENTS for ref-counted types */
    scm_gc_protect_object (val);
    value->data[0].v_long = SCM_UNPACK (val);

  } else {
    /* No value */
    value->data[0].v_long = SCM_UNPACK (SCM_UNDEFINED);
  }

  return NULL;
}

/*   "This function is responsible for storing the value contents into
 *   arguments passed through a variable argument list which got
 *   collected into collect_values according to lcopy_format.
 *   n_collect_values equals the string length of lcopy_format, and
 *   collect_flags may contain %G_VALUE_NOCOPY_CONTENTS.  In contrast
 *   to collect_value(), lcopy_value() is obliged to always properly
 *   support G_VALUE_NOCOPY_CONTENTS.  Similar to collect_value() the
 *   function may prematurely abort by returning a newly allocated
 *   string describing an error condition."
 */
static gchar *
value_lcopy_scm (const GValue *value,
                 guint n_collect_values,
                 GTypeCValue *collect_values,
                 guint collect_flags)
{
  SCM val = SCM_PACK (value->data[0].v_long);
  glong *long_p = collect_values[0].v_pointer;

  if (!long_p)
    return g_strdup_printf ("value location for `%s' passed as NULL",
                            G_VALUE_TYPE_NAME (value));

  if (!edascm_is_defined (val)) {
    /* No value */
    *long_p = SCM_UNPACK (SCM_UNDEFINED);
  } else if (collect_flags & G_VALUE_NOCOPY_CONTENTS) {
    /* There's a value; because we're not copying it, don't add a
     * level of GC protection. */
    *long_p = SCM_UNPACK (val);
  } else {
    /* There's a value. It needs a level of GC protection. */
    scm_gc_protect_object (val);
    *long_p = SCM_UNPACK (val);
  }

 return NULL;
}

/* ---------------------------------------------------------------- */
/* Public functions related to low-level GType / GValue support. */

GType
edascm_scm_get_type (void)
{
  static GType edascm_scm_type = 0;
  if (g_once_init_enter (&edascm_scm_type)) {
    GType type;

    static const GTypeValueTable value_table = {
      value_init_scm,    /* value_init */
      value_free_scm,    /* value_free */
      value_copy_scm,    /* value_copy */
      NULL,              /* value_peek_pointer */
      "l",               /* collect_format */
      value_collect_scm, /* collect_value */
      "p",               /* lcopy_format */
      value_lcopy_scm,   /* lcopy_value */
    };

    static const GTypeInfo info = {
      0,            /* class_size */
      NULL,         /* base_init */
      NULL,         /* base_finalize */
      NULL,         /* class_init */
      NULL,         /* class_finalize */
      NULL,         /* class_data */
      0,            /* instance_size */
      0,            /* n_preallocs */
      NULL,         /* instance_init */
      &value_table, /* value_table */
    };

    static const GTypeFundamentalInfo finfo = {
      0, /* type_flags */
    };

    type = g_type_register_fundamental (g_type_fundamental_next (),
                                        g_intern_static_string ("SCM"), /* type_name */
                                        &info,  /* info */
                                        &finfo, /* finfo */
                                        0);    /* type_flags */

    g_once_init_leave (&edascm_scm_type, type);
  }
  return edascm_scm_type;
}

/*! \brief Set the contents of a SCM GValue.
 * \ingroup guile_c_iface
 * Set the contents of a SCM GValue to \a v_scm.
 *
 * \param value a valid GValue of #EDASCM_TYPE_SCM type.
 * \param v_scm SCM value to set.
 */
void
edascm_value_set_scm (GValue *value, SCM v_scm)
{
  SCM old;

  g_return_if_fail (EDASCM_VALUE_HOLDS_SCM (value));

  old = SCM_PACK (value->data[0].v_long);

  if (edascm_is_defined (v_scm)) {
    value->data[0].v_long = SCM_UNPACK (v_scm);
    scm_gc_protect_object (v_scm);
  } else {
    value->data[0].v_long = SCM_UNPACK (SCM_UNDEFINED);
  }

  if (edascm_is_defined (old))
    scm_gc_unprotect_object (old);
}

/*! \brief Get the contents of a SCM GValue.
 * \ingroup guile_c_iface
 * Get the contents of a SCM GValue.
 *
 * \param value a valid GValue of #EDASCM_TYPE_SCM type.
 * \return SCM value contents of \a value.
 */
SCM
edascm_value_get_scm (const GValue *value)
{
  g_return_val_if_fail (EDASCM_VALUE_HOLDS_SCM (value), SCM_UNDEFINED);

  return SCM_PACK (value->data[0].v_long);
}

/* ---------------------------------------------------------------- */
/* Internal functions used by GParamSpec. */

static gint
param_scm_values_cmp (GParamSpec *pspec,
                      const GValue *value1,
                      const GValue *value2)
{
  SCM a = edascm_value_get_scm (value1);
  SCM b = edascm_value_get_scm (value2);

  if (scm_is_true (scm_equal_p (a, b))) return 0;
  return memcmp (value1, value2, sizeof (GValue));
}

/* ---------------------------------------------------------------- */
/* Public functions related to GParamSpec support. */

GType
edascm_param_spec_scm_get_type (void)
{
  static GType edascm_param_spec_scm_type = 0;
  if (g_once_init_enter (&edascm_param_spec_scm_type)) {
    GType type;
    const GParamSpecTypeInfo pspec_info = {
      sizeof (EdascmParamSpecSCM), /* instance_size */
      0, /* n_preallocs */
      NULL, /* instance_init */
      EDASCM_TYPE_SCM, /* value_type */
      NULL, /* finalize */
      NULL, /* value_set_default */
      NULL, /* value_validate */
      param_scm_values_cmp, /* values_cmp */
    };

    type = g_param_type_register_static (g_intern_static_string ("EdascmParamSCM"),
                                         &pspec_info);

    g_once_init_leave (&edascm_param_spec_scm_type, type);
  }
  return edascm_param_spec_scm_type;
}

/*! \brief Create a new #EdascmParamSpecSCM.
 * \public \memberof EdascmParamSCM
 * Create a new #EdascmParamSpecSCM instance specifying a
 * #EDASCM_TYPE_SCM property.
 *
 * \param name canonical name of the property specified.
 * \param nick nick name for the property specified.
 * \param blurb description of the property specified.
 * \param flags flags for the property specified.
 * \return a newly created parameter specification.
 */
GParamSpec *edascm_param_spec_scm (const gchar *name,
                                   const gchar *nick,
                                   const gchar *blurb,
                                   GParamFlags flags)
{
  EdascmParamSpecSCM *sspec;

  sspec = g_param_spec_internal (EDASCM_TYPE_PARAM_SCM,
                                 name,
                                 nick,
                                 blurb,
                                 flags);

  return G_PARAM_SPEC (sspec);
}
