/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2017 gEDA Contributors
 * Copyright (C) 2017-2020 Lepton EDA Contributors
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
#include <config.h>
#include <stdio.h>

#include "libgeda_priv.h"



/* \note
 * Kazu Hirata <kazu@seul.org> on July 16, 1999 - Added these absolute
 * defaults used when default_... is NULL.
 */

int   default_attribute_promotion = TRUE;
int   default_promote_invisible = FALSE;
int   default_keep_invisible = TRUE;

int   default_make_backup_files = TRUE;



/* \brief Read a boolean configuration key.
 *
 * \par Function Description
 * On success, set \a result to the value of the
 * configuration key, otherwise set it to \a defval.
 *
 * \todo  Refactoring: this function was copied as is from schematic/src/i_vars.c.
 *
 * \param [in]       group   Configuration group name
 * \param [in]       key     Configuration key name
 * \param [in]       defval  Default value
 * \param [in, out]  result  Result
 *
 * \return  TRUE if a specified config parameter was successfully read
 */
static gboolean
cfg_read_bool (const gchar* group,
               const gchar* key,
               gboolean     defval,
               gboolean*    result)
{
  gchar*     cwd = g_get_current_dir();
  EdaConfig* cfg = eda_config_get_context_for_path (cwd);
  g_free (cwd);

  GError*  err = NULL;
  gboolean val = eda_config_get_boolean (cfg, group, key, &err);

  gboolean success = err == NULL;
  g_clear_error (&err);

  *result = success ? val : defval;
  return success;
}



/*! \brief Read configuration, initialize variables in TOPLEVEL object.
 *
 *  \param [in] toplevel  The TOPLEVEL object to be updated.
 */
void i_vars_libgeda_set(TOPLEVEL *toplevel)
{
  cfg_read_bool ("schematic.attrib", "promote",
                 default_attribute_promotion, &toplevel->attribute_promotion);

  cfg_read_bool ("schematic.attrib", "promote-invisible",
                 default_promote_invisible, &toplevel->promote_invisible);

  cfg_read_bool ("schematic.attrib", "keep-invisible",
                 default_keep_invisible, &toplevel->keep_invisible);

  cfg_read_bool ("schematic.backup", "create-files",
                 default_make_backup_files, &toplevel->make_backup_files);


  if (toplevel->always_promote_attributes)
  {
    g_ptr_array_unref (toplevel->always_promote_attributes);
    toplevel->always_promote_attributes = NULL;
  }

  toplevel->always_promote_attributes = g_ptr_array_new (); /* => refcnt == 1 */

  gchar*     cwd = g_get_current_dir();
  EdaConfig* cfg = eda_config_get_context_for_path (cwd);
  g_free (cwd);

  GError* err   = NULL;
  gsize   size  = 0;
  gchar** ppstr = eda_config_get_string_list (cfg,
                                              "schematic.attrib",
                                              "always-promote",
                                              &size,
                                              &err);
  if (err == NULL && ppstr != NULL)
  {
    for (gsize i = 0; i < size; ++i)
    {
      gchar* attr = ppstr[i];

      if (attr != NULL && strlen (attr) > 0)
      {
#ifdef DEBUG
        printf( " >> always_promote_attributes += [%s]\n", attr );
#endif
        /* important: use g_intern_string() here, because attr strings are
         * compared like pointers in o_complex_is_eligible_attribute():
        */
        g_ptr_array_add (toplevel->always_promote_attributes,
                         (gpointer) g_intern_string (attr));
      }
    }

    g_strfreev (ppstr);
  }

  g_clear_error (&err);

} /* i_vars_libgeda_set() */



/*! \brief Free default variables.
 */
void i_vars_libgeda_freenames()
{
}
