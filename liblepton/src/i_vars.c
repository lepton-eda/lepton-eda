/* Lepton EDA library
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

#include "libgeda_priv.h"



/* \note
 * Kazu Hirata <kazu@seul.org> on July 16, 1999 - Added these absolute
 * defaults used when default_... is NULL.
 */

int   default_attribute_promotion = TRUE;
int   default_promote_invisible = FALSE;
int   default_keep_invisible = TRUE;

int   default_make_backup_files = TRUE;
int   default_net_consolidate = TRUE;
int   default_force_boundingbox = FALSE;

/* \brief Read a boolean configuration key.
 *
 * \par Function Description
 * On success, set \a result to the value of the
 * configuration key, otherwise set it to \a defval.
 *
 * \param [in]       group   Configuration group name
 * \param [in]       key     Configuration key name
 * \param [in]       defval  Default value
 * \param [in, out]  result  Result
 *
 * \return  TRUE if a specified config parameter was successfully read
 */
gboolean
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


/* \brief Read an int configuration key.
 *
 * \par Function Description
 * On success, set \a result to the value of the
 * configuration key, otherwise set it to \a defval.
 *
 * \param [in]       group   Configuration group name
 * \param [in]       key     Configuration key name
 * \param [in]       defval  Default value
 * \param [in, out]  result  Result
 *
 * \return  TRUE if a specified config parameter was successfully read
 */
gboolean
cfg_read_int (const gchar* group,
              const gchar* key,
              gint         defval,
              gint*        result)
{
  gchar*     cwd = g_get_current_dir();
  EdaConfig* cfg = eda_config_get_context_for_path (cwd);
  g_free (cwd);

  GError*  err = NULL;
  gint val = eda_config_get_int (cfg, group, key, &err);

  gboolean success = err == NULL;
  g_clear_error (&err);

  *result = success ? val : defval;
  return success;
}



gboolean
cfg_check_int_not_0 (gint val) { return val != 0; }

gboolean
cfg_check_int_greater_0 (gint val) { return val > 0; }

gboolean
cfg_check_int_greater_eq_0 (gint val) { return val >= 0; }

gboolean
cfg_check_int_text_size (gint val) { return val >= MINIMUM_TEXT_SIZE; }



/* \brief Read an int configuration key, check read value.
 *
 * \par Function Description
 * On success, set \a result to the value of the
 * configuration key, otherwise set it to \a defval.
 * Also, check read value with pfn_check() function, and
 * if it returns FALSE, reset \a result to \a defval and
 * print an error message to STDERR.
 *
 * \param [in]       group      Configuration group name
 * \param [in]       key        Configuration key name
 * \param [in]       defval     Default value
 * \param [in, out]  result     Result
 * \param [in]       pfn_check  Function to check if value is valid
 *
 * \return  TRUE if a specified config parameter was successfully read
 */
gboolean
cfg_read_int_with_check (const gchar* group,
                         const gchar* key,
                         gint         defval,
                         gint*        result,
                         gboolean     (*pfn_check)(int))
{
  gint val = 0;
  gboolean success = cfg_read_int (group, key, defval, &val);

  if (pfn_check (val))
  {
    *result = val;
  }
  else
  {
    *result = defval;
    const gchar* errmsg = _("Invalid [%s]::%s (%d) is set in configuration\n");
    fprintf (stderr, errmsg, group, key, val);
  }

  return success;
}


/* \brief Read a string configuration key, set a corresponding int option.
 *
 * \par Function Description
 * Read and compare a configuration value to the options passed
 * as an array of OptionStringInt structures - the mapping of
 * the string option values to the corresponding int values.
 * On success, set the \a result to the int value of
 * the option (if found), otherwise set it to \a defval.
 *
 * \param [in]       group      Configuration group name
 * \param [in]       key        Configuration key name
 * \param [in]       defval     Default value
 * \param [in]       vals       An array of OptionStringInt structures
 * \param [in]       nvals      A size of \a vals array
 * \param [in, out]  result     Result
 *
 * \return  TRUE if a specified config parameter was successfully read
 */
gboolean
cfg_read_string2int (const gchar* group,
                     const gchar* key,
                     gint         defval,
                     const struct OptionStringInt* vals,
                     size_t       nvals,
                     gint*        result)
{
  gchar*     cwd = g_get_current_dir();
  EdaConfig* cfg = eda_config_get_context_for_path (cwd);
  g_free (cwd);

  GError* err = NULL;
  gchar*  str = eda_config_get_string (cfg, group, key, &err);

  gboolean success = err == NULL;
  g_clear_error (&err);

  *result = defval;

  if (success)
  {
    for (size_t i = 0; i < nvals; ++i)
    {
      if ( strcmp (vals[i].str_, str) == 0 )
      {
        *result = vals[i].int_;
        break;
      }
    }

    g_free (str);
  }

  return success;

} /* cfg_read_string2int() */
