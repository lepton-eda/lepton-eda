/* Lepton EDA Schematic Capture
 * Copyright (C) 2013, 2016 Peter Brett <peter@peter-b.co.uk>
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

/*! \file g_action.c
 * \brief Functions relating to working with lepton-schematic actions.
 */

#include <config.h>

#include "gschem.h"

/*! \brief Evaluate a lepton-schematic action by name.
 * \par Function Description
 * Evaluates the action named \a action_name, which should be a UTF-8
 * string naming a symbol in the user module.  If evaluating the
 * action fails, prints a message to the log and returns FALSE;
 * otherwise, returns TRUE.
 *
 * \param w_current    Current lepton-schematic toplevel structure.
 * \param action_name  Name of action to evaluate.
 *
 * \return TRUE on success, FALSE on failure.
 */
gboolean
g_action_eval_by_name (GschemToplevel *w_current, const gchar *action_name)
{
  SCM s_eval_action_proc;
  SCM s_expr;
  SCM s_result;
  gboolean result;

  g_assert (w_current);
  g_assert (action_name);

  scm_dynwind_begin ((scm_t_dynwind_flags) 0);
  g_dynwind_window (w_current);

  /* Get the eval-action procedure */
  s_eval_action_proc =
          scm_variable_ref (scm_c_public_variable ("schematic action",
                                                   "eval-action!"));
  /* Build expression to evaluate */
  s_expr = scm_list_2 (s_eval_action_proc,
                       scm_list_2 (scm_from_utf8_symbol ("quote"),
                                   scm_from_utf8_symbol (action_name)));
  /* Evaluate and get return value */
  s_result = g_scm_eval_protected (s_expr, SCM_UNDEFINED);
  result = scm_is_true (s_result);

  scm_dynwind_end ();
  return result;
}
