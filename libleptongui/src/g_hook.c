/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2024 Lepton EDA Contributors
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
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <math.h>

#include "schematic.h"


/*! \brief Gets a Scheme hook object by name.
 * \par Function Description
 * Returns the contents of variable with the given name in the
 * module (schematic hook).  Used for looking up hook objects.
 *
 * \param name name of hook to lookup.
 * \return value found in the (schematic hook) module.
 */
static SCM
g_get_hook_by_name (const char *name)
{
  SCM exp = scm_list_3 (scm_from_utf8_symbol ("@"),
                        scm_list_2 (scm_from_utf8_symbol ("schematic"),
                                    scm_from_utf8_symbol ("hook")),
                        scm_from_utf8_symbol (name));
  return g_scm_eval_protected (exp, SCM_UNDEFINED);
}

/*! \brief Runs a object hook for a list of objects.
 * \par Function Description
 * Runs a hook called \a name, which should expect a list of \c
 * LeptonObject smobs as its argument, with \a obj_lst as the
 * argument list.
 *
 * \see g_run_hook_object()
 *
 * \param w_current The current #SchematicWindow object.
 * \param name    name of hook to run.
 * \param obj_lst list of \c LeptonObject smobs as hook argument.
 */
void
g_run_hook_object_list (SchematicWindow *w_current,
                        const char *name,
                        GList *obj_lst)
{
  SCM lst = SCM_EOL;
  GList *iter;

  scm_dynwind_begin ((scm_t_dynwind_flags) 0);
  g_dynwind_window (w_current);

  for (iter = obj_lst; iter != NULL; iter = g_list_next (iter)) {
    lst = scm_cons (scm_from_pointer ((LeptonObject *) iter->data, NULL), lst);
  }
  SCM expr = scm_list_3 (scm_from_utf8_symbol ("run-hook"),
                         g_get_hook_by_name (name),
                         scm_list_3 (scm_from_utf8_symbol ("map"),
                                     scm_from_utf8_symbol ("pointer->object"),
                                     scm_cons (scm_from_utf8_symbol ("list"),
                                               scm_reverse_x (lst, SCM_EOL))));

  g_scm_eval_protected (expr, scm_interaction_environment ());
  scm_dynwind_end ();
  scm_remember_upto_here_1 (expr);
}

/*! \brief Runs a object hook with a single LeptonObject.
 * \par Function Description
 * Runs a hook called \a name, which should expect a list of \c LeptonObject
 * smobs as its argument, with a single-element list containing only \a obj.
 *
 * \see g_run_hook_object_list()
 *
 * \param w_current The current #SchematicWindow object.
 * \param name name of hook to run.
 * \param obj  \c LeptonObject argument for hook.
 */
void
g_run_hook_object (SchematicWindow *w_current,
                   const char *name,
                   LeptonObject *obj)
{
  scm_dynwind_begin ((scm_t_dynwind_flags) 0);
  g_dynwind_window (w_current);

  SCM expr = scm_list_3 (scm_from_utf8_symbol ("run-hook"),
                         g_get_hook_by_name (name),
                         scm_list_2 (scm_from_utf8_symbol ("list"),
                                     scm_list_2 (scm_from_utf8_symbol ("pointer->object"),
                                                 scm_from_pointer (obj, NULL))));

  g_scm_eval_protected (expr, scm_interaction_environment ());
  scm_dynwind_end ();
  scm_remember_upto_here_1 (expr);
}

/*! \brief Runs a page hook.
 * \par Function Description
 * Runs a hook called \a name, which should expect the single
 * \c LeptonPage \a page as its argument.
 *
 * \param w_current The current #SchematicWindow object.
 * \param name name of hook to run
 * \param page \c LeptonPage argument for hook.
 */
void
g_run_hook_page (SchematicWindow *w_current,
                 const char *name,
                 LeptonPage *page)
{
  scm_dynwind_begin ((scm_t_dynwind_flags) 0);
  g_dynwind_window (w_current);

  SCM expr = scm_list_3 (scm_from_utf8_symbol ("run-hook"),
                         g_get_hook_by_name (name),
                         scm_list_2 (scm_from_utf8_symbol ("pointer->page"),
                                     scm_from_pointer (page, NULL)));

  g_scm_eval_protected (expr, scm_interaction_environment ());
  scm_dynwind_end ();
  scm_remember_upto_here_1 (expr);
}

/*! \brief Runs a change action mode hook.
 * \par Function Description
 * Runs a hook called \a name, which should expect the single \a
 * action_mode as its argument.
 *
 * \param [in] w_current The current #SchematicWindow environment.
 * \param [in] name The name of hook to run.
 * \param [in] action_mode The mode argument for hook.
 */
void
g_run_hook_action_mode (SchematicWindow *w_current,
                        const gchar *name,
                        const gchar *action_mode)
{
  scm_dynwind_begin ((scm_t_dynwind_flags) 0);
  g_dynwind_window (w_current);

  SCM expr = scm_list_3 (scm_from_utf8_symbol ("run-hook"),
                         g_get_hook_by_name (name),
                         scm_list_2 (scm_from_utf8_symbol ("quote"),
                                     scm_from_utf8_symbol (action_mode)));

  g_scm_eval_protected (expr, scm_interaction_environment ());
  scm_dynwind_end ();
  scm_remember_upto_here_1 (expr);
}
