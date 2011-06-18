/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2011 gEDA Contributors (see ChangeLog for details)
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
#include <missing.h>

#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <math.h>

#include "gschem.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

SCM_SYMBOL (at_sym, "@");
SCM_SYMBOL (gschem_sym, "gschem");
SCM_SYMBOL (core_sym, "core");
SCM_SYMBOL (hook_sym, "hook");
SCM_SYMBOL (run_hook_sym, "run-hook");

/*! \brief Gets a Scheme hook object by name.
 * \par Function Description
 * Returns the contents of variable with the given name in the (gschem
 * core hook).  Used for looking up hook objects.
 *
 * \param name name of hook to lookup.
 * \return value found in the (gschem core hook) module.
 */
static SCM
g_get_hook_by_name (const char *name)
{
  SCM exp = scm_list_3 (at_sym,
                        scm_list_3 (gschem_sym, core_sym, hook_sym),
                        scm_from_utf8_symbol (name));
  return g_scm_eval_protected (exp, SCM_UNDEFINED);
}

/*! \brief Runs a object hook for a list of objects.
 * \par Function Description
 * Runs a hook called \a name, which should expect a list of #OBJECT
 * smobs as its argument, with \a obj_lst as the argument list.
 *
 * \see g_run_hook_object()
 *
 * \param name    name of hook to run.
 * \param obj_lst list of #OBJECT smobs as hook argument.
 */
void
g_run_hook_object_list (const char *name, GList *obj_lst)
{
  SCM lst = SCM_EOL;
  GList *iter;
  for (iter = obj_lst; iter != NULL; iter = g_list_next (iter)) {
    lst = scm_cons (edascm_from_object ((OBJECT *) iter->data), lst);
  }
  SCM args = scm_list_1 (scm_reverse_x (lst, SCM_EOL));

  scm_run_hook (g_get_hook_by_name (name), args);
  scm_remember_upto_here_2 (lst, args);
}

/*! \brief Runs a object hook with a single OBJECT.
 * \par Function Description
 * Runs a hook called \a name, which should expect a list of #OBJECT
 * smobs as its argument, with a single-element list containing only \a obj.
 *
 * \see g_run_hook_object_list()
 *
 * \param name name of hook to run.
 * \param obj  #OBJECT argument for hook.
 */
void
g_run_hook_object (const char *name, OBJECT *obj)
{
  SCM args = scm_list_1 (scm_list_1 (edascm_from_object (obj)));
  scm_run_hook (g_get_hook_by_name (name), args);
  scm_remember_upto_here_1 (args);
}

/*! \brief Runs a page hook.
 * \par Function Description
 * Runs a hook called \a name, which should expect the single #PAGE \a
 * page as its argument.
 *
 * \param name name of hook to run
 * \param page #PAGE argument for hook.
 */
void
g_run_hook_page (const char *name, PAGE *page)
{
  SCM args = scm_list_1 (edascm_from_page (page));
  scm_run_hook (g_get_hook_by_name (name), args);
  scm_remember_upto_here_1 (args);
}

/*! \brief Create the (gschem core hook) Scheme module.
 * \par Function Description
 * Defines some hooks in the (gschem core hook) module.  These hooks
 * allow Scheme callbacks to be triggered on certain gschem actions.
 * For a description of the arguments and behaviour of these hooks,
 * please see ../scheme/gschem/hook.scm.
 */
static void
init_module_gschem_core_hook ()
{

#include "g_hook.x"

#define DEFINE_HOOK(name) \
  do { \
    scm_c_define (name, scm_make_hook (scm_from_int (1)));      \
    scm_c_export (name, NULL); \
  } while (0)

  DEFINE_HOOK ("%add-objects-hook");
  DEFINE_HOOK ("%remove-objects-hook");
  DEFINE_HOOK ("%move-objects-hook");
  DEFINE_HOOK ("%mirror-objects-hook");
  DEFINE_HOOK ("%rotate-objects-hook");
  DEFINE_HOOK ("%paste-objects-hook");
  DEFINE_HOOK ("%attach-attribs-hook");
  DEFINE_HOOK ("%detach-attribs-hook");
  DEFINE_HOOK ("%select-objects-hook");
  DEFINE_HOOK ("%deselect-objects-hook");
  DEFINE_HOOK ("%new-page-hook");
}

/*!
 * \brief Initialise the gschem hooks.
 * \par Function Description

 * Registers gschem's Guile hooks for various events.. Should only be
 * called by main_prog().
 */
void
g_init_hook ()
{
  /* Define the (gschem core hook) module */
  scm_c_define_module ("gschem core hook",
                       init_module_gschem_core_hook,
                       NULL);
}
