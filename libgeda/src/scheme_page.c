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
 * \file scheme_page.c
 * \brief Scheme API page manipulation procedures.
 */

#include <config.h>

#include "libgeda_priv.h"
#include "libgedaguile_priv.h"

/*! \brief Get a of open pages.
 * \par Function Description
 * Retrieves a Scheme list of currently-opened pages.
 *
 * \note Scheme API: Implements the %active-pages procedure of the
 * (geda core page) module.
 *
 * \return a Scheme list of #PAGE smobs.
 */
SCM_DEFINE (active_pages, "%active-pages", 0, 0, 0,
            (), "Retrieve a list of currently-opened pages")
{
  TOPLEVEL *toplevel = edascm_c_current_toplevel ();
  SCM lst = SCM_EOL;
  SCM rlst;
  GList *page_list = geda_list_get_glist (toplevel->pages);

  while (page_list != NULL) {
    lst = scm_cons (edascm_from_page (page_list->data), lst);
    page_list = g_list_next (page_list);
  }

  rlst = scm_reverse (lst);
  scm_remember_upto_here_1 (lst);
  return rlst;
}

/*! \brief Create a new page.
 * \par Function Description
 * Creates and initialises a new #PAGE structure associated with the
 * filename \a filename_s. Note that this does not check that a file
 * exists with that name, or attempt to load any data from it.
 *
 * \note Scheme API: Implements the %new-page procedure of the (geda
 * core page) module.
 *
 * \return a newly-created #PAGE smob.
 */
SCM_DEFINE (new_page, "%new-page", 1, 0, 0,
            (SCM filename_s), "Create a new page")
{
  TOPLEVEL *toplevel = edascm_c_current_toplevel ();
  char *filename;
  PAGE *page;

  /* Ensure that the argument is a string */
  SCM_ASSERT (scm_is_string (filename_s), filename_s,
              SCM_ARG1, s_new_page);

  filename = scm_to_utf8_string (filename_s);
  page = s_page_new (toplevel, filename);
  g_free (filename);

  return edascm_from_page (page);
}

/*! \brief Close a page
 * \par Function Description

 * Destroys the #PAGE structure \a page_s, freeing all of its
 * resources.  Attempting to use \a page_s after calling this function
 * will cause an error.
 *
 * \note Scheme API: Implements the %close-page procedure of the (geda
 * core page) module.
 *
 * \param page_s The page to close.
 * \return SCM_UNDEFINED.
 */
SCM_DEFINE (close_page_x, "%close-page!", 1, 0, 0,
            (SCM page_s), "Close a page.")
{
  /* Ensure that the argument is a page smob */
  SCM_ASSERT (EDASCM_PAGEP (page_s), page_s,
              SCM_ARG1, s_close_page_x);

  TOPLEVEL *toplevel = edascm_c_current_toplevel ();
  PAGE *page = edascm_to_page (page_s);

  s_page_delete (toplevel, page);

  return SCM_UNDEFINED;
}

/*! \brief Get the filename associated with a page.
 * \par Function Description
 * Retrieves the filename associated with the #PAGE smob \a page_s.
 *
 * \note Scheme API: Implements the %page-filename procedure of the
 * (geda core page) module.
 *
 * \return a Scheme string containing the page filename.
 */
SCM_DEFINE (page_filename, "%page-filename", 1, 0, 0,
            (SCM page_s), "Get a page's associated filename")
{
  PAGE *page;

  /* Ensure that the argument is a page smob */
  SCM_ASSERT (EDASCM_PAGEP (page_s), page_s,
              SCM_ARG1, s_page_filename);


  page = edascm_to_page (page_s);
  return scm_from_utf8_string (page->page_filename);
}

/*! \brief Change the filename associated with a page.
 * \par Function Description
 * Sets the filename associated with the #PAGE smob \a page_s.
 *
 * \note Scheme API: Implements the %set-page-filename! procedure of
 * the (geda core page) module.
 *
 * \param page_s page to set filename for.
 * \param filename_s new filename for \a page.
 * \return \a page.
 */
SCM_DEFINE (set_page_filename_x, "%set-page-filename!", 2, 0, 0,
            (SCM page_s, SCM filename_s), "Set a page's associated filename")
{
  SCM_ASSERT (EDASCM_PAGEP (page_s), page_s,
              SCM_ARG1, s_set_page_filename_x);
  SCM_ASSERT (scm_is_string (filename_s), filename_s,
              SCM_ARG2, s_set_page_filename_x);

  PAGE *page = edascm_to_page (page_s);
  char *new_fn = scm_to_utf8_string (filename_s);
  if (page->page_filename != NULL) {
    g_free (page->page_filename);
  }
  page->page_filename = g_strdup (new_fn);
  free (new_fn);

  return page_s;
}

/*! \brief Get a list of objects in a page.
 * \par Function Description
 * Retrieves the contents of a the #PAGE smob \a page_s as a Scheme
 * list of #OBJECT smobs.
 *
 * \note Scheme API: Implements the %page-contents procedure of the
 * (geda core page) module.
 *
 * \return a list of #OBJECT smobs.
 */
SCM_DEFINE (page_contents, "%page-contents", 1, 0, 0,
            (SCM page_s), "Get a page's contents.")
{
  PAGE *page;

  /* Ensure that the argument is a page smob */
  SCM_ASSERT (EDASCM_PAGEP (page_s), page_s,
              SCM_ARG1, s_page_contents);

  page = edascm_to_page (page_s);

  return edascm_from_object_glist (s_page_objects (page));
}

/*! \brief Get the page an object belongs to.
 * \par Function Description
 * Returns a smob for the #PAGE that \a obj_s belongs to.  If \a obj_s
 * does not belong to a #PAGE, returns SCM_BOOL_F.
 *
 * \note Scheme API: Implements the %object-page procedure in the
 * (geda core page) module.
 *
 * \param [in] obj_s an #OBJECT smob.
 * \return a #PAGE smob or SCM_BOOL_F.
 */
SCM_DEFINE (object_page, "%object-page", 1, 0, 0,
            (SCM obj_s), "Get the page that an object smob belongs to")
{
  SCM_ASSERT (EDASCM_OBJECTP (obj_s), obj_s,
              SCM_ARG1, s_object_page);

  PAGE *page = o_get_page (edascm_c_current_toplevel (),
                           edascm_to_object (obj_s));

  if (page != NULL) {
    return edascm_from_page (page);
  } else {
    return SCM_BOOL_F;
  }
}


/*! \brief Add an object to a page.
 * \par Function Description
 * Adds \a obj_s to \a page_s.  If \a obj_s is already attached to a
 * #PAGE or to a complex #OBJECT, throws a Scheme error.
 *
 * \note Scheme API: Implements the %page-append! procedure of the
 * (geda core page) module.
 *
 * \return \a page_s.
 */
SCM_DEFINE (page_append_x, "%page-append!", 2, 0, 0,
            (SCM page_s, SCM obj_s), "Add an object to a page.")
{
  /* Ensure that the arguments have the correct types. */
  SCM_ASSERT (EDASCM_PAGEP (page_s), page_s,
              SCM_ARG1, s_page_append_x);
  SCM_ASSERT (EDASCM_OBJECTP (obj_s), obj_s,
              SCM_ARG2, s_page_append_x);

  PAGE *page = edascm_to_page (page_s);
  OBJECT *obj = edascm_to_object (obj_s);
  TOPLEVEL *toplevel = edascm_c_current_toplevel ();

  /* Check that the object isn't already attached to something. */
  PAGE *curr_page = o_get_page (toplevel, obj);
  if (((curr_page != NULL) && (curr_page != page))
      || (obj->parent != NULL)) {
    scm_error (edascm_object_state_sym, s_page_append_x,
               _("Object ~A is already attached to something"),
               scm_list_1 (obj_s), SCM_EOL);
  }

  if (curr_page == page) return obj_s;

  /* Object cleanup now managed by C code. */
  edascm_c_set_gc (obj_s, 0);
  o_emit_pre_change_notify (toplevel, obj);
  s_page_append (edascm_c_current_toplevel (), page, obj);
  o_emit_change_notify (toplevel, obj);
  page->CHANGED = 1; /* Ugh. */

  return page_s;
}

/*! \brief Remove an object from a page.
 * \par Function Description
 * Removes \a obj_s from \a page_s.  If \a obj_s is attached to a
 * #PAGE other than \a page_s, or to a complex #OBJECT, throws a
 * Scheme error. If \a obj_s is not attached to a page, does nothing.
 *
 * \note Scheme API: Implements the %page-remove! procedure of the
 * (geda core page) module.
 *
 * \return \a page_s.
 */
SCM_DEFINE (page_remove_x, "%page-remove!", 2, 0, 0,
            (SCM page_s, SCM obj_s), "Remove an object from a page.")
{
  /* Ensure that the arguments have the correct types. */
  SCM_ASSERT (EDASCM_PAGEP (page_s), page_s,
              SCM_ARG1, s_page_remove_x);
  SCM_ASSERT (EDASCM_OBJECTP (obj_s), obj_s,
              SCM_ARG2, s_page_remove_x);

  PAGE *page = edascm_to_page (page_s);
  OBJECT *obj = edascm_to_object (obj_s);
  TOPLEVEL *toplevel = edascm_c_current_toplevel ();

  /* Check that the object is not attached to something else. */
  PAGE *curr_page = o_get_page (toplevel, obj);
  if ((curr_page != NULL && curr_page != page)
      || (obj->parent != NULL)) {
    scm_error (edascm_object_state_sym, s_page_remove_x,
               _("Object ~A is attached to a complex or different page"),
               scm_list_1 (obj_s), SCM_EOL);
  }

  /* Check that object is not attached as an attribute. */
  if (obj->attached_to != NULL) {
    scm_error (edascm_object_state_sym, s_page_remove_x,
               _("Object ~A is attached as an attribute"),
               scm_list_1 (obj_s), SCM_EOL);
  }

  /* Check that object doesn't have attributes. */
  if (obj->attribs != NULL) {
    scm_error (edascm_object_state_sym, s_page_remove_x,
               _("Object ~A has attributes"),
               scm_list_1 (obj_s), SCM_EOL);
  }

  if (curr_page == NULL) return obj_s;

  o_emit_pre_change_notify (toplevel, obj);
  s_page_remove (toplevel, page, obj);
  page->CHANGED = 1; /* Ugh. */
  /* If the object is currently selected, unselect it. */
  o_selection_remove (toplevel, page->selection_list, obj);
  o_emit_change_notify (toplevel, obj);

  /* Object cleanup now managed by Guile. */
  edascm_c_set_gc (obj_s, 1);
  return page_s;
}

/*! \brief Check whether a page has been flagged as changed.
 * \par Function Description
 * Returns SCM_BOOL_T if \a page_s has been flagged as having been
 * modified.
 *
 * \note Scheme API: Implements the %page-dirty? procedure of the
 * (geda core page) module.
 *
 * \param page_s page to inspect.
 * \return SCM_BOOL_T if page is dirtied, otherwise SCM_BOOL_F.
 */
SCM_DEFINE (page_dirty, "%page-dirty?", 1, 0, 0,
            (SCM page_s), "Check whether a page has been flagged as changed.")
{
  /* Ensure that the argument is a page smob */
  SCM_ASSERT (EDASCM_PAGEP (page_s), page_s,
              SCM_ARG1, s_page_dirty);

  PAGE *page = edascm_to_page (page_s);
  return page->CHANGED ? SCM_BOOL_T : SCM_BOOL_F;
}

/*! \brief Set a page's changed flag.
 * \par Function Description
 * If \a flag_s is true, flag \a page_s as having been modified.
 * Otherwise, clears the change flag.
 *
 * \note Scheme API: Implements the %set-page-dirty! procedure of the
 * (geda core page) module.
 *
 * \param page_s page to modify.
 * \param flag_s new flag setting.
 * \return \a page_s
 */
SCM_DEFINE (set_page_dirty_x, "%set-page-dirty!", 2, 0, 0,
            (SCM page_s, SCM flag_s),
            "Set whether a page is flagged as changed.")
{
  /* Ensure that the argument is a page smob */
  SCM_ASSERT (EDASCM_PAGEP (page_s), page_s,
              SCM_ARG1, s_set_page_dirty_x);

  PAGE *page = edascm_to_page (page_s);
  page->CHANGED = scm_is_true (flag_s);
  return page_s;
}

/*! \brief Create a string representation of a page.
 * \par Function Description
 * Returns a string representation of the contents of \a page_s.
 *
 * \note Scheme API: Implements the %page->string procedure of the
 * (geda core page) module.
 *
 * \param page_s page to convert to a string.
 * \return a string representation of \a page_s.
 */
SCM_DEFINE (page_to_string, "%page->string", 1, 0, 0,
            (SCM page_s),
            "Create a string representation of a page.")
{
  /* Ensure that the argument is a page smob */
  SCM_ASSERT (EDASCM_PAGEP (page_s), page_s,
              SCM_ARG1, s_page_to_string);

  PAGE *page = edascm_to_page (page_s);
  TOPLEVEL *toplevel = edascm_c_current_toplevel ();

  gchar *buf = o_save_buffer (toplevel, s_page_objects (page));
  scm_dynwind_begin (0);
  scm_dynwind_unwind_handler (g_free, buf, SCM_F_WIND_EXPLICITLY);
  SCM result = scm_from_utf8_string (buf);
  scm_dynwind_end ();
  return result;
}

/*! \brief Create a page from a string representation.
 * \par Function Description
 * Returns a page with filename \a filename_s created by parsing \a
 * str_s. Throws an error if \a str_s contains invalid gEDA file
 * format syntax.
 *
 * \note Scheme API: Implements the %string->page procedure of the
 * (geda core page) module.
 *
 * \param filename_s Filename for new page.
 * \param str_s      String to parse to create page.
 * \return a new page created by parsing \a str_s.
 */
SCM_DEFINE (string_to_page, "%string->page", 2, 0, 0,
            (SCM filename_s, SCM str_s),
            "Create a new page from a string.")
{
  /* Ensure that the arguments are strings */
  SCM_ASSERT (scm_is_string (filename_s), filename_s,
              SCM_ARG1, s_string_to_page);
  SCM_ASSERT (scm_is_string (str_s), str_s,
              SCM_ARG2, s_string_to_page);

  TOPLEVEL *toplevel = edascm_c_current_toplevel ();
  char *filename = scm_to_utf8_string (filename_s);
  PAGE *page = s_page_new (toplevel, filename);
  free (filename);

  size_t len;
  GError * err = NULL;
  char *str = scm_to_utf8_stringn (str_s, &len);
  GList *objects = o_read_buffer (toplevel, NULL, str, len,
                                  page->page_filename, &err);
  free (str);

  if (err) {
      SCM error_message = scm_from_utf8_string (err->message);

      g_error_free(err);
      scm_error (edascm_string_format_sym, s_string_to_page,
                 _("Parse error: ~s"), scm_list_1 (error_message), SCM_EOL);
  }

  s_page_append_list (toplevel, page, objects);

  return edascm_from_page (page);
}

/*!
 * \brief Create the (geda core page) Scheme module.
 * \par Function Description
 * Defines procedures in the (geda core page) module. The module can
 * be accessed using (use-modules (geda core page)).
 */
static void
init_module_geda_core_page ()
{
  /* Register the functions */
  #include "scheme_page.x"

  /* Add them to the module's public definitions. */

  scm_c_export (s_active_pages, s_new_page, s_close_page_x,
                s_page_filename, s_set_page_filename_x, s_page_contents,
                s_object_page, s_page_append_x, s_page_remove_x, s_page_dirty,
                s_set_page_dirty_x, s_page_to_string, s_string_to_page, NULL);
}

/*!
 * \brief Initialise the basic gEDA page manipulation procedures.
 * \par Function Description
 * Registers some Scheme procedures for working with #PAGE
 * smobs. Should only be called by scheme_api_init().
 */
void
edascm_init_page ()
{
  /* Define the (geda core page) module */
  scm_c_define_module ("geda core page",
                       init_module_geda_core_page,
                       NULL);
}
