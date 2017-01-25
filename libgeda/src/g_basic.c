/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
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
#include <sys/stat.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "libgeda_priv.h"
#include "libgedaguile.h"

static void process_error_stack (SCM s_stack, SCM s_key, SCM s_args, GError **err);

/* Pre-unwind handler called in the context in which the exception was
 * thrown. */
static SCM protected_pre_unwind_handler (void *data, SCM key, SCM args)
{
  /* Capture the stack trace */
  *((SCM *) data) = scm_make_stack (SCM_BOOL_T, SCM_EOL);

  return SCM_BOOL_T;
}

/* Post-unwind handler called in the context of the catch expression.
 * This actually does the work of parsing the stack and generating log
 * messages. */
static SCM protected_post_unwind_handler (void *data, SCM key, SCM args)
{ 
  /* The stack was captured pre-unwind */
  SCM s_stack = *(SCM *) data;

  process_error_stack (s_stack, key, args, NULL);

  return SCM_BOOL_F;
}

/* Actually carries out evaluation for protected eval */
static SCM protected_body_eval (void *data)
{
  SCM args = *((SCM *)data);
  return scm_eval (scm_car (args), scm_cadr (args));
}

/*! \brief Evaluate a Scheme expression safely.
 *  \par Function Description
 *
 *  Often a libgeda program (or libgeda itself) will need to call out
 *  to Scheme code, for example to load a Scheme initialisation (RC) file.
 *  If an error or exception caused by such code goes uncaught, it
 *  locks up the Scheme interpreter, stopping any further Scheme code
 *  from being run until the program is restarted.
 *
 *  This function is equivalent to scm_eval (), with the important
 *  difference that any errors or exceptions caused by the evaluated
 *  expression \a exp are caught and reported via the libgeda logging
 *  mechanism.  If an error occurs during evaluation, this function
 *  returns SCM_BOOL_F.  If \a module_or_state is undefined, uses the
 *  current interaction environment.
 *
 *  \param exp             Expression to evaluate
 *  \param module_or_state Environment in which to evaluate \a exp
 *
 *  \returns Evaluation results or SCM_BOOL_F if exception caught.
 */
SCM g_scm_eval_protected (SCM exp, SCM module_or_state)
{
  SCM stack = SCM_BOOL_T;
  SCM body_data;
  SCM result;

  if (scm_is_eq (module_or_state, SCM_UNDEFINED)) {
    body_data = scm_list_2 (exp, scm_interaction_environment ());
  } else {
    body_data = scm_list_2 (exp, module_or_state);
  }

  result = scm_c_catch (SCM_BOOL_T,
                        protected_body_eval,           /* catch body */
                        &body_data,                    /* body data */
                        protected_post_unwind_handler, /* post handler */
                        &stack,                        /* post data */
                        protected_pre_unwind_handler,  /* pre handler */
                        &stack                         /* pre data */
                        );

  scm_remember_upto_here_2 (body_data, stack);

  return result;
}

/*! \brief Evaluate a C string as a Scheme expression safely
 *  \par Function Description
 *
 *  Evaluates a C string like scm_c_eval_string().  Simple wrapper for
 *  g_scm_eval_string_protected().
 *
 *  \param str  String to evaluate.
 *
 *  \returns Evaluation results or SCM_BOOL_F if exception caught.
 */
SCM g_scm_c_eval_string_protected (const gchar *str) {
  SCM s_str;
  g_return_val_if_fail ((str != NULL), SCM_BOOL_F);
  s_str = scm_from_utf8_string (str);
  return g_scm_eval_string_protected (s_str);
}

/*! \brief Evaluate a string as a Scheme expression safely
 *  \par Function Description
 *
 *  Evaluates a string similarly to scm_eval_string(), but catching
 *  any errors or exceptions and reporting them via the libgeda
 *  logging mechanism.
 *
 *  See also g_scm_eval_protected() and g_scm_c_eval_string_protected().
 *
 *  \param str  String to evaluate.
 *
 *  \returns Evaluation results or SCM_BOOL_F if exception caught.
 */
SCM g_scm_eval_string_protected (SCM str)
{
  SCM expr = scm_list_2 (scm_from_utf8_symbol ("eval-string"),
                         str);

  return g_scm_eval_protected (expr, SCM_UNDEFINED);
}

/* Data to be passed to g_read_file()'s worker functions. */
struct g_read_file_data_t
{
  SCM stack;
  SCM filename;
  GError *err;
};

/* Body function for g_read_file(). Simply loads the specified
 * file. */
SCM
g_read_file__body (struct g_read_file_data_t *data)
{
  return scm_primitive_load (data->filename);
}

/* Post-unwind handler for g_read_file(). Processes the stack captured
 * in the pre-unwind handler. */
SCM
g_read_file__post_handler (struct g_read_file_data_t *data, SCM key, SCM args)
{
  process_error_stack (data->stack, key, args, &data->err);
  return SCM_BOOL_F;
}

/* Pre-unwind handler for g_read_file().  Captures the Guile stack for
 * processing in the post-unwind handler. */
SCM
g_read_file__pre_handler (struct g_read_file_data_t *data, SCM key, SCM args)
{
  data->stack = scm_make_stack (SCM_BOOL_T, SCM_EOL);
  return SCM_BOOL_F;
}

/*! \brief Load a Scheme file, catching and logging errors.
 * \par Function Description
 * Loads \a filename, catching any uncaught errors and logging them.
 *
 * \bug Most other functions in the libgeda API return TRUE on success
 * and FALSE on failure. g_read_file() shouldn't be an exception.
 *
 * \param toplevel  The TOPLEVEL structure.
 * \param filename  The file name of the Scheme file to load.
 * \param err       Return location for errors, or NULL.
 *  \return TRUE on success, FALSE on failure.
 */
gboolean
g_read_file(TOPLEVEL *toplevel, const gchar *filename, GError **err)
{
  struct g_read_file_data_t data;

  g_return_val_if_fail ((filename != NULL), FALSE);

  data.stack = SCM_BOOL_F;
  data.filename = scm_from_utf8_string (filename);
  data.err = NULL;

  scm_dynwind_begin (SCM_F_DYNWIND_REWINDABLE);
  edascm_dynwind_toplevel (toplevel);

  scm_c_catch (SCM_BOOL_T,
               (scm_t_catch_body) g_read_file__body, &data,
               (scm_t_catch_handler) g_read_file__post_handler, &data,
               (scm_t_catch_handler) g_read_file__pre_handler, &data);

  scm_dynwind_end ();

  /* If no error occurred, indicate success. */
  if (data.err == NULL) return TRUE;

  g_propagate_error (err, data.err);
  return FALSE;
}


/*! \brief Process a Scheme error into the log and/or a GError
 * \par Function Description
 * Process a captured Guile exception with the given \a s_key and \a
 * s_args, and optionally the stack trace \a s_stack.  The stack trace
 * and source location are logged, and if a GError return location \a
 * err is provided, it is populated with an informative error message.
 */
static void
process_error_stack (SCM s_stack, SCM s_key, SCM s_args, GError **err) {
  char *long_message;
  char *short_message;
  SCM s_port, s_subr, s_message, s_message_args, s_rest;
  SCM s_location = SCM_BOOL_F;

  /* Split s_args up */
  s_rest = s_args;
  s_subr = scm_car (s_rest);         s_rest = scm_cdr (s_rest);
  s_message = scm_car (s_rest);      s_rest = scm_cdr (s_rest);
  s_message_args = scm_car (s_rest); s_rest = scm_cdr (s_rest);

  /* Capture short error message */
  s_port = scm_open_output_string ();
  scm_display_error_message (s_message, s_message_args, s_port);
  short_message = scm_to_utf8_string (scm_get_output_string (s_port));
  scm_close_output_port (s_port);

  /* Capture long error message (including possible backtrace) */
  s_port = scm_open_output_string ();
  if (scm_is_true (scm_stack_p (s_stack))) {
    scm_puts (_("\nBacktrace:\n"), s_port);
    scm_display_backtrace (s_stack, s_port, SCM_BOOL_F, SCM_BOOL_F);
    scm_puts ("\n", s_port);
  }

#ifdef HAVE_SCM_DISPLAY_ERROR_STACK
  s_location = s_stack;
#endif /* HAVE_SCM_DISPLAY_ERROR_STACK */
#ifdef HAVE_SCM_DISPLAY_ERROR_FRAME
  s_location =
    scm_is_true (s_stack) ? scm_stack_ref (s_stack, SCM_INUM0) : SCM_BOOL_F;
#endif /* HAVE_SCM_DISPLAY_ERROR_FRAME */

  scm_display_error (s_location, s_port, s_subr,
                     s_message, s_message_args, s_rest);

  long_message = scm_to_utf8_string (scm_get_output_string (s_port));
  scm_close_output_port (s_port);

  /* Send long message to log */
  s_log_message ("%s", long_message);

  /* Populate any GError */
  g_set_error (err, EDA_ERROR, EDA_ERROR_SCHEME, "%s", short_message);
}
