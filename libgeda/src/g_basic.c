/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2007 Ales Hvezda
 * Copyright (C) 1998-2007 gEDA Contributors (see ChangeLog for details)
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

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/* Pre-unwind handler called in the context in which the exception was
 * thrown.  Not used with Guile 1.6.x. */
#if HAVE_DECL_SCM_C_CATCH
static SCM protected_pre_unwind_handler (void *data, SCM key, SCM args)
{
  /* Capture the stack trace */
  *((SCM *) data) = scm_make_stack (SCM_BOOL_T, SCM_EOL);

  return SCM_BOOL_T;
}
#endif

/* Post-unwind handler called in the context of the catch expression.
 * This actually does the work of parsing the stack and generating log
 * messages. */
static SCM protected_post_unwind_handler (void *data, SCM key, SCM args)
{
  SCM s_stack;
#if HAVE_DECL_SCM_C_CATCH /* The stack was captured pre-unwind */
  s_stack = *(SCM *) data;
#else                        /* Get stack from magic variable */
  s_stack = scm_fluid_ref (SCM_VARIABLE_REF (scm_the_last_stack_fluid_var));
#endif /* HAVE_DECL_SCM_C_CATCH */


  char *message = NULL;
  
  /* Capture the error message */
  if (scm_list_p (scm_caddr (args)) == SCM_BOOL_T) {
    SCM s_msg = scm_simple_format (SCM_BOOL_F, 
                                   scm_cadr (args), 
                                   scm_caddr (args));
    message = scm_to_locale_string (s_msg);
  } else {
    message = scm_to_locale_string (scm_cadr (args));
  }

  /* If a stack was captured, extract debugging information */
  if (scm_stack_p (s_stack) == SCM_BOOL_T) {
    SCM s_port, s_source, s_filename, s_line_num, s_col_num;
    char *filename, *trace;

    /* Capture & log backtrace */
    s_port = scm_open_output_string();
    scm_display_backtrace (s_stack, s_port,
                           SCM_BOOL_F, SCM_BOOL_F);
    trace = scm_to_locale_string (scm_get_output_string (s_port));
    scm_close_output_port (s_port);
    s_log_message ("\n%s\n", trace);
    free (trace);
    trace = NULL;

    /* Capture & log location */
    s_source = scm_frame_source (scm_stack_ref (s_stack, scm_from_int (0)));

    s_filename = scm_source_property (s_source,
                                      scm_from_locale_symbol ("filename"));
    s_line_num = scm_source_property (s_source,
                                      scm_from_locale_symbol ("line"));
    s_col_num = scm_source_property (s_source,
                                     scm_from_locale_symbol ("column"));
    
    if (scm_is_string (s_filename)
         && scm_is_integer (s_line_num)
         && scm_is_integer (s_col_num)) {

       filename = scm_to_locale_string (s_filename);
       s_log_message (_("%s:%i:%i: %s\n"), filename,
                      scm_to_int (s_line_num),
                      scm_to_int (s_col_num), message);
       free (filename);

    } else {

      s_log_message (_("Unknown file: %s\n"), message);

    }

  } else {
    /* No stack, so can't display debugging info */
    s_log_message (_("Evaluation failed: %s\n"), message);
    s_log_message (_("Enable debugging for more detailed information\n"));
  }

  free (message);

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
 *  to Scheme code, for example to load a Scheme configuration file.
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

  if (module_or_state == SCM_UNDEFINED) {
    body_data = scm_list_2 (exp, scm_interaction_environment ());
  } else {
    body_data = scm_list_2 (exp, module_or_state);
  }

#if HAVE_DECL_SCM_C_CATCH /* Guile 1.8.x approach */
  result = scm_c_catch (SCM_BOOL_T,
                        protected_body_eval,           /* catch body */
                        &body_data,                    /* body data */
                        protected_post_unwind_handler, /* post handler */
                        &stack,                        /* post data */
                        protected_pre_unwind_handler,  /* pre handler */
                        &stack                         /* pre data */
                        );
#else                     /* Guile 1.6.x approach using magic variables */
  result =
    scm_internal_stack_catch (SCM_BOOL_T,
                              protected_body_eval,           /* catch body */
                              &body_data,                    /* body data */
                              protected_post_unwind_handler, /* post handler */
                              NULL);
#endif /* HAVE_DECL_SCM_C_CATCH */

  scm_remember_upto_here_2 (body_data, stack);

  return result;
}

/* Actually carries out evaluation for protected eval-string */
static SCM protected_body_eval_string (void *data)
{
  SCM str = *((SCM *)data);
  return scm_eval_string (str);
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
  s_str = scm_from_locale_string (str);
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
  SCM stack = SCM_BOOL_T;
  SCM result;

#if HAVE_DECL_SCM_C_CATCH /* Guile 1.8.x approach */
  result = scm_c_catch (SCM_BOOL_T,
                        protected_body_eval_string,    /* catch body */
                        &str,                          /* body data */
                        protected_post_unwind_handler, /* post handler */
                        &stack,                        /* post data */
                        protected_pre_unwind_handler,  /* pre handler */
                        &stack                         /* pre data */
                        );
#else                     /* Guile 1.6.x approach using magic variables */
  result =
    scm_internal_stack_catch (SCM_BOOL_T,
                              protected_body_eval_string,    /* catch body */
                              &str,                          /* body data */
                              protected_post_unwind_handler, /* post handler */
                              NULL);
#endif /* HAVE_DECL_SCM_C_CATCH */

  scm_remember_upto_here_1 (stack);

  return result;
}


/*! \brief Start reading a scheme file
 *  \par Function Description
 *  Start reading a scheme file
 *
 *  \param [in] filename  The file name to start reading from.
 *  \return 0 on success, -1 on failure.
 */
int g_read_file(const gchar *filename)
{
	SCM eval_result = SCM_BOOL_F;
        SCM expr;
	char * full_filename;

	if (filename == NULL) {
		return(-1);
	}

	/* get full, absolute path to file */
	full_filename = f_normalize_filename (filename, NULL);
	if (full_filename == NULL) {
		return(-1);
	}
	
	if (access(full_filename, R_OK) != 0) {
          s_log_message(_("Could not find [%s] for interpretation\n"),
                        full_filename);
		return(-1);
  	}

        expr = scm_list_2 (scm_from_locale_symbol ("load"),
                           scm_from_locale_string (full_filename));
        eval_result = g_scm_eval_protected (expr,
                                            scm_interaction_environment ());

	g_free(full_filename);

	return (eval_result != SCM_BOOL_F);
}
