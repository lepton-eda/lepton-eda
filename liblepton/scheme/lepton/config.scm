;;; Lepton EDA
;;; liblepton - Lepton's library - Scheme API
;;; Copyright (C) 2011-2012 Peter Brett <peter@peter-b.co.uk>
;;; Copyright (C) 2017-2021 Lepton EDA Contributors
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

(define-module (lepton config)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)

  ; Import C procedures
  #:use-module (lepton core config)
  #:use-module (lepton ffi)
  #:use-module (lepton config foreign)

  #:export (config?
            default-config-context
            system-config-context
            user-config-context
            path-config-context
            cache-config-context
            config-filename
            config-load!
            config-loaded?
            config-save!
            config-changed?
            config-parent
            set-config-parent!
            config-trusted?
            set-config-trusted!
            config-has-group?
            config-remove-key!
            config-remove-group!
            config-groups
            config-keys
            config-source
            config-string
            config-boolean
            config-int
            config-real
            config-string-list
            config-legacy-mode?
            config-set-legacy-mode!
            anyfile-config-context))

;;; Convert a GError to a Scheme error.
;;; Raise a 'config-error Scheme exception for the given error,
;;; with the procedure name subr. The error will be freed with
;;; g_clear_error().  The error will be converted to a Scheme
;;; error according to the following rules:
(define (gerror-error *error proc-name)
  (define (gerror-list *err)
    ;; GError struct consists of:
    ;; GQuark (uint32) domain
    ;; gint (int) code
    ;; gchar* (char*) message
    (parse-c-struct *err (list uint32 int '*)))

  (define (gerror-message *err)
    (match (gerror-list *err)
      ((domain code message)
       (pointer->string message))
      (_ #f)))

  (unless (null-pointer? *error)
    (let ((*err (dereference-pointer *error)))
      (unless (null-pointer? *err)
        (let ((type (string->symbol (pointer->string (config_error_type *error))))
              (code (string->symbol (pointer->string (config_error_code *error))))
              (message (gerror-message *err)))
          (g_clear_error *error)
          (scm-error type
                     proc-name
                     message
                     '()
                     (if (eq? code 'unknown)
                         #f
                         (list code))))))))

(define (config? config)
  "Returns #t if PAGE is a #<geda-config> instance, otherwise
returns #f."
  (true? (edascm_is_config (scm->pointer config))))

(define (default-config-context)
  "Returns the default configuration context."
  (pointer->geda-config (eda_config_get_default_context)))


(define (system-config-context)
  "Returns the system configuration context."
  (pointer->geda-config (eda_config_get_system_context)))


(define (user-config-context)
  "Returns the user configuration context."
  (pointer->geda-config (eda_config_get_user_context)))


(define (path-config-context path)
  "Returns configuration context for PATH."
  (check-string path 1)

  (pointer->geda-config
   (eda_config_get_context_for_path (string->pointer path))))



(define* (anyfile-config-context path #:key (parent #f) (trusted #f))
  "Returns configuration context for a given configuration file
specified as PATH.  PARENT is used as its parent context if it is
set.  If TRUSTED is not #f the context is marked as trusted."
  (define path-pointer (if path
                           (and (check-string path 1)
                                (string->pointer path))
                           %null-pointer))
  (define parent-pointer (if parent
                             (geda-config->pointer* parent 2)
                             %null-pointer))
  (check-boolean trusted 3)

  (pointer->geda-config
   (eda_config_get_anyfile_context path-pointer
                                   parent-pointer
                                   (if trusted TRUE FALSE))))



(define (cache-config-context)
  "Returns the cache configuration context."
  (pointer->geda-config (eda_config_get_cache_context)))


(define (config-filename config)
  "Returns the underlying filename for the configuration context
CONFIG, or #f if it has no filename associated with it."
  (define *cfg (geda-config->pointer* config 1))

  (let ((*path (eda_config_get_filename *cfg)))
    (and (not (null-pointer? *path))
         (pointer->string *path))))


(define* (config-load! config #:key (force-load #f))
  "Attempts to load configuration parameters for CONFIG from the
file associated with it.  Raises 'system-error on failure.  If
FORCE-LOAD is not #f, forces configuration loading even if it has
been already loaded.  Returns CONFIG."
  (define *cfg (geda-config->pointer* config 1))
  (check-boolean force-load 2)

  (when (or (not (true? (eda_config_is_loaded *cfg)))
            force-load)
    (let ((*error (bytevector->pointer (make-bytevector (sizeof '*) 0))))
      (unless (true? (eda_config_load *cfg *error))
        (if (true? (config_error_file_not_found (dereference-pointer *error)))
            ;; Missing configuration file is not an error.
            (g_clear_error *error)
            (gerror-error *error 'config-load!)))))
  config)


(define (config-loaded? config)
  "Returns #t if CONFIG has been loaded from file at some point,
and #f otherwise."
  (define *cfg (geda-config->pointer* config 1))

  (true? (eda_config_is_loaded *cfg)))


(define (config-save! config)
  "Attempts to save configuration parameters for the context
CONFIG to its ssociated file.  Raises a system-error on failure."
  (define *cfg (geda-config->pointer* config 1))

  (let ((*error (bytevector->pointer (make-bytevector (sizeof '*) 0))))
    (unless (true? (eda_config_save *cfg *error))
      (gerror-error *error 'config-save!))
    config))


(define (config-changed? config)
  "Determine whether the configuration context CONFIG has been
altered since it was last synchronised with the on-disk version by
loading or saving it.  Returns #t if CONFIG has unsaved changes,
#f otherwise."
  (define *cfg (geda-config->pointer* config 1))

  (true? (eda_config_is_changed *cfg)))


(define (config-parent config)
  "Return the parent context of the configuration context CONFIG,
if it has one, or #f otherwise."
  (define *cfg (geda-config->pointer* config 1))

  (let ((*parent (eda_config_get_parent *cfg)))
    (and (not (null-pointer? *parent))
         (pointer->geda-config *parent))))


(define (set-config-parent! config parent)
  "Set the parent context of the configuration context CONFIG to
PARENT.  If PARENT is #f, sets CONFIG as having no parent context.
Returns CONFIG."
  (define *cfg (geda-config->pointer* config 1))
  (define *parent (if parent
                      (geda-config->pointer* parent 2)
                      %null-pointer))

  (eda_config_set_parent *cfg *parent)
  config)


(define (config-trusted? config)
  "Tests if CONFIG is a \"trusted\" configuration context (i.e. if
it is permitted as a source for risky configuration parameters
such as system commands).  Returns #t if CONFIG is trusted, #f
otherwise."
  (define *cfg (geda-config->pointer* config 1))

  (true? (eda_config_is_trusted *cfg)))


(define (set-config-trusted! config trusted?)
  "Set whether the configuration context CONFIG is trusted as a
source for risky configuration parameters depending on the boolean
value of TRUSTED?.  Returns CONFIG."
  (define *cfg (geda-config->pointer* config 1))
  (check-boolean trusted? 2)

  (eda_config_set_trusted *cfg (if trusted? TRUE FALSE))
  config)


(define-public (config-trusted-context cfg)
  (cond
   ((not cfg) #f)
   ((config-trusted? cfg) cfg)
   (else (config-trusted-context (config-parent cfg)))))


(define (config-groups config)
  "Returns a list of the all group names available in CONFIG and
its parent contexts."
  (define *cfg (geda-config->pointer* config 1))

  (let ((*len (bytevector->pointer (make-bytevector (sizeof int) 0))))
    (c-string-array->list (eda_config_get_groups *cfg *len))))


(define (config-has-group? config group)
  "Tests whether the configuration context CONFIG, or any of its
parent contexts, contains the GROUP.  Returns #t if CONFIG or any
ancestor contains GROUP, #f otherwise."
  (define *cfg (geda-config->pointer* config 1))
  (check-string group 2)

  (true? (eda_config_has_group *cfg (string->pointer group))))


(define (config-keys config group)
  "Returns a list of the all keys available in CONFIG and GROUP.
If the GROUP cannot be found, raises a 'config-error error."
  (define *cfg (geda-config->pointer* config 1))
  (check-string group 2)

  (let* ((*len (bytevector->pointer (make-bytevector (sizeof int) 0)))
         (*error (bytevector->pointer (make-bytevector (sizeof '*) 0)))
         (*keys (eda_config_get_keys *cfg
                                     (string->pointer group)
                                     *len
                                     *error)))
    (if (null-pointer? *keys)
        (gerror-error *error 'config-keys)
        (c-string-array->list *keys))))


(define-public (config-has-key? cfg group key)
  (false-if-exception
   (begin (config-source cfg group key)
          #t)))

(define-public (config-inherited? cfg group key)
  (not (equal? cfg (config-source cfg group key))))


(define (config-source config group key)
  "Returns the configuration context for CONFIG (either CONFIG
itself or one of its parent contexts) in which the configuration
parameter with the given GROUP and KEY has a value specified.  If
the group or key cannot be found, raises a 'config-error error."
  (define *cfg (geda-config->pointer* config 1))
  (check-string group 2)
  (check-string key 3)

  (let* ((*error (bytevector->pointer (make-bytevector (sizeof '*) 0)))
         (*src (eda_config_get_source *cfg
                                      (string->pointer group)
                                      (string->pointer key)
                                      *error)))
    (if (null-pointer? *src)
        (gerror-error *error 'config-source)
        (pointer->geda-config *src))))


(define (config-string config group key)
  "Returns the value of the configuration parameter specified by
GROUP and KEY in the configuration context CONFIG, as a string.
If the group or key cannot be found, raises a 'config-error
error."
  (define *cfg (geda-config->pointer* config 1))
  (check-string group 2)
  (check-string key 3)

  (let* ((*error (bytevector->pointer (make-bytevector (sizeof '*) 0)))
         (*value (eda_config_get_string *cfg
                                        (string->pointer group)
                                        (string->pointer key)
                                        *error)))
    (if (null-pointer? *value)
        (gerror-error *error 'config-string)
        (pointer->string *value))))


(define (config-boolean config group key)
  "Returns the value of the configuration parameter specified by
GROUP and KEY in the configuration context CONFIG, as a boolean.
If the group or key cannot be found, raises a 'config-error
error."
  (define *cfg (geda-config->pointer* config 1))
  (check-string group 2)
  (check-string key 3)

  (let* ((*error (bytevector->pointer (make-bytevector (sizeof '*) 0)))
         (value (eda_config_get_boolean *cfg
                                        (string->pointer group)
                                        (string->pointer key)
                                        *error)))
    (unless (null-pointer? *error)
      (gerror-error *error 'config-boolean))
    (true? value)))


(define (config-int config group key)
  "Returns the value of the configuration parameter specified by
GROUP and KEY in the configuration context CONFIG, as a integer.
If the group or key cannot be found, raises a 'config-error
error."
  (define *cfg (geda-config->pointer* config 1))
  (check-string group 2)
  (check-string key 3)

  (let* ((*error (bytevector->pointer (make-bytevector (sizeof '*) 0)))
         (value (eda_config_get_int *cfg
                                    (string->pointer group)
                                    (string->pointer key)
                                    *error)))
    (unless (null-pointer? *error)
      (gerror-error *error 'config-int))
    value))


(define (config-real config group key)
  "Return the value of the configuration parameter specified by
GROUP and KEY in the configuration context CONFIG, as an inexact
real number.  If the group or key cannot be found, raises a
'config-error error."
  (define *cfg (geda-config->pointer* config 1))
  (check-string group 2)
  (check-string key 3)

  (let* ((*error (bytevector->pointer (make-bytevector (sizeof '*) 0)))
         (value (eda_config_get_double *cfg
                                       (string->pointer group)
                                       (string->pointer key)
                                       *error)))
    (unless (null-pointer? *error)
      (gerror-error *error 'config-real))
    value))


(define (config-string-list config group key)
  "Returns the value of the configuration parameter specified by
GROUP and KEY in the configuration context CONFIG, as a list of
strings.  If the group or key cannot be found, raises a
'config-error error."
  (define *cfg (geda-config->pointer* config 1))
  (check-string group 2)
  (check-string key 3)

  (let* ((*len (bytevector->pointer (make-bytevector (sizeof int) 0)))
         (*error (bytevector->pointer (make-bytevector (sizeof '*) 0)))
         (*value (eda_config_get_string_list *cfg
                                             (string->pointer group)
                                             (string->pointer key)
                                             *len
                                             *error)))
    (if (null-pointer? *value)
        (gerror-error *error 'config-string-list)
        (c-string-array->list *value))))


(define-public config-boolean-list %config-boolean-list)
(define-public config-int-list %config-int-list)
(define-public config-real-list %config-real-list)
(define-public set-config! %set-config!)
(define-public add-config-event! %add-config-event!)
(define-public remove-config-event! %remove-config-event!)


(define (config-remove-key! config group key)
  "Removes the configuration parameter specified by GROUP and KEY
in the configuration context CONFIG.  Returns boolean value
indicating success or failure."
  (define *cfg (geda-config->pointer* config 1))
  (check-string group 2)
  (check-string key 3)

  (let* ((*error (bytevector->pointer (make-bytevector (sizeof '*) 0)))
         (result (true? (eda_config_remove_key *cfg
                                               (string->pointer group)
                                               (string->pointer key)
                                               *error))))
    (unless result
      (gerror-error *error 'config-remove-key!))
    result))


(define (config-remove-group! config group)
  "Remove configuration GROUP and all its parameters from
configuration context CONFIG.  Returns boolean value indicating
success or failure."
  (define *cfg (geda-config->pointer* config 1))
  (check-string group 2)

  (let* ((*error (bytevector->pointer (make-bytevector (sizeof '*) 0)))
         (result (true? (eda_config_remove_group *cfg
                                                 (string->pointer group)
                                                 *error))))
    (unless result
      (gerror-error *error 'config-remove-group!))
    result))


(define (config-legacy-mode?)
  "Return #t if legacy configuration mode is currently in use,
otherwise return #f. This function was added to assist in config
migration and not intended for the end user.  It will be removed."
  (true? (config_get_legacy_mode)))


(define (config-set-legacy-mode! legacy?)
  "If LEGACY? is #t, enables using of legacy configuration file
names, otherwise disables it.  This function is added to assist in
config migration and not intended for the end user.  It will be
removed.  Returns the config mode previously set: #t if legacy,
otherwise #f."
  (check-boolean legacy? 1)
  (let ((result (config-legacy-mode?)))
    (config_set_legacy_mode (if legacy? TRUE FALSE))
    result))
