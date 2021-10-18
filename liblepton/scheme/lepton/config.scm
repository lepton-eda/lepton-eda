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
  #:use-module (ice-9 optargs) ; for define*-public
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
            config-remove-key!
            config-remove-group!
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


( define*-public ( config-load! cfg #:key (force-load #f) )
  ( %config-load! cfg force-load )
)

(define-public config-loaded? %config-loaded?)
(define-public config-save! %config-save!)
(define-public config-changed? %config-changed?)
(define-public config-parent %config-parent)
(define-public set-config-parent! %set-config-parent!)
(define-public config-trusted? %config-trusted?)
(define-public set-config-trusted! %set-config-trusted!)

(define-public (config-trusted-context cfg)
  (cond
   ((not cfg) #f)
   ((config-trusted? cfg) cfg)
   (else (config-trusted-context (config-parent cfg)))))

(define-public config-groups %config-groups)
(define-public config-has-group? %config-has-group?)
(define-public config-keys %config-keys)

(define-public (config-has-key? cfg group key)
  (false-if-exception
   (begin (config-source cfg group key)
          #t)))

(define-public (config-inherited? cfg group key)
  (not (equal? cfg (config-source cfg group key))))

(define-public config-source %config-source)
(define-public config-string %config-string)
(define-public config-boolean %config-boolean)
(define-public config-int %config-int)
(define-public config-real %config-real)
(define-public config-string-list %config-string-list)
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
