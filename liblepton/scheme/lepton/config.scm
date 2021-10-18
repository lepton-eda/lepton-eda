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
  #:use-module (ice-9 optargs) ; for define*-public
  #:use-module (system foreign)

  ; Import C procedures
  #:use-module (lepton core config)
  #:use-module (lepton ffi)
  #:use-module (lepton config foreign)

  #:export (config?
            anyfile-config-context))

(define (config? config)
  "Returns #t if PAGE is a #<geda-config> instance, otherwise
returns #f."
  (true? (edascm_is_config (scm->pointer config))))

(define-public default-config-context %default-config-context)
(define-public system-config-context %system-config-context)
(define-public user-config-context %user-config-context)
(define-public path-config-context %path-config-context)


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


(define-public cache-config-context %cache-config-context)
(define-public config-filename %config-filename)

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

(define-public config-remove-key! %config-remove-key!)
(define-public config-remove-group! %config-remove-group!)

(define-public config-set-legacy-mode! %config-set-legacy-mode!)
(define-public config-legacy-mode? %config-legacy-mode?)
