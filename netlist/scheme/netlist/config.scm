;;; Lepton EDA netlister
;;; Copyright (C) 2016-2017 gEDA Contributors
;;; Copyright (C) 2017-2020 Lepton EDA Contributors
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
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;;; MA 02111-1301 USA.

(define-module (netlist config)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (geda log)
  #:use-module (lepton config)
  #:use-module (netlist option)
  #:export (gnetlist-config-ref
            print-gnetlist-config))

(define %gnetlist-config #f)

;;; Init default values
(let ((cfg (default-config-context)))

  ;; This is the default name used for nets for which the user has set
  ;; no explicit name via the netname= or net= attributes.
  (set-config! cfg "gnetlist" "default-net-name" "unnamed_net")

  ;; This is the default name used for buses for which the user has set
  ;; no explicit name via the netname= or net= attributes.
  (set-config! cfg "gnetlist" "default-bus-name" "unnamed_bus")

  ;; By default, net= attributes beat netname= attributes.
  (set-config! cfg "gnetlist" "net-naming-priority" "net-attribute")

  ;; By default, hierarchy processing is enabled.
  (set-config! cfg "gnetlist.hierarchy" "traverse-hierarchy" #t)

  ;; By default, sub-schematic attributes 'refdes' are built
  ;; accounting for the parent schematic's ones.
  (set-config! cfg "gnetlist.hierarchy" "mangle-refdes-attribute" #t)

  ;; By default, sub-schematic attributes 'refdese' are appended to the parent
  ;; schematic's ones to build hierarchical attributes 'refdes'.
  (set-config! cfg "gnetlist.hierarchy" "refdes-attribute-order" #f)

  ;; This is the default separator which is used to built hierarchical
  ;; refdeses of any component in sub-schematics.
  (set-config! cfg "gnetlist.hierarchy" "refdes-attribute-separator" "/")

  ;; By default, sub-schematic attributes 'netname' are built
  ;; accounting for the parent schematic's ones.
  (set-config! cfg "gnetlist.hierarchy" "mangle-netname-attribute" #t)

  ;; By default, sub-schematic attributes 'netname' are appended to the parent
  ;; schematic's ones to build hierarchical netnames.
  (set-config! cfg "gnetlist.hierarchy" "netname-attribute-order" #f)

  ;; This is the default separator which is used to built hierarchical
  ;; attributes 'netname' for nets in sub-schematics.
  (set-config! cfg "gnetlist.hierarchy" "netname-attribute-separator" "/")

  ;; By default, sub-schematic attributes 'net' are built
  ;; accounting for the parent schematic's ones.
  (set-config! cfg "gnetlist.hierarchy" "mangle-net-attribute" #t)

  ;; By default, sub-schematic attributes 'net' are appended to the parent
  ;; schematic's ones to build hierarchical netnames.
  (set-config! cfg "gnetlist.hierarchy" "net-attribute-order" #f)

  ;; This is the default separator which is used to built hierarchical
  ;; attributes 'net' for nets in sub-schematics.
  (set-config! cfg "gnetlist.hierarchy" "net-attribute-separator" "/"))

(define %gnetlist-config-table
  `((traverse-hierarchy    ,config-boolean "gnetlist.hierarchy" "traverse-hierarchy")
    (reverse-refdes-order  ,config-boolean "gnetlist.hierarchy" "refdes-attribute-order")
    (refdes-separator      ,config-string  "gnetlist.hierarchy" "refdes-attribute-separator")
    (mangle-refdes         ,config-boolean "gnetlist.hierarchy" "mangle-refdes-attribute")
    (reverse-net-order     ,config-boolean "gnetlist.hierarchy" "net-attribute-order")
    (mangle-net            ,config-boolean "gnetlist.hierarchy" "mangle-net-attribute")
    (net-separator         ,config-string  "gnetlist.hierarchy" "net-attribute-separator")
    (reverse-netname-order ,config-boolean "gnetlist.hierarchy" "netname-attribute-order")
    (mangle-netname        ,config-boolean "gnetlist.hierarchy" "mangle-netname-attribute")
    (netname-separator     ,config-string  "gnetlist.hierarchy" "netname-attribute-separator")
    (netname-attribute-priority
                           ,(lambda args (string=? "netname-attribute" (apply config-string args)))
                                           "gnetlist"           "net-naming-priority")
    (default-net-name      ,config-string  "gnetlist"           "default-net-name")
    (default-bus-name      ,config-string  "gnetlist"           "default-bus-name")))

;; Try to load a configuration file for gnetlist
;;
;; FIXME[2017-02-01] This should error if the load fails due to
;; anything other than "file not found"
(define (try-load-config cfg)
    (catch 'system-error
           (lambda () (config-load! cfg))
           (lambda (key subr message args rest)
             (log! 'warning "Failed to load config from ~S: ~?"
                   (config-filename cfg) message args)))
)

(define (reload-config config . args)
  (define (process info)
    (match info
           ((sym proc group key)
            (list sym (proc config group key)
                  (config-source config group key)))))

  (set! %gnetlist-config (map process %gnetlist-config-table)))

(define (gnetlist-config-ref key)
  "Returns value of gnetlist configuration KEY."
  (car (assq-ref %gnetlist-config key)))

(define (print-gnetlist-config)
  "Displays gnetlist configuration settings."
  (define (source-string cfg)
    (if (equal? cfg (default-config-context))
        "(default)"
        (string-append "[" (config-filename cfg) "]")))
  (display "Gnetlist settings:\n\n")
  (for-each
   (lambda (x)
     (match x
            ((sym value cfg)
             (format #t "~A: ~S ~A\n" sym value (source-string cfg)))))
   %gnetlist-config))

;;; Init config once
(when (not %gnetlist-config)
      (let ((config (path-config-context (getcwd))))
        (for-each try-load-config
                  (list (system-config-context) (user-config-context) config))
        (add-config-event! config reload-config)
        (reload-config config)))
