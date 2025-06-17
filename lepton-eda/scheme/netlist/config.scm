;;; Lepton EDA netlister
;;; Copyright (C) 2016-2017 gEDA Contributors
;;; Copyright (C) 2017-2022 Lepton EDA Contributors
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

(define-module (netlist config)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (lepton config)
  #:use-module (lepton log)
  #:use-module (netlist option)
  #:export (netlist-config-ref
            print-netlist-config))

(define %netlist-config #f)

;;; Init default values
(let ((cfg (default-config-context)))

  ;; This is the default name used for nets for which the user has set
  ;; no explicit name via the netname= or net= attributes.
  (set-config! cfg "netlist" "default-net-name" "unnamed_net")

  ;; This is the default name used for buses for which the user has set
  ;; no explicit name via the netname= or net= attributes.
  (set-config! cfg "netlist" "default-bus-name" "unnamed_bus")

  ;; By default, net= attributes beat netname= attributes.
  (set-config! cfg "netlist" "net-naming-priority" "net-attribute")

  ;; By default, hierarchy processing is enabled.
  (set-config! cfg "netlist.hierarchy" "traverse-hierarchy" #t)

  ;; By default, sub-schematic attributes 'refdes' are built
  ;; accounting for the parent schematic's ones.
  (set-config! cfg "netlist.hierarchy" "mangle-refdes-attribute" #t)

  ;; By default, sub-schematic attributes 'refdese' are appended to the parent
  ;; schematic's ones to build hierarchical attributes 'refdes'.
  (set-config! cfg "netlist.hierarchy" "refdes-attribute-order" #f)

  ;; This is the default separator which is used to built hierarchical
  ;; refdeses of any component in sub-schematics.
  (set-config! cfg "netlist.hierarchy" "refdes-attribute-separator" "/")

  ;; By default, sub-schematic attributes 'netname' are built
  ;; accounting for the parent schematic's ones.
  (set-config! cfg "netlist.hierarchy" "mangle-netname-attribute" #t)

  ;; By default, sub-schematic attributes 'netname' are appended to the parent
  ;; schematic's ones to build hierarchical netnames.
  (set-config! cfg "netlist.hierarchy" "netname-attribute-order" #f)

  ;; This is the default separator which is used to built hierarchical
  ;; attributes 'netname' for nets in sub-schematics.
  (set-config! cfg "netlist.hierarchy" "netname-attribute-separator" "/")

  ;; By default, sub-schematic attributes 'net' are built
  ;; accounting for the parent schematic's ones.
  (set-config! cfg "netlist.hierarchy" "mangle-net-attribute" #t)

  ;; By default, sub-schematic attributes 'net' are appended to the parent
  ;; schematic's ones to build hierarchical netnames.
  (set-config! cfg "netlist.hierarchy" "net-attribute-order" #f)

  ;; This is the default separator which is used to built hierarchical
  ;; attributes 'net' for nets in sub-schematics.
  (set-config! cfg "netlist.hierarchy" "net-attribute-separator" "/"))

(define %netlist-config-table
  `((traverse-hierarchy    ,config-boolean "netlist.hierarchy" "traverse-hierarchy")
    (reverse-refdes-order  ,config-boolean "netlist.hierarchy" "refdes-attribute-order")
    (refdes-separator      ,config-string  "netlist.hierarchy" "refdes-attribute-separator")
    (mangle-refdes         ,config-boolean "netlist.hierarchy" "mangle-refdes-attribute")
    (reverse-net-order     ,config-boolean "netlist.hierarchy" "net-attribute-order")
    (mangle-net            ,config-boolean "netlist.hierarchy" "mangle-net-attribute")
    (net-separator         ,config-string  "netlist.hierarchy" "net-attribute-separator")
    (reverse-netname-order ,config-boolean "netlist.hierarchy" "netname-attribute-order")
    (mangle-netname        ,config-boolean "netlist.hierarchy" "mangle-netname-attribute")
    (netname-separator     ,config-string  "netlist.hierarchy" "netname-attribute-separator")
    (netname-attribute-priority
                           ,(lambda args (string=? "netname-attribute" (apply config-string args)))
                                           "netlist"           "net-naming-priority")
    (default-net-name      ,config-string  "netlist"           "default-net-name")
    (default-bus-name      ,config-string  "netlist"           "default-bus-name")))

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

  (set! %netlist-config (map process %netlist-config-table)))

(define (netlist-config-ref key)
  "Returns value of lepton-netlist configuration KEY."
  (car (assq-ref %netlist-config key)))

(define (print-netlist-config)
  "Displays lepton-netlist configuration settings."
  (define (source-string cfg)
    (if (equal? cfg (default-config-context))
        "(default)"
        (string-append "[" (config-filename cfg) "]")))
  (display "lepton-netlist settings:\n\n")
  (for-each
   (lambda (x)
     (match x
            ((sym value cfg)
             (format #t "~A: ~S ~A\n" sym value (source-string cfg)))))
   %netlist-config))

;;; Init config once
(when (not %netlist-config)
      (let ((config (path-config-context (getcwd))))
        (for-each try-load-config
                  (list (system-config-context) (user-config-context) config))
        (add-config-event! config reload-config)
        (reload-config config)))
