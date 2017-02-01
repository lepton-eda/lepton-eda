;;; gEDA - GPL Electronic Design Automation
;;; gnetlist - gEDA Netlist
;;; Copyright (C) 2016-2017 gEDA Contributors
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

(define-module (gnetlist config)
  #:use-module (geda log)
  #:use-module (geda config)
  #:use-module (ice-9 match)
  #:export (gnetlist-config-ref
            print-gnetlist-config))

(define %gnetlist-config #f)

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
                   (config-filename cfg) message args))))

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
