;;; gEDA - GPL Electronic Design Automation
;;; gnetlist - gEDA Netlist
;;; Copyright (C) 2016 gEDA Contributors
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
  #:export (gnetlist-config-ref
            print-gnetlist-config
            %gnetlist-config))

(define %gnetlist-config #f)

(define (init-config)
  (let ((config (path-config-context "")))
    (set! %gnetlist-config
          `((traverse-hierarchy
             . ,(config-boolean config "gnetlist.hierarchy" "traverse-hierarchy"))
            ;; APPEND is #f, PREPEND is #t
            (reverse-refdes-order
             . ,(config-boolean config "gnetlist.hierarchy" "refdes-attribute-order"))
            (refdes-separator
             . ,(config-string config "gnetlist.hierarchy" "refdes-attribute-separator"))
            (mangle-refdes
             . ,(config-boolean config "gnetlist.hierarchy" "mangle-refdes-attribute"))
            (reverse-net-order
             . ,(config-boolean config "gnetlist.hierarchy" "net-attribute-order"))
            (mangle-net
             . ,(config-boolean config "gnetlist.hierarchy" "mangle-net-attribute"))
            (net-separator
             . ,(config-string config "gnetlist.hierarchy" "net-attribute-separator"))
            (reverse-netname-order
             . ,(config-boolean config "gnetlist.hierarchy" "netname-attribute-order"))
            (mangle-netname
             . ,(config-boolean config "gnetlist.hierarchy" "mangle-netname-attribute"))
            (netname-separator
             . ,(config-string config "gnetlist.hierarchy" "netname-attribute-separator"))
            (netname-attrib-priority
             . ,(string=? (config-string config "gnetlist" "net-naming-priority") "netname-attribute"))
            (default-net-name
              . ,(config-string config "gnetlist" "default-net-name"))
            (default-bus-name
              . ,(config-string config "gnetlist" "default-bus-name"))))))

(define (gnetlist-config-ref key)
  "Returns value of gnetlist configuration KEY."
  (assq-ref %gnetlist-config key))

(define (print-gnetlist-config)
  "Displays gnetlist configuration settings."
  (display "Gnetlist settings:\n\n")
  (for-each
   (lambda (x) (format #t "~A: ~S\n" (car x) (cdr x)))
   %gnetlist-config))

;;; Init config once
(when (not %gnetlist-config)
  (init-config))
