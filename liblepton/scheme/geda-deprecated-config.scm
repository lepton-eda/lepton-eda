;; gEDA - GPL Electronic Design Automation
;; gschem - gEDA Schematic Capture - Scheme API
;; Copyright (C) 2012 Peter Brett <peter@peter-b.co.uk>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
;;

;; This file contains deprecated configuration functions for RC files.
;; The use gEDA RC files for configuration is being replaced by the
;; use of key-value files that are parsed rather than executed. This
;; will allow configuration to be safely written back to disk, and
;; will have security benefits.  The functions defined in this file
;; are intended for use in legacy RC files during the transition
;; process.

(or (defined? 'define-syntax)
    (use-modules (ice-9 syncase)))

;; ===================================================================
;; Utility functions and macros
;; ===================================================================

;; Returns an RC function closure to replace the legacy configuration
;; function OLD-ID.  The returned closure takes an arbitrary number of
;; arguments, and does nothing other than print a deprecation message
;; the first time it is called.
(define (rc-dead-config old-id)
  ;; FIXME more helpful error message with link to documentation.
  (define (deprecation-warning)
    (format (current-error-port)
"WARNING: The RC file function '~A' is deprecated and does nothing.

" old-id))
  (let ((warned? #f))
    (lambda args
      (or warned? (begin (deprecation-warning) (set! warned? #t))))))

;; Convenience macro for using rc-dead-config.
;;
;;   define-rc-dead-config OLD-ID
;;
;; Creates a dead rc configuration function called OLD-ID.
(define-syntax define-rc-dead-config
  (syntax-rules ()
    ((_ old-id)
     (define old-id (rc-dead-config (quote old-id))))))

;; Returns an RC function closure to replace the legacy configuration
;; function OLD-ID. The returned closure takes an arbitrary number of
;; arguments, and sets the configuration parameter determined by GROUP
;; and KEY to the result of passing its arguments to
;; VALUE-TRANSFORMER.  The first time the closure is called, it prints
;; a deprecation message.
(define (rc-deprecated-config old-id group key value-transformer)
  ;; FIXME more helpful error message with link to documentation.
  (define (deprecation-warning)
    (format (current-error-port)
"WARNING: The RC file function '~A' is deprecated.

RC configuration functions will be removed in an upcoming gEDA
release.  Please use configuration files instead.

" old-id))
  (let ((warned? #f))
    (lambda args
      (or warned?
          (begin (deprecation-warning) (set! warned? #t)))
      ((@ (geda config) set-config!)
       (rc-config) group key (apply value-transformer args)))))

;; Convenience macro for using rc-deprecated-config.
;;
;;   define-rc-deprecated-config OLD-ID GROUP KEY VALUE-TRANSFORMER
;;
;; Creates a deprecated rc configuration function called OLD-ID that
;; uses VALUE-TRANSFORMER to set the configuration parameter by GROUP
;; and KEY.
(define-syntax define-rc-deprecated-config
  (syntax-rules ()
    ((_ old-id group key value-transformer)
     (define old-id (rc-deprecated-config (quote old-id) group key
                                           value-transformer)))))

;; Identity value transformer for define-rc-deprecated-config
(define (rc-deprecated-string-transformer str) str)

;; Transformer for "enabled"/"disabled" to boolean
(define (rc-deprecated-string-boolean-transformer str)
  (string=? "enabled" str))

;; ===================================================================
;; Deprecated libgeda configuration functions
;; ===================================================================

(define-rc-dead-config postscript-prolog)
(define-rc-dead-config world-size)

;; ===================================================================
;; Deprecated gschem configuration functions
;; ===================================================================

(define-rc-dead-config output-capstyle)
(define-rc-dead-config output-color)
(define-rc-dead-config output-orientation)
(define-rc-dead-config output-type)
(define-rc-dead-config paper-size)
(define-rc-dead-config paper-sizes)
(define-rc-dead-config print-command)
(define-rc-dead-config setpagedevice-orientation)
(define-rc-dead-config setpagedevice-pagesize)

(define-rc-deprecated-config
 print-paper "gschem.printing" "paper"
 rc-deprecated-string-transformer)

(define-rc-deprecated-config
 print-orientation "gschem.printing" "layout"
 rc-deprecated-string-transformer)

(define-rc-deprecated-config
 print-color "gschem.printing" "monochrome"
 (lambda (x) (not (rc-deprecated-string-boolean-transformer x))))

(define-rc-dead-config net-style)
(define-rc-dead-config bus-style)
(define-rc-dead-config pin-style)
(define-rc-dead-config line-style)
(define-rc-dead-config net-endpoint-mode)
(define-rc-dead-config net-midpoint-mode)
(define-rc-dead-config object-clipping)
(define-rc-dead-config text-origin-marker)
(define-rc-dead-config text-display-zoomfactor)
(define-rc-dead-config text-feedback)

(define-rc-deprecated-config
 untitled-name "gschem" "default-filename"
 rc-deprecated-string-transformer)

(define-rc-dead-config scrollbar-update)

(define-rc-deprecated-config
 sort-component-library "gschem.library" "sort"
 rc-deprecated-string-boolean-transformer)

(define-rc-deprecated-config
 component-dialog-attributes "gschem.library" "component-attributes"
 (lambda (x) x))

;; ===================================================================
;; Deprecated gnetlist configuration functions
;; ===================================================================
(define-rc-dead-config gnetlist-version)

(define-rc-deprecated-config
  unnamed-netname "gnetlist" "default-net-name"
  rc-deprecated-string-transformer)
(define-rc-deprecated-config
  unnamed-busname "gnetlist" "default-bus-name"
  rc-deprecated-string-transformer)
(define-rc-deprecated-config
  net-naming-priority "gnetlist" "net-naming-priority"
  (lambda (x) (if (string=? x "netname") "netname-attribute" "net-attribute")))
(define-rc-deprecated-config
  hierarchy-traversal "gnetlist.hierarchy" "traverse-hierarchy"
  rc-deprecated-string-boolean-transformer)
(define-rc-deprecated-config
  hierarchy-uref-mangle "gnetlist.hierarchy" "mangle-refdes-attribute"
  rc-deprecated-string-boolean-transformer)
(define-rc-deprecated-config
  hierarchy-uref-order "gnetlist.hierarchy" "refdes-attribute-order"
  (lambda (x) (string=? "prepend" x)))
(define-rc-deprecated-config
  hierarchy-uref-separator "gnetlist.hierarchy" "refdes-attribute-separator"
  rc-deprecated-string-transformer)
(define-rc-deprecated-config
  hierarchy-netname-mangle "gnetlist.hierarchy" "mangle-netname-attribute"
  rc-deprecated-string-boolean-transformer)
(define-rc-deprecated-config
  hierarchy-netname-order "gnetlist.hierarchy" "netname-attribute-order"
  (lambda (x) (string=? "prepend" x)))
(define-rc-deprecated-config
  hierarchy-netname-separator "gnetlist.hierarchy" "netname-attribute-separator"
  rc-deprecated-string-transformer)
(define-rc-deprecated-config
  hierarchy-netattrib-mangle "gnetlist.hierarchy" "mangle-net-attribute"
  rc-deprecated-string-boolean-transformer)
(define-rc-deprecated-config
  hierarchy-netattrib-order "gnetlist.hierarchy" "net-attribute-order"
  (lambda (x) (string=? "prepend" x)))
(define-rc-deprecated-config
  hierarchy-netattrib-separator "gnetlist.hierarchy" "net-attribute-separator"
  rc-deprecated-string-transformer)
