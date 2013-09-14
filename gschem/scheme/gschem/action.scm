;; gEDA - GPL Electronic Design Automation
;; gschem - gEDA Schematic Capture - Scheme API
;; Copyright (C) 2013 Peter Brett <peter@peter-b.co.uk>
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

(define-module (gschem action)
  #:use-module (gschem core gettext)
  #:use-module (gschem window)
  #:use-module (gschem hook)
  #:use-module (ice-9 optargs)
  #:export-syntax (define-action))

(or (defined? 'define-syntax)
    (use-modules (ice-9 syncase)))

(define last-action (make-fluid))
(define current-action-position (make-fluid))

;; Define an eval-in-currentmodule procedure
(define (eval-cm expr) (eval expr (current-module)))

;; Evaluates a gschem action.  A gschem action is expected to be a
;; symbol naming a thunk variable in the current module.
;;
;; The special-case symbol repeat-last-command causes the last action
;; executed to be repeated.
(define-public (eval-action! action)
  (define (invalid-action-error)
    (error (_ "~S is not a valid gschem action.") action))

  (define (eval-action!/recursive a)

    (cond
     ;; Handle repeat-last-command
     ((equal? 'repeat-last-command a)
      (eval-action!/recursive (fluid-ref last-action)))

     ;; Sometimes you get a first-class action
     ((action? a)
      (eval-action!/recursive (false-if-exception (action-thunk a))))

     ;; Sometimes actions are specified just by a symbol naming them
     ((symbol? a)
      (eval-action!/recursive (false-if-exception (eval-cm a))))

     ;; Eventually you just end up with a thunk.
     ((thunk? a)
      (begin 
        (fluid-set! last-action action)
        (a) ;; Actually execute the action
        #t))

     ;; Otherwise, fail
     (else (invalid-action-error))))
  
  (eval-action!/recursive action))

;; Evaluate an action at a particular point on the schematic plane.
;; If the point is omitted, the action is evaluated at the current
;; mouse pointer position.
(define*-public (eval-action-at-point!
                 action
                 #:optional (point (pointer-position)))

  (with-fluids ((current-action-position point))
               (eval-action! action)))

;; Return the current action pointer position.  This should be the
;; location at which the action was invoked (set via
;; eval-action-at-point!).
(define-public (action-position)
  (fluid-ref current-action-position))

;; -------------------------------------------------------------------
;; First-class actions

;; Make a symbol that's guaranteed to be unique in this session.
(define %cookie (make-symbol "gschem-action-cookie"))

(define-public (action? proc)
  (false-if-exception
   (eq? %cookie (procedure-property proc 'gschem-cookie))))

(define-syntax define-action
  (syntax-rules ()
    ((_ (name . args) . forms)
     (define name (make-action (lambda () . forms) . args)))))

(define-public (make-action thunk . props)
  ;; The action is a magical procedure that does nothing but call
  ;; eval-action! *on itself*.  This allows you to invoke an action
  ;; just by calling it like a normal function.
  (letrec ((action (lambda () (eval-action! action))))

    ;; The action data is stored in procedure properties -- most
    ;; importantly, the actual thunk that the action wraps
    (let ((sp! (lambda (k v) (set-procedure-property! action k v))))
      (sp! 'gschem-cookie %cookie)
      (sp! 'gschem-thunk thunk)
      (sp! 'gschem-properties '()))

    ;; Deal with any properties.  props should contain arguments in
    ;; pairs, where the first element of each pair is a keyword naming
    ;; a procedure property (e.g. #:icon) and the second element in
    ;; the corresponding value (e.g. "insert-text").
    (let loop ((lst props))
      (and (< 1 (length lst))
           (set-action-property! action
                                 (keyword->symbol (list-ref lst 0))
                                 (list-ref lst 1))
           (loop (list-tail lst 2))))
          
          

    action))

(define (action-thunk action)
  (procedure-property action 'gschem-thunk))

(define (action-properties action)
  (procedure-property action 'gschem-properties))
(define (set-action-properties! action alist)
  (set-procedure-property! action 'gschem-properties alist))

(define-public (set-action-property! action key value)
  (set-action-properties! action
   (assq-set! (action-properties action) key value))
  (run-hook action-property-hook action key value))
(define-public (action-property action key)
  (assq-ref (action-properties action) key))
