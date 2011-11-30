;;; gEDA - GPL Electronic Design Automation
;;; gschem - gEDA Schematic Capture
;;; Copyright (C) 1998-2010 Ales Hvezda
;;; Copyright (C) 1998-2011 gEDA Contributors (see ChangeLog for details)
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
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

(use-modules (gschem keymap))

;; Define an eval-in-currentmodule procedure
(define (eval-cm expr) (eval expr (current-module)))

(define last-action #f)
(define current-keys '())

(define %global-keymap (make-keymap))
(define current-keymap %global-keymap)

;; Set a global keybinding
(define (global-set-key key binding)
  (bind-keys! %global-keymap key binding))

;; Called from C code to evaluate keys.
(define (press-key key)
  (eval-pressed-key current-keymap key))

;; Does the work of evaluating a key.  Adds the key to the current key
;; sequence, then looks up the key sequence in the current keymap.  If
;; the key sequence resolves to an action, calls the action.  If the
;; key sequence can be resolved (either to a keymap or an action),
;; returns #t; otherwise, returns #f.  If the key is #f, clears the
;; current key sequence.
(define (eval-pressed-key keymap key)
  ;; Function for resetting current key sequence
  (define (reset-keys) (set! current-keys '()) #f)

  (if key
      (begin
        ;; Add key to current key sequence
        (set! current-keys (cons key current-keys))
        (let* ((keys (list->vector (reverse current-keys)))
               (bound (lookup-keys keymap keys)))
          (cond
           ;; Keys are a prefix -- do nothing successfully
           ((keymap? bound) #t)
           ;; Keys are bound to something -- reset current key
           ;; sequence, then try to run the action
           (bound (begin
                    (reset-keys)
                    (eval-keymap-action bound)))
           ;; No binding
           (else (reset-keys)))))

      (reset-keys)))

;; Evaluates a keymap action.  A keymap action is expected to be a
;; symbol naming a thunk variable in the current module.
;;
;; The special-case symbol repeat-last-command causes the last action
;; executed via keypress to be repeated.
(define (eval-keymap-action action)
  (define (invalid-action-error)
    (error "~S is not a valid action for keybinding." action))

  (cond
   ;; Handle repeat-last-command
   ((equal? 'repeat-last-command action)
    (eval-keymap-action last-action))

   ;; Normal actions
   ((symbol? action)
    (let ((proc (false-if-exception (eval-cm action))))
      (if (thunk? proc)
          (begin
            (set! last-action action)
            (proc)
            #t)
          (invalid-action-error))))

   ;; Otherwise, fail
   (else (invalid-action-error))))

(define (eval-stroke stroke)
  (let ((action (assoc stroke strokes)))
    (cond ((not action)
;           (display "No such stroke\n")
;          (display stroke)
           #f)
          (else
;           (display "Scheme found action ")
;           (display action)
;           (display "\n")
           ((eval-cm (cdr action)))
           #t))))

;; Search the global keymap for a particular symbol and return the
;; keys which execute this hotkey, as a string suitable for display to
;; the user. This is used by the gschem menu system.
(define (find-key action)
  (let ((keys (lookup-binding %global-keymap action)))
    (and keys (keys->display-string keys))))

;; Printing out current key bindings for gEDA (gschem)
(define (dump-global-keymap)
  (dump-keymap %global-keymap))

(define (dump-keymap keymap)

  (define lst '())

  (define (binding->entry prefix key binding)
    (let ((keys (list->vector (reverse (cons key prefix)))))
      (set! lst (cons (cons (symbol->string binding)
                            (keys->display-string keys))
                      lst))))

  (define (build-dump! km prefix)
    (keymap-for-each
     (lambda (key binding)
       (cond
        ((symbol? binding)
         (binding->entry prefix key binding))
        ((keymap? binding)
         (build-dump! binding (cons key prefix)))
        (else (error "Invalid action ~S bound to ~S"
                     binding (list->vector (reverse (cons key prefix)))))))
     km))

  (build-dump! keymap '())
  lst)
