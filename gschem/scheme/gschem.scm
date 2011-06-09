;;; gEDA - GPL Electronic Design Automation
;;; gschem - gEDA Schematic Capture
;;; Copyright (C) 1998-2010 Ales Hvezda
;;; Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
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


; guile 1.4/1.6 compatibility:  Define an eval-in-currentmodule procedure
; If this version of guile has an R5RS-compatible eval (that requires a
; second argument specfying the environment), and a current-module function
; (like 1.6) use them to define eval-cm. else define eval-cm to eval (for 1.4)
(define eval-cm
  (if (false-if-exception (eval 'display (current-module)))
      (lambda (exp) (eval exp (current-module)))
      eval))

(define last-command-sequence #f)
(define current-command-sequence '())

; Doers
(define (press-key key)
  (eval-pressed-key current-keymap key))

(define (eval-pressed-key keymap key)
  (and keymap
       (let ((lookup (assoc key keymap)))
         (cond ((pair? lookup)
                (if (not (equal? 'repeat-last-command (cdr lookup)))
                    (set! current-command-sequence 
                          (cons key current-command-sequence)))
                (perform-action (cdr lookup)))
               (else
                (set! current-keymap global-keymap)
                ;(display "No keymap found")
                ;(newline)
                #f
                )))))

(define (perform-action action)
    (let ((local-action (eval-cm action)))
      (cond ((list? local-action)
             (set! current-keymap local-action))
            ((equal? 'repeat-last-command action)
             (repeat-last-command))
            (else
             (set! last-command-sequence current-command-sequence)
             (set! current-command-sequence '())
             (local-action)
             (set! current-keymap global-keymap)))))

(define (repeat-last-command)
  ;; need to `reverse' because the sequence was "push"ed initially
  ;(display last-command-sequence)
  ;(newline)
  (and last-command-sequence
       (not (null? last-command-sequence))
       (for-each press-key (reverse last-command-sequence))))

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


;; Search the keymap for a particular scheme function and return the keys
;; which execute this hotkey
(define foundkey "")
(define temp "")

(define find-key-lowlevel 
  (let ((keys '()))
    (lambda (keymap function)
      (for-each 
       (lambda (mapped-key) ; Receives a pair
         (if (list? (eval-cm (cdr mapped-key)))
             (begin
               (set! temp (car mapped-key))
               (find-key-lowlevel (eval-cm (cdr mapped-key)) function)
               (set! temp "")
               )
             (if (eq? (cdr mapped-key) function)	
                 (set! foundkey (string-append temp (car mapped-key)))
                 
                 )
             )
         ) 
       keymap))))

(define find-key 
  (lambda(function)
    (set! temp "")
    (set! foundkey "")
;;    (display function) (newline)
    (find-key-lowlevel global-keymap function)
    (if (eq? (string-length foundkey) 0) 
        #f
        foundkey
        )
    ))

;; Printing out current key bindings for gEDA (gschem)

(define (dump-current-keymap)
  (dump-keymap global-keymap))

(use-modules (srfi srfi-13))
(define (dump-keymap keymap)
  (let loop ((keymap keymap)
             (keys   '()))
    (if (null? keymap)
        '()
        (let* ((entry  (car keymap))
               (key    (car entry))
               (action (eval-cm (cdr entry))))
          (cond ((list? action)
                 (append (loop action (cons key keys))
                         (loop (cdr keymap) keys)))
                (else
                 (cons (cons (cdr entry) 
                             (string-join (reverse (cons key keys)) " "))
                       (loop (cdr keymap) keys))))))))

;; Predicate to test if entry is keymap or action
(define gschem:keymap? list?)

;; Map over keymap tree, applying f to every node before descending
(define (gschem:for-each-keymap f kmap)
  (if (gschem:keymap? kmap)
    (for-each
      (lambda (kmap-entry)
        (apply f (list kmap-entry))
        (gschem:for-each-keymap f (eval-cm (cdr kmap-entry))))
      kmap)))

;; Sorting multiple key modifiers for unambiguos keymaps
(define gschem:normalize-accel!
  (lambda (kmap-entry)
    (let* ((accel-list (reverse (string-split (car kmap-entry) #\space)))
           (modifiers (cdr accel-list))
           (key (car accel-list))
           (sorted (sort modifiers string<?))
           (appended (append sorted (list key)))
           (joined (string-join appended " ")))
      (set-car! kmap-entry joined))))

;; Once at startup normalize the global keymap
(gschem:for-each-keymap gschem:normalize-accel! global-keymap)
