;;; gEDA - GNU Electronic Design Automation
;;; gschem - GNU Schematic Capture
;;; Copyright (C) 1998 Ales V. Hvezda
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


(define last-command-sequence #f)
(define current-command-sequence '())


; no action hotkey
(define no-action
  (lambda ()
        ()
  )
)

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
                )))))

(define (perform-action action)
    (let ((local-action (eval action)))
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


