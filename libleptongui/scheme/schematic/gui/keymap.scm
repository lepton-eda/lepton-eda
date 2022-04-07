;;; Lepton EDA Schematic Capture
;;; Copyright (C) 1998-2010 Ales Hvezda
;;; Copyright (C) 1998-2013 gEDA Contributors
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

(define-module (schematic gui keymap)
  #:use-module (srfi srfi-1)
  #:use-module (system foreign)

  #:use-module (lepton ffi)
  #:use-module (lepton log)
  #:use-module (lepton toplevel)

  #:use-module (schematic action)
  #:use-module (schematic ffi)
  #:use-module (schematic keymap)

  #:export (%global-keymap
            current-keymap
            global-set-key
            reset-keys
            find-key
            %gschem-hotkey-store/dump-global-keymap
            *process-key-event))

;;; Key event processing.

(define (process-key-event *page_view *event *window)
  (define (protected-eval-key-press key)
    (catch #t
      (lambda () (press-key key))
      (lambda (k . a)
        (log! 'message
              "Could not eval key ~S: ~S ~S"
              (key->display-string key)
              k
              a))))

  ;; Validate event data and create a Scheme key record.
  (define (event->key *event)
    (and (not (null-pointer? *event))
         (make-key (schematic_keys_get_event_keyval *event)
                   (schematic_keys_get_event_modifiers *event))))

  ;; %lepton-window is defined in C code and is not visible at the
  ;; moment the module is compiled.  Use last resort to refer to
  ;; it.
  (with-fluids (((@@ (guile-user) %lepton-window) *window))
    ;; We have to dynwind LeptonToplevel as well since there are
    ;; functions that depend on toplevel only and should know what
    ;; its current value is.
    (%with-toplevel
     (pointer->geda-toplevel (gschem_toplevel_get_toplevel *window))
     (lambda ()
       (let ((*event (x_event_key *page_view *event *window)))
         (let ((key (event->key *event)))
           (if key
               (begin
                 ;; Update the status bar with the current key
                 ;; sequence.
                 (schematic_window_update_keyaccel_string *window
                                                          (string->pointer (key->display-string key)))
                 ;; Actually evaluate the key press.
                 (let* ((retval (protected-eval-key-press key))
                        ;; If the keystroke was not part of a key
                        ;; sequence prefix, start a timer to clear
                        ;; the status bar display.
                        (start-cleanup-timer?
                         (if (eq? retval 'prefix) FALSE TRUE)))

                   (schematic_window_update_keyaccel_timer *window
                                                           start-cleanup-timer?)
                   ;; Propagate the event further if press-key()
                   ;; returned #f.  Thus, you can move from page
                   ;; view to toolbar by Tab if the key is not
                   ;; assigned in the global keymap.
                   (if retval TRUE FALSE)))
               ;; Invalid key.
               FALSE)))))))

(define *process-key-event
  (procedure->pointer int process-key-event '(* * *)))


;; -------------------------------------------------------------------
;;;; Global keymaps and key dispatch logic

(define current-keys '())

;;; Default global keymap.
(define %global-keymap (make-keymap))
;;; User defined keymap.
(define current-keymap %global-keymap)

;; Set a global keybinding
(define (global-set-key key binding)
  (bind-keys! %global-keymap key binding))

;; Called from C code to evaluate keys.
(define (press-key key)
  (eval-pressed-key current-keymap key))

;; Function for resetting current key sequence
(define (reset-keys) (set! current-keys '()) #f)

;; Does the work of evaluating a key.  Adds the key to the current key
;; sequence, then looks up the key sequence in the current keymap.  If
;; the key sequence resolves to an action, calls the action.  If the
;; key sequence can be resolved to an action, returns #t; if it
;; resolves to a keymap (i.e. it's a prefix key), returns the "prefix"
;; symbol; otherwise, returns #f.  If the key is #f, clears the
;; current key sequence.
(define (eval-pressed-key keymap key)
  (if key
      (begin
        ;; Add key to current key sequence
        (set! current-keys (cons key current-keys))
        (let* ((keys (list->vector (reverse current-keys)))
               (bound (lookup-keys keymap keys)))
          (cond
           ;; Keys are a prefix -- do nothing successfully
           ((keymap? bound) 'prefix)
           ;; Keys are bound to something -- reset current key
           ;; sequence, then try to run the action
           (bound (begin
                    (reset-keys)
                    (eval-action-at-point! bound)))
           ;; No binding
           (else (reset-keys)))))

      (reset-keys)))

;; Search the global keymap for a particular symbol and return the
;; keys which execute this hotkey, as a string suitable for display to
;; the user. This is used by the lepton-schematic menu system.
(define (find-key action)
  (let ((keys (lookup-binding %global-keymap action)))
    (and keys (keys->display-string keys))))

;; Printing out current key bindings for lepton-schematic.
(define (%gschem-hotkey-store/dump-global-keymap)
  (dump-keymap %global-keymap))

(define (dump-keymap keymap)

  ;; Use this to change "Page_Up" to "Page Up" (etc.)
  (define (munge-keystring str)
    (string-map (lambda (c) (case c ((#\_) #\ ) (else c))) str))

  (define lst '())

  (define (binding->entry prefix key binding)
    (let ((keys (list->vector (reverse (cons key prefix))))
          (action (false-if-exception (eval binding (current-module)))))

      ;; If the binding points to an action, then use its label.
      ;; Otherwise, just use the string value of the binding.
      (let ((keystr (munge-keystring (keys->display-string keys)))
            (cmdstr (or (and (action? action)
                             (action-property action 'label))
                        (symbol->string binding)))
            (iconstr (and (action? action)
                          (action-property action 'icon))))
      (set! lst (cons (list cmdstr keystr iconstr) lst)))))

  (define (build-dump! km prefix)
    (keymap-for-each
     (lambda (key binding)
       (cond

        ((or (symbol? binding) (action? binding))
         (binding->entry prefix key binding))

        ((keymap? binding)
         (build-dump! binding (cons key prefix)))
        (else (error "Invalid action ~S bound to ~S"
                     binding (list->vector (reverse (cons key prefix)))))))
     km))

  (build-dump! keymap '())
  lst)
