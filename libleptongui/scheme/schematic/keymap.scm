;; Lepton EDA Schematic Capture
;; Scheme API
;; Copyright (C) 2011 Peter Brett <peter@peter-b.co.uk>
;; Copyright (C) 2011-2013 gEDA Contributors
;; Copyright (C) 2017-2022 Lepton EDA Contributors
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
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
;;

(define-module (schematic keymap)
  #:use-module (ice-9 control)
  #:use-module (ice-9 optargs)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (system foreign)

  #:use-module (lepton ffi)

  #:use-module (schematic core gettext)
  #:use-module (schematic ffi)
  #:use-module (schematic ffi gtk)
  #:use-module (schematic hook)

  #:export (key->string
            string->key
            key->display-string
            key?
            make-key))

;;; <schematic-key> record.

(define-record-type <schematic-key>
  (make-schematic-key keyval modifiers name label)
  schematic-key?
  (keyval schematic-key-keyval set-schematic-key-keyval!)
  (modifiers schematic-key-modifiers set-schematic-key-modifiers!)
  (name schematic-key-name set-schematic-key-name!)
  (label schematic-key-label set-schematic-key-label!))

(set-record-type-printer!
 <schematic-key>
 (lambda (record port)
   (format port "#<schematic-key ~A>" (schematic-key-label record))))


;;; Creates and returns a new <schematic-key> object from a KEYVAL
;;; and MODIFIERS.  If the values are invalid, returns #f.
(define (make-key keyval modifiers)
  (let* ((*name (gtk_accelerator_name keyval modifiers))
         (name (pointer->string *name))
         (*label (gtk_accelerator_get_label keyval modifiers))
         (label (pointer->string *label)))
    (g_free *name)
    (g_free *label)
    (make-schematic-key keyval modifiers name label)))


;; -------------------- Key combinations --------------------

(define (key? key)
  "Returns #t if KEY is a <schematic-key> record.  Otherwise
returns #f."
  (schematic-key? key))

(define (key->string key)
  "Converts the bindable key object KEY to a string.  Returns a
string representation of the key combination, in a format suitable
for parsing with string->key()."
  (schematic-key-name key))


(define (key->display-string key)
  "Converts the bindable key object KEY to a displayable string.
Returns a string representation of the key combination in a format
suitable for display to the user (e.g. as accelerator text in a
menu)."
  (schematic-key-label key))


(define (string->key str)
  "Parse the string key description STR to create and return a new
lepton-schematic key object.  If STR contains syntax errors, or
does not represent a valid bindable key combination, raises the
'key-format Scheme error."
  (check-string str 1)

  (let ((keyval-bv (make-bytevector (sizeof int) 0))
        (modifiers-bv (make-bytevector (sizeof int) 0)))

    (gtk_accelerator_parse (string->pointer str)
                           (bytevector->pointer keyval-bv)
                           (bytevector->pointer modifiers-bv))
    (let ((keyval (bytevector-uint-ref keyval-bv
                                       0
                                       (native-endianness)
                                       (sizeof int)))
          (modifiers (bytevector-uint-ref modifiers-bv
                                         0
                                         (native-endianness)
                                         (sizeof int))))
      (if (and (zero? keyval)
               (zero? modifiers))
          (scm-error 'key-format
                     #f
                     "~S is not a valid key combination."
                     (list str)
                     #f)
          (make-key keyval modifiers)))))


;; -------------------- Key sequences --------------------

(define-public (keys? obj)
  (and (vector? obj)
       (> (vector-length obj) 0)
       (let/ec return
        (array-for-each
         (lambda (x) (or (key? x) (return #f)))
         obj))))

(define-public (keys->string keys)
  (string-join (map key->string (vector->list keys)) " "))

(define-public (string->keys str)
  (list->vector (map string->key
                     (filter! (lambda (x) (not (string-null? x)))
                              (string-split str #\space)))))

(define-public (keys->display-string keys)
  (string-join (map key->display-string (vector->list keys)) " "))

;; -------------------- Keymaps --------------------

;; We use a record type here in case we later want to add additional
;; information into the keymap (e.g. default actions, canonical
;; ordering, keymap names, etc):
;;
( define-record-type         ; define srfi-9-style structure ("record")
  <schematic-keymap>         ; record name
  ( %make-keymap key-table ) ; constructor taking 1 argument
  %keymap?                   ; predicate function
  (
    key-table                ; data field
    keymap-key-table         ; data field accessor function
    set-keymap-key-table!    ; data field modifier function
  )
)

(define-public (keymap? km) (%keymap? km))

(define*-public (make-keymap)
  (let ((k (%make-keymap
            '() ;; key-table: This is an empty alist.
            )))
    (add-bind-keys-hook! k)
    k))

(define-public (keymap-lookup-key keymap key)
  (assoc-ref (keymap-key-table keymap) key))

(define*-public (keymap-bind-key! keymap key #:optional (bindable #f))
  (let ((alist (keymap-key-table keymap)))
    (set-keymap-key-table! keymap
                           (if bindable
                               (assoc-set! alist key bindable)
                               (assoc-remove! alist key))))
    (run-hook bind-keys-hook keymap (make-vector 1 key) bindable))

(define-public (keymap-lookup-binding keymap bindable)
  (let ((entry (find (lambda (x) (eq? bindable (cdr x)))
                     (keymap-key-table keymap))))
    (and entry (car entry))))

(define-public (keymap-for-each proc keymap)
  (for-each
   (lambda (x) (proc (car x) (cdr x)))
   (keymap-key-table keymap)))

;; -------------------- Recursive keymaps --------------------

;; This helper function takes a string, key or key sequence, and
;; returns a key sequence.
(define (resolve-keys keys)
  (cond
   ((keys? keys) keys)
   ((key? keys) (vector keys))
   ((string? keys) (resolve-keys (string->keys keys)))
   (error "~S is not a valid key sequence" keys)))

;; This helper function recursively looks up the prefix of a key
;; sequence (i.e. all keystrokes apart from the last one) and returns
;; the corresponding keymap, or #f if there is no prefix keymap for
;; the given key sequence. If create is #t, it creates empty keymaps
;; for missing prefix keys as it goes.
(define* (keymap-for-prefix-keys! keymap keys #:optional (create #f))

  ;; Returns a new key sequence containing only the prefix key
  ;; combinations from KEYS.  This is relatively expensive, so it's
  ;; only used when constructing error messages.
  (define (prefix-keys keys)
    (let* ((N (1- (vector-length keys)))
           (p (make-vector N)))
      (vector-move-left! keys 0 N p 0)
      p))

  ;; Recursive function that does the heavy lifting.
  (define (lookup keymap keys ofs)
    (if (= (1+ ofs) (vector-length keys))
        ;; We've seen all the prefix keys, so return the keymap.
        keymap

        ;; Otherwise, check that the current key is bound to a keymap.
        ;; If so, recurse; otherwise, error.
        (let* ((key (vector-ref keys ofs))
               (binding (keymap-lookup-key keymap key)))
          (cond
           ;; If not bound and we're creating new keymaps, create a
           ;; new one, bind it, and recurse.
           ((and create (not binding))
            (let ((km (make-keymap)))
              (keymap-bind-key! keymap key km)
              (lookup km keys (1+ ofs))))

           ;; If not bound and we're not creating new keymaps, return
           ;; #f.
           ((not binding) #f)

           ;; If bound to a keymap already, recurse.
           ((keymap? binding) (lookup binding keys (1+ ofs)))

           ;; Otherwise, generate an error.
           (else (error (G_ "~S is not a prefix key sequence.")
                        (keys->display-string (prefix-keys keys))))))))

  (lookup keymap keys 0))

(define-public (lookup-keys keymap keys)
  (let* ((keyseq (resolve-keys keys))
         (km (keymap-for-prefix-keys! keymap keyseq)))
    (and km (keymap-lookup-key
             km
             (vector-ref keyseq (1- (vector-length keyseq)))))))

(define*-public (bind-keys! keymap keys #:optional (bindable #f))
  (let* ((keyseq (resolve-keys keys))
         (km (keymap-for-prefix-keys! keymap keyseq #t)))
    (keymap-bind-key! km
                      (vector-ref keyseq (1- (vector-length keyseq)))
                      bindable)))

(define-public (lookup-binding keymap bindable)

  ;; Recursive function that does the heavy lifting. This ends up
  ;; being a depth-first search, unfortunately. Return is a
  ;; continuation to pass the result to.
  (define (lookup-binding-recursive km prefix return)
    (keymap-for-each
     (lambda (key bound)
       (cond
        ;; Success! Return the full key sequence.
        ((eq? bound bindable)
         (return (list->vector (reverse (cons key prefix)))))

        ;; If a keymap, recurse.
        ((keymap? bound)
         (lookup-binding-recursive bound (cons key prefix) return))

        (else #f)))
        km))

  (let/ec return
    (lookup-binding-recursive keymap '() return)
    #f))  ;; Return #f if no binding found.

;; -------------------- Bind keys hook --------------------

;; bind-keys-hook is called whenever a keymap changes.  That means
;; that it needs to be called recursively by any keymap that binds an
;; inferior keymap to implement prefix keys.  For example, the key
;; sequence "F N" is implemented by binding "N" to an action in a
;; keymap, and then binding "F" to that keymap in the %global-keymap.
;; If "N" is rebound in the inferior keymap, then a bind-key-hook call
;; must occur for the %global-keymap.
;;
;; Although each keymap could add a closure directly to
;; bind-keys-hook, that approach would prevent keymaps from ever being
;; garbage-collected (because bind-keys-hook would forever hold a
;; reference to them).  Instead, we use a weak hash table to hold the
;; list of keymaps so that when the keymaps are garbage-collected they
;; are automatically disconnected from bind-keys-hook.

(define %bind-keys-hook-keymaps (make-weak-key-hash-table 32))

(define (add-bind-keys-hook! keymap)
  (hash-set! %bind-keys-hook-keymaps keymap #t))

(define (bind-keys-default-handler keymap keys binding)

  (define (left-extend-keys key keys)
    ;; Adds KEY onto the front of the key sequence KEYS.
    (let* ((len (vector-length keys))
           (vec (make-vector (1+ len))))
      (vector-set! vec 0 key)
      (vector-move-right! keys 0 len vec 1)
      vec))

  (define (bind-keys-recurse keymap inferior-keymap keys binding)
    ;; Check whether INFERIOR-KEYMAP is bound in KEYMAP, and if so,
    ;; run bind-keys-hook again with the key sequence left-extended
    ;; with the associated prefix key.
    (let ((prefix-key (keymap-lookup-binding keymap inferior-keymap)))
      (and prefix-key
           (run-hook bind-keys-hook keymap
                     (left-extend-keys prefix-key keys)
                     binding))))

  (hash-for-each
   (lambda (k v) (or (eq? k keymap)
                     (bind-keys-recurse k keymap keys binding)))
   %bind-keys-hook-keymaps))

;; Add the default handler to be run *last* in the bind-keys-hook
(add-hook! bind-keys-hook bind-keys-default-handler #t)
