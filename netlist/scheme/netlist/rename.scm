;;; Lepton EDA netlister
;;; Copyright (C) 2017-2018 Lepton EDA Contributors
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

(define-module (netlist rename)
  #:use-module (srfi srfi-69)
  #:use-module (netlist core gettext)
  #:use-module (netlist verbose)
  #:use-module (netlist schematic-component)
  #:use-module (netlist package-pin)

  #:export (search-rename
            add-rename
            add-net-rename
            rename-all
            get-rename-list))

(define (rename-lowlevel netlist from to)

  (define (rename-in-pin pin)
    (if (string=? from (package-pin-name pin))
        (set-package-pin-name! pin to)))

  (define (rename-in-package package)
    (for-each rename-in-pin (schematic-component-pins package)))

  (for-each rename-in-package netlist)
  netlist)

;;; Hash tables for renamings by netname= and net= attributes.
(define %netname-renames (make-hash-table))
(define %net-renames (make-hash-table))
;;; Hash tables to store results for post-use.
(define %%netname-renames)
(define %%net-renames)

(define (renamed? rename-table from)
  (hash-table-ref/default rename-table from #f))

(define (set-rename! rename-table from to)
  (hash-table-set! rename-table from to))

(define (update-rename rename-table from to)
  (let ((from-rename (renamed? rename-table from))
        (to-rename   (renamed? rename-table to)))
    (cond
     ;; Forward rename already exists
     ((and from-rename (equal? to from-rename)) #t)
     ;; Reverse rename already exists
     ((and to-rename (equal? from to-rename)) #t)
     ;; we found a -> b, while adding c -> a.
     ;; hence we would have c -> a -> b, so add c -> b.
     ;; avoid renaming if b is same as c!
     (to-rename (set-rename! rename-table from to-rename))
     ;; we found a -> b, while adding a -> c.
     ;; hence b <==> c, so add c -> b.
     ;; avoid renaming if b is same as c!
     (from-rename (set-rename! rename-table to from-rename))
     ;; No pre-existing renames
     (else (set-rename! rename-table from to)))))

(define (%add-rename rename-table from to)
  (and from to (not (string=? from to)) (update-rename rename-table from to)))

(define (add-rename from to)
  (%add-rename %netname-renames from to))

(define (add-net-rename from to)
  (%add-rename %net-renames from to))

(define (%search-rename rename-table from to quiet)
  "Searches rename source FROM and rename destination TO in the
internal netlist rename list. Returns #t if either FROM or TO is
found. In the latter case, if QUIET is not #f warns the user that
the net in question has been renamed several times."
  ;; FIXME[2017-02-20] This warning message is very unhelpful
  (define (warn)
    (when (not quiet)
          (format (current-error-port)
                  (_ "WARNING: Trying to rename something twice:
\t~A and ~A
are both a src and dest name
This warning is okay if you have multiple levels of hierarchy!
")
                  to
                  to)))

  (cond
   ((renamed? rename-table from) #t)
   ((renamed? rename-table to) (begin (warn) #t))
   (else #f)))

(define (search-rename from to quiet)
  (%search-rename %netname-renames from to quiet))


(define (rename-all netlist)
  ;; Do actual renaming.
  (hash-table-walk %netname-renames
                   (lambda (from to)
                     (rename-lowlevel netlist from to)))
  (hash-table-walk %net-renames
                   (lambda (from to)
                     (rename-lowlevel netlist from to)))

  ;; Backup hash tables for usage in get-rename-list().
  (set! %%netname-renames %netname-renames)
  (set! %%net-renames %net-renames)
  ;; Reinitialise hash tables to prepare for next time work.
  (set! %netname-renames (make-hash-table))
  (set! %net-renames (make-hash-table))
  netlist)

;; Return the alist of renames.  Sort it so that it's in a canonical
;; order no matter what the internal implementation of the hash table
;; is.
(define (get-rename-list)
  (define (pair<? a b)
    (or (string<? (cdr a) (cdr b))
        (and (string= (cdr a) (cdr b))
             (string<? (car a) (car b)))))

  (append (sort! (hash-table->alist %%netname-renames) pair<?)
          (sort! (hash-table->alist %%net-renames) pair<?)))
