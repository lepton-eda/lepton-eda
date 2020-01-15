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
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-69)

  #:use-module (geda object)

  #:use-module (netlist core gettext)

  #:use-module (netlist attrib compare)
  #:use-module (netlist net)
  #:use-module (netlist package-pin)
  #:use-module (netlist schematic toplevel)
  #:use-module (netlist schematic)
  #:use-module (netlist schematic-component)
  #:use-module (netlist schematic-connection)
  #:use-module (netlist verbose)

  #:export (add-rename
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
  (define (make-special-netname connection hname)
    (let* ((object (car (schematic-connection-objects connection)))
           (coord (line-start object)))
      (create-net-name
       (format #f "unnamed_net_at_~Ax~A" (car coord) (cdr coord))
       hname
       #f)))

  (define (unnamed? name)
    (eq? 'unnamed (car name)))

  (define (name<? a b)
    (refdes<? (schematic-connection-override-name a)
              (schematic-connection-override-name b)))

  (define (net? name)
    (eq? 'net (car name)))

  (define (create-net-name* connection hname)
    (if (unnamed? hname)
        (make-special-netname connection (cdr hname))
        (create-net-name (cadr hname) (cddr hname) (net? hname))))

  (let ((connections (sort (schematic-connections (toplevel-schematic))
                           name<?)))
    (append-map
     (lambda (connection)
       (let ((common-name (schematic-connection-override-name connection))
             (other-names (map (cut create-net-name* connection <>)
                               (cdr (schematic-connection-name connection)))))

         (map (lambda (n) (cons n common-name)) other-names)))
     connections)))
