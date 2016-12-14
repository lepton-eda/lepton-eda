;;; gEDA - GPL Electronic Design Automation
;;; gnetlist - gEDA Netlist
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
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;;; MA 02111-1301 USA.


(use-modules (srfi srfi-1)
             (gnetlist traverse)
             (gnetlist package)
             (gnetlist package-pin)
             (gnetlist pin-net)
             (gnetlist attrib compare)
             (ice-9 match)
             (geda page)
             (geda object)
             (sxml transform))

;;; Toplevel schematic is just a list of pages given on the
;;; command line.
(define toplevel-schematic (active-pages))


(define gnetlist:cwd (getcwd))
(define netlist (traverse))
;;; Change back to the directory where we started.  This is done
;;; because (traverse) can change the current working directory.
(chdir gnetlist:cwd)


(define (page->sxml page)
  (define composites
    (filter-map (lambda (p) (and (package-composite? p) p)) netlist))

  (define (source-page source)
    (let loop ((pages (active-pages)))
      (and (not (null? pages))
           (if (string=? source (basename (page-filename (car pages))))
               (page->sxml (car pages))
               (loop (cdr pages))))))

  (define (composite->sxml p)
    (and (string=? (basename (page-filename (object-page (package-object p))))
                   (basename (page-filename page)))
         (let* ((sources-value (or (assq-ref (package-attribs p) 'source)
                                   (assq-ref (package-iattribs p) 'source)))
                (sources (and sources-value
                              (append-map (lambda (s) (string-split s #\,))
                                          sources-value))))
           (if sources
               `(package ,p ,@(map source-page sources))
               `(package ,p)))))

  `(page ,page ,@(filter-map composite->sxml composites)))

(define (schematic->sxml schematic)
  `(*TOP* ,@(map page->sxml schematic)))

(define (schematic-sxml-tree->dependency-tree tree)
  (define (simplify-dependencies x)
    (delete-duplicates (apply append x)))

  (pre-post-order
   tree
   `((*TOP* . ,(lambda (x . t) `(*TOP* . ,t)))
     (page . ,(lambda (x pg . deps) `(,(basename (page-filename pg)) .
                                 ,(simplify-dependencies deps))))
     (package . ,(lambda (x pkg . deps) deps))
     (*text* . ,(lambda (x t) t))
     (*default* . ,(lambda (x . t) t)))))

(define non-unique-packages
  (sort (filter-map package-refdes netlist) refdes<?))

(define (get-packages netlist)
  "Returns a sorted list of unique packages in NETLIST."
  ;; Uniqueness of packages is guaranteed by the hashtable.
  (define ht (make-hash-table (length netlist)))
  (define (get-value key value) value)
  (for-each (lambda (s) (hashq-set! ht (string->symbol s) s))
            non-unique-packages)
  (sort (hash-map->list get-value ht) refdes<?))

;;; Only unique packages
(define packages (get-packages netlist))

(define (sort-remove-duplicates ls sort-func)
  (let ((ls (sort ls sort-func)))
    (fold-right
     (lambda (elem ret)
       (if (equal? elem (first ret))
           ret
           (cons elem ret)))
     (last-pair ls)
     ls)))

;;; Helper function for sorting connections.
(define (pair<? a b)
  (or (refdes<? (car a) (car b))
      (and (string=? (car a) (car b))
           (refdes<? (cdr a) (cdr b)))))



(define (get-all-connections netname)
  "Returns all connections in the form of ((refdes pin) ...) for
NETNAME."
  (define (found? x)
    (and x
         (string=? x netname)))

  (define (get-found-pin-connections pin)
    (if (found? (package-pin-name pin))
        (filter-map
         (lambda (net) (let ((package (pin-net-connection-package net))
                        (pinnumber (pin-net-connection-pinnumber net)))
                    (and package
                         pinnumber
                         (cons package pinnumber))))
         (package-pin-nets pin))
        '()))

  (define (get-netlist-connections netlist)
    (append-map
     (lambda (package)
       (append-map get-found-pin-connections (package-pins package)))
     netlist))

  (sort-remove-duplicates (get-netlist-connections netlist) pair<?))


(define (get-all-nets)
  (define (connected? pin)
    (let ((netname (package-pin-name pin)))
      (and (string? netname)
           (not (string-prefix? "unconnected_pin" netname))
           netname)))

  (append-map
   (lambda (package)
     (filter-map connected? (package-pins package)))
   netlist))


(define (get-all-unique-nets)
  "Returns a list of unique nets in design."
  (sort-remove-duplicates (get-all-nets) refdes<?))


;; return a list of all unique the nets in the design
(define all-unique-nets
  (get-all-unique-nets))


;; return a list of all the nets in the design
;; Might return duplicates
(define all-nets
  (get-all-nets))


(define (get-pins-nets package)
  "Returns a list of pairs (pin-name . net-name) where net-name is
the name of the net connected to the pin pin-name for specified
PACKAGE."

  (define (found? x)
    (and x
         (string=? x package)))

  (define (get-pin-netname-pair pin)
    (let ((pin-number (package-pin-number pin))
          (pin-name (package-pin-name pin)))
      (and pin-number
           pin-name
           (cons pin-number pin-name))))

  (define (get-pin-netname-list package)
     (if (found? (package-refdes package))
         (filter-map get-pin-netname-pair (package-pins package))
         '()))

  ;; Currently, netlist can contain many `packages' with the same
  ;; name, so we have to deal with this.
  (let ((result-list (append-map get-pin-netname-list netlist)))
    (sort-remove-duplicates result-list pair<?)))


(define (get-pins refdes)
  (define (found? x)
    (and x
         (string=? x refdes)))

  (sort-remove-duplicates
   (append-map
    (lambda (package)
      (if (found? (package-refdes package))
          (filter-map package-pin-number (package-pins package))
          '()))
    netlist)
   refdes<?))


;;; This procedure is buggy in the same way as gnetlist:get-nets.
;;; It should first search for netname, and then get all
;;; package-pin pairs by that netname.
(define (get-nets package pin-number)
  (define (net-connections nets)
    (filter-map
     (lambda (net)
       (let ((package (pin-net-connection-package net))
             (pinnumber (pin-net-connection-pinnumber net)))
         (and package
              pinnumber
              (cons package pinnumber))))
     nets))

  (define (lookup-through-nets nets package pin-number)
    (let ((connections (net-connections nets)))
      (and (not (null? connections))
           (member (cons package pin-number) connections)
           connections)))

  (define (found-pin-number? x)
    (and x
         (string=? x pin-number)))

  (define (lookup-through-pins pins)
    (filter-map
     (lambda (pin)
       (and (found-pin-number? (package-pin-number pin))
            (cons (package-pin-name pin)
                  (lookup-through-nets (package-pin-nets pin)
                                       package
                                       pin-number))))
     pins))

  (define (found-package? x)
    (and x
         (string=? x package)))

  (define (lookup-through-netlist netlist)
    (append-map
     (lambda (package)
       (if (found-package? (package-refdes package))
           (lookup-through-pins (package-pins package))
           '()))
     netlist))

  (let ((found (lookup-through-netlist netlist)))
    (match found
      (() '("ERROR_INVALID_PIN"))
      (((netname . rest) ...)
       (cons (car netname) (append-map identity (filter-map identity rest))))
      (_ '("ERROR_INVALID_PIN")))))


(define (package-pin-netname package pinnumber)
  (or (assoc-ref (get-pins-nets package) pinnumber)
      "ERROR_INVALID_PIN"))

;;
;; Functions for dealing with naming requirements for different
;; output netlist formats which may be more restrictive than
;; gEDA's internals.
;;

;; These will become hash tables which provide the mapping
;; from gEDA net name to netlist net name and from netlist
;; net name to gEDA net name.
(define gnetlist:net-hash-forward (make-hash-table  (length all-nets)))
(define gnetlist:net-hash-reverse (make-hash-table  (length all-nets)))

;; These will become hash tables which provide the mapping
;; from gEDA refdes to netlist refdes and from netlist
;; refdes to gEDA refdes.
(define gnetlist:refdes-hash-forward (make-hash-table  (length packages)))
(define gnetlist:refdes-hash-reverse (make-hash-table  (length packages)))

;; build the hash tables with the net name mappings and
;; while doing so, check for any shorts which are created
;; by modifying the netnames.  If a short occurs, error out
;; with a descriptive message.
;;
;; This function should be called as one of the first steps
;; in a netlister which needs to alias nets.
(define gnetlist:build-net-aliases
  (lambda (mapfn nets)
    (if (not (null? nets))
        (begin
          (let ( (net (car nets))
                 (alias (mapfn (car nets)))
                 )

            (if (hash-ref gnetlist:net-hash-reverse alias)
                (begin
                  (message "***** ERROR *****\n")
                  (message "There is a net name collision!\n")
                  (message "The net called \"")
                  (message net)
                  (message "\" will be remapped\nto \"")
                  (message alias)
                  (message "\" which is already used\n")
                  (message "by the net called \"")
                  (message (hash-ref gnetlist:net-hash-reverse alias))
                  (message "\".\n")
                  (message "This may be caused by netname attributes colliding with other netnames\n")
                  (message "due to truncation of the name, case insensitivity, or\n")
                  (message "other limitations imposed by this netlist format.\n")
                  (error)
                  )
                )
            (hash-create-handle! gnetlist:net-hash-forward net   alias)
            (hash-create-handle! gnetlist:net-hash-reverse alias net  )
            (gnetlist:build-net-aliases mapfn (cdr nets))
            )
          )
        )
    )
  )

;; build the hash tables with the refdes mappings and
;; while doing so, check for any name clashes which are created
;; by modifying the refdes's.  If a name clash occurs, error out
;; with a descriptive message.
;;
;; This function should be called as one of the first steps
;; in a netlister which needs to alias refdes's.
(define gnetlist:build-refdes-aliases
  (lambda (mapfn refdeses)
    (if (not (null? refdeses))
        (begin
          (let ( (refdes (car refdeses))
                 (alias (mapfn (car refdeses)))
                 )

            (if (hash-ref gnetlist:refdes-hash-reverse alias)
                (begin
                  (message "***** ERROR *****\n")
                  (message "There is a refdes name collision!\n")
                  (message "The refdes \"")
                  (message refdes)
                  (message "\" will be mapped\nto \"")
                  (message alias)
                  (message "\" which is already used\n")
                  (message "by \"")
                  (message (hash-ref gnetlist:refdes-hash-reverse alias))
                  (message "\".\n")
                  (message "This may be caused by refdes attributes colliding with others\n")
                  (message "due to truncation of the refdes, case insensitivity, or\n")
                  (message "other limitations imposed by this netlist format.\n")
                  (error)
                  )
                )
            (hash-create-handle! gnetlist:refdes-hash-forward refdes alias)
            (hash-create-handle! gnetlist:refdes-hash-reverse alias  refdes  )
            (gnetlist:build-refdes-aliases mapfn (cdr refdeses))
            )
          )
        )
    )
  )

;; convert a gEDA netname into an output netlist net name
(define gnetlist:alias-net
  (lambda (net)
    (hash-ref gnetlist:net-hash-forward net)
    )
  )

;; convert a gEDA refdes into an output netlist refdes
(define gnetlist:alias-refdes
  (lambda (refdes)
    (hash-ref gnetlist:refdes-hash-forward refdes)
    )
  )

;; convert an output netlist net name into a gEDA netname
(define gnetlist:unalias-net
  (lambda (net)
    (hash-ref gnetlist:net-hash-reverse net)
    )
  )

;; convert an output netlist refdes into a gEDA refdes
(define gnetlist:unalias-refdes
  (lambda (refdes)
    (hash-ref gnetlist:refdes-hash-reverse refdes)
    )
  )

