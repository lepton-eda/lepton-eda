;;; Lepton EDA netlister
;;; Copyright (C) 2016-2017 gEDA Contributors
;;; Copyright (C) 2017 Lepton EDA Contributors
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

(define-module (gnetlist schematic)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (gnetlist attrib compare)
  #:use-module (gnetlist sort)
  #:use-module (gnetlist traverse)
  #:use-module (gnetlist package)
  #:use-module (gnetlist package-pin)
  #:use-module (geda page)
  #:use-module (geda attrib)
  #:use-module (geda object)
  #:export (make-schematic schematic?
            schematic-id set-schematic-id!
            schematic-toplevel-pages set-schematic-toplevel-pages!
            schematic-toplevel-attribs set-schematic-toplevel-attribs!
            schematic-tree set-schematic-tree!
            schematic-netlist set-schematic-netlist!
            schematic-graphicals set-schematic-graphicals!
            schematic-non-unique-packages set-schematic-non-unique-packages!
            schematic-packages set-schematic-packages!
            schematic-non-unique-nets set-schematic-non-unique-nets!
            schematic-nets set-schematic-nets!
            make-toplevel-schematic
            schematic-toplevel-attrib))

(define-record-type <schematic>
  (make-schematic id
                  toplevel-pages
                  toplevel-attribs
                  tree
                  netlist
                  graphicals
                  non-unique-packages
                  packages
                  non-unique-nets
                  nets)
  schematic?
  (id schematic-id set-schematic-id!)
  (toplevel-pages schematic-toplevel-pages set-schematic-toplevel-pages!)
  (toplevel-attribs schematic-toplevel-attribs set-schematic-toplevel-attribs!)
  (tree schematic-tree set-schematic-tree!)
  (netlist schematic-netlist set-schematic-netlist!)
  (graphicals schematic-graphicals set-schematic-graphicals!)
  (non-unique-packages schematic-non-unique-packages set-schematic-non-unique-packages!)
  (packages schematic-packages set-schematic-packages!)
  (non-unique-nets schematic-non-unique-nets set-schematic-non-unique-nets!)
  (nets schematic-nets set-schematic-nets!))

(set-record-type-printer!
 <schematic>
 (lambda (record port) (format port "#<geda-schematic ~A>" (schematic-id record))))


(define next-schematic-id #f)

;;; Closure to return a new schematic id.
(let ((id 0))
  (set! next-schematic-id
        (lambda () (set! id (1+ id)) (1- id))))


;;; For now, we just make a schematic SXML tree from the list of
;;; pages given on the command line.
(define (schematic->sxml netlist toplevel-pages)
  (define (page->sxml page)
    (define (source-page source)
      (let loop ((pages (active-pages)))
        (and (not (null? pages))
             (if (string=? source (basename (page-filename (car pages))))
                 (page->sxml (car pages))
                 (loop (cdr pages))))))

    ;; Given a list of strings, some of which may contain commas,
    ;; splits comma separated strings and returns the new combined
    ;; list
    (define (comma-separated->list ls)
      (append-map (lambda (s) (string-split s #\,)) ls))

    (define (composite->sxml p)
      (let ((sources (and=> (package-attributes p 'source)
                            comma-separated->list)))
        (if sources
            `(package ,p ,@(map source-page sources))
            `(package ,p))))

    (define (non-composite->sxml p)
      `(package ,p))

    (define (component->sxml p)
      (and (string=? (basename (page-filename (object-page (package-object p))))
                     (basename (page-filename page)))
           ((if (package-composite? p) composite->sxml non-composite->sxml) p)))

    `(page ,page ,@(filter-map component->sxml netlist)))


  `(*TOP* (package "TOPLEVEL"
                   ,@(map page->sxml toplevel-pages))))

;;; Gets non unique set of package refdeses.
;;; Backward compatibility procedure for old backends.
(define (non-unique-packages netlist)
  (sort (filter-map package-refdes netlist) refdes<?))

;;; Returns a sorted list of unique packages in NETLIST.
(define (get-packages non-unique-packages)
  ;; Uniqueness of packages is guaranteed by the hashtable.
  (define ht (make-hash-table (length non-unique-packages)))
  (define (get-value key value) value)
  (for-each (lambda (s) (hashq-set! ht (string->symbol s) s))
            non-unique-packages)
  (sort (hash-map->list get-value ht) refdes<?))


;;; Returns a list of all pin nets in NETLIST.
(define (get-all-nets netlist)
  (define (connected? pin)
    (let ((netname (package-pin-name pin)))
      (and (string? netname)
           (not (string-prefix? "unconnected_pin" netname))
           netname)))

  (append-map
   (lambda (package)
     (filter-map connected? (package-pins package)))
   netlist))

;;; Returns a sorted list of unique nets in NETLIST.
(define (get-nets netlist)
  (sort-remove-duplicates (get-all-nets netlist)
                          refdes<?))


(define (get-toplevel-attributes toplevel-pages)
  (define (toplevel-attrib? object)
    (and (attribute? object)
         (cons (string->symbol (attrib-name object))
               (attrib-value object))))

  (filter-map toplevel-attrib? (append-map page-contents toplevel-pages)))


(define (make-toplevel-schematic toplevel-pages netlist-mode)
  "Creates a new schematic record based on TOPLEVEL-PAGES which
must be a list of pages."
  (let* ((id (next-schematic-id))
         (toplevel-attribs (get-toplevel-attributes toplevel-pages))
         (full-netlist (traverse toplevel-pages netlist-mode))
         (netlist (filter-map
                   (lambda (x) (and (not (package-graphical? x)) x))
                   full-netlist))
         (graphicals (filter-map
                      (lambda (x) (and (package-graphical? x) x))
                      full-netlist))
         (tree (schematic->sxml netlist toplevel-pages))
         (nu-packages (non-unique-packages netlist))
         (packages (get-packages nu-packages))
         (nu-nets (get-all-nets netlist))
         (nets (get-nets netlist)))
    (make-schematic id
                    toplevel-pages
                    toplevel-attribs
                    tree
                    netlist
                    graphicals
                    nu-packages
                    packages
                    nu-nets
                    nets)))

(define (schematic-toplevel-attrib schematic attrib-name)
  "Returns value of toplevel attribute ATTRIB-NAME for SCHEMATIC."
  (assq-ref (schematic-toplevel-attribs schematic) attrib-name))
