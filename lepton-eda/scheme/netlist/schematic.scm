;;; Lepton EDA netlister
;;; Copyright (C) 2016-2017 gEDA Contributors
;;; Copyright (C) 2017-2020 Lepton EDA Contributors
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

(define-module (netlist schematic)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (sxml match)
  #:use-module (sxml transform)
  #:use-module (lepton attrib)
  #:use-module (lepton object)
  #:use-module (lepton page)
  #:use-module (netlist attrib compare)
  #:use-module (netlist attrib refdes)
  #:use-module (netlist config)
  #:use-module (netlist duplicate)
  #:use-module (netlist hierarchy)
  #:use-module (netlist package)
  #:use-module (netlist schematic-component)
  #:use-module (netlist schematic-connection)
  #:use-module (netlist schematic-port)
  #:use-module (netlist subschematic)
  #:use-module (netlist package-pin)

  #:export-syntax (make-schematic schematic?
                   schematic-id set-schematic-id!
                   schematic-subschematic set-schematic-subschematic!
                   schematic-toplevel-pages set-schematic-toplevel-pages!
                   schematic-toplevel-attribs set-schematic-toplevel-attribs!
                   schematic-components set-schematic-components!
                   schematic-packages set-schematic-packages!
                   schematic-graphicals set-schematic-graphicals!
                   schematic-non-unique-nets set-schematic-non-unique-nets!
                   schematic-nets set-schematic-nets!
                   schematic-nc-nets set-schematic-nc-nets!
                   schematic-connections set-schematic-connections!)

  #:export (make-toplevel-schematic
            file-name-list->schematic
            page-list->schematic
            schematic-toplevel-attrib
            schematic-non-unique-package-names
            schematic-package-names
            schematic-pins
            schematic-ports
            schematic-tree
            schematic-name-tree
            schematic-components*))

(define-record-type <schematic>
  (make-schematic id
                  subschematic
                  toplevel-pages
                  toplevel-attribs
                  components
                  packages
                  graphicals
                  non-unique-nets
                  nets
                  nc-nets
                  connections)
  schematic?
  (id schematic-id set-schematic-id!)
  (subschematic schematic-subschematic set-schematic-subschematic!)
  (toplevel-pages schematic-toplevel-pages set-schematic-toplevel-pages!)
  (toplevel-attribs schematic-toplevel-attribs set-schematic-toplevel-attribs!)
  (components schematic-components set-schematic-components!)
  (packages schematic-packages set-schematic-packages!)
  (graphicals schematic-graphicals set-schematic-graphicals!)
  (non-unique-nets schematic-non-unique-nets set-schematic-non-unique-nets!)
  (nets schematic-nets set-schematic-nets!)
  (nc-nets schematic-nc-nets set-schematic-nc-nets!)
  (connections schematic-connections set-schematic-connections!))

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

    (define (composite->sxml p)
      (let ((sources (schematic-component-sources p)))
        (if sources
            `(package ,p ,@(map source-page sources))
            `(package ,p))))

    (define (non-composite->sxml p)
      `(package ,p))

    (define (component->sxml p)
      (and (string=? (basename (page-filename (object-page (schematic-component-object p))))
                     (basename (page-filename page)))
           ((if (schematic-component-sources p) composite->sxml non-composite->sxml) p)))

    `(page ,page ,@(filter-map component->sxml netlist)))


  `(*TOP* (package "TOPLEVEL"
                   ,@(map page->sxml toplevel-pages))))


(define (schematic-sxml->schematic-name-tree tree)
  (define (page-name p)
    (and (page? p)
         (basename (page-filename p))))

  (define (sxml-page-name page)
    (sxml-match
     page
     ((page (@ (page-name ,name)) ,deps ...) name)))

  (define (sxml-page-prerequisites page)
    (sxml-match page ((page (@ (prerequisites ,pr)) ,deps ...) pr)))

  ;; Package prerequisites are the names of its subcircuit pages
  ;; defined in its source= attribs and the subcircuit names the
  ;; pages depend on, i.e. their prerequisites. Here we append all
  ;; that info and remove duplicates.
  (define (package-prerequisites pages)
    (append (map sxml-page-name pages)
             (append-map sxml-page-prerequisites pages)))

  (define (sxml-package-name package)
    (sxml-match
     package
     ((package (@ (package-name ,name)) ,deps ...) name)))

  ;; SXML package-name attribute is #f if package is not composite
  ;; and has no file= attribute attached to it, so we filter such
  ;; packages out.
  (define (page-prerequisites packages)
    (filter-map sxml-package-name packages))

  (define (sxml-package-rules package)
    (sxml-match
     package
     ((package (@ (rules ,r)) ,deps ...) `(,r))))

  (define (page-rules packages)
    (filter (lambda (x) (not (null? x)))

    (append-map sxml-package-rules packages)))

  (define (sxml-page-rules page)
    (sxml-match
     page
     ((page (@ (rules ,r)) ,deps ...) r)))

  (define (package-rules pages)
         `(,@(package-prerequisites pages) ,@(append-map sxml-page-rules pages)))

  (pre-post-order
   tree
   `((*TOP* . ,(lambda (x . t) (sxml-package-rules (car t))))
     (page . ,(lambda (x pg . deps)
                `(page (@ (page-name ,(page-name pg))
                          (prerequisites ,(page-prerequisites deps))
                          (rules ,(page-rules deps))))))
     (package . ,(lambda (x pk . deps)
                   `(package (@ (package-name #f)
                                  (rules ,(package-rules deps))))))
     (*text* . ,(lambda (x t) t))
     (*default* . ,(lambda (x . t) t)))))


(define (schematic-name-tree schematic)
  "Recursively returns the names of all subschematic pages in SCHEMATIC."
  (schematic-sxml->schematic-name-tree (schematic-tree schematic)))


;;; Gets non unique set of package refdeses.
;;; Backward compatibility procedure for legacy backends.
(define (schematic-non-unique-package-names netlist)
  (sort (filter-map schematic-component-refdes netlist) refdes<?))

;;; Returns a sorted list of unique packages in NETLIST.
;;; Backward compatibility procedure for legacy backends.
(define (schematic-package-names schematic)
  (sort (map package-refdes
             (schematic-packages schematic))
        refdes<?))


;;; Returns a list of all pin nets in NETLIST.
(define (get-all-nets netlist)
  (define (connected? pin)
    (let ((netname (package-pin-name pin)))
      (and (string? netname)
           (not (string-prefix? "unconnected_pin" netname))
           netname)))

  (append-map
   (lambda (package)
     (filter-map connected? (schematic-component-pins package)))
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


;;; Returns #t if NETNAME is a "no-connect" net, that is, one of
;;; its connected symbols is one of PACKAGES and a "no-connect"
;;; symbol. Otherwise returns #f.
(define (nc-net? netname packages)
  (define (pin-netname-matches? pin)
    (string=? (package-pin-name pin) netname))

  (define (wanted-package-pin-netname=? package)
    (any pin-netname-matches? (schematic-component-pins package)))

  (any wanted-package-pin-netname=? packages))


(define (collect-components-recursively subschematic)
  (let* ((components (subschematic-components subschematic))
         (subschematics (filter-map schematic-component-subschematic components)))
    (append components
            (append-map collect-components-recursively subschematics))))


(define* (page-list->schematic pages)
  "Creates a new schematic record from PAGES, which must be a list
of schematic pages."
  (define (plain-package? x)
    (and (not (schematic-component-graphical? x))
         (not (schematic-component-nc? x))))

  (let* ((id (next-schematic-id))
         (toplevel-attribs (get-toplevel-attributes pages))
         ;; '() is toplevel hierarchy tag
         (subschematic (page-list->hierarchical-subschematic pages '()))
         (components (collect-components-recursively subschematic))
         (connections (make-hierarchical-connections subschematic))
         (full-netlist (hierarchy-post-process components connections))
         (netlist (filter plain-package? full-netlist))
         (packages (make-package-list netlist))
         (graphicals (filter schematic-component-graphical? full-netlist))
         (nu-nets (get-all-nets netlist))
         (unique-nets (get-nets netlist)))
    ;; Partition all unique net names into 'no-connection' nets
    ;; and plain nets.
    (receive (nc-nets nets)
        (partition (lambda (x)
                     (nc-net? x (filter schematic-component-nc? full-netlist)))
                   unique-nets)
      (make-schematic id
                      subschematic
                      pages
                      toplevel-attribs
                      netlist
                      packages
                      graphicals
                      nu-nets
                      nets
                      nc-nets
                      connections
                      ))))


(define (file-name-list->schematic filenames)
  "Creates a new schematic record from FILENAMES, which must be a
list of strings representing file names."
  (let ((pages (map file->page filenames)))
    (page-list->schematic pages)))


;;; An alias for the above procedure.
(define make-toplevel-schematic file-name-list->schematic)

(define (schematic-toplevel-attrib schematic attrib-name)
  "Returns value of toplevel attribute ATTRIB-NAME for SCHEMATIC."
  (assq-ref (schematic-toplevel-attribs schematic) attrib-name))

(define (schematic-tree schematic)
  (schematic->sxml (schematic-components schematic)
                   (schematic-toplevel-pages schematic)))

;;; This is a work-around to allow the 'schematic-components'
;;; syntax-transformer to be used in toplevel code.  This is known
;;; Guile bug appearing at least in the versions up to 2.2.
(define (schematic-components* schematic)
  (schematic-components schematic))

(define (schematic-ports schematic)
  "Returns a list of port components for SCHEMATIC."
  (filter-map schematic-component-port
              (schematic-components schematic)))


(define (schematic-pins schematic)
  "Returns a list of all component pins in SCHEMATIC."
  (append-map schematic-component-pins
              (schematic-components schematic)))
