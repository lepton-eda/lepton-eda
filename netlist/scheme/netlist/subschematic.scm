;;; Lepton EDA netlister
;;; Copyright (C) 2019 Lepton EDA Contributors
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

;;; Subschematic is schematic of a list of pages usually defined
;;; in the "source=" attributes of a component.

(define-module (netlist subschematic)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (geda object)
  #:use-module (lepton page)
  #:use-module (netlist package-pin)
  #:use-module (netlist schematic-component)
  #:use-module (netlist schematic-connection)
  #:use-module (netlist subschematic-connection)
  #:use-module (symbol check net-attrib)

  #:export-syntax (make-subschematic subschematic?
                   subschematic-name set-subschematic-name!
                   subschematic-pages set-subschematic-pages!
                   subschematic-components set-subschematic-components!
                   subschematic-connections set-subschematic-connections!)

  #:export (page-list->subschematic))

(define-record-type <subschematic>
  (make-subschematic name pages components connections)
  subschematic?
  (name subschematic-name set-subschematic-name!)
  (pages subschematic-pages set-subschematic-pages!)
  (components subschematic-components set-subschematic-components!)
  (connections subschematic-connections set-subschematic-connections!))

;;; Sets default printer for <subschematic>
(set-record-type-printer!
 <subschematic>
 (lambda (record port) (format port
                          "#<subschematic-~A>"
                          (subschematic-name record))))

(define (get-package-pin-connection pin-object connections)
  (let loop ((groups connections))
    (and (not (null? groups))
         (let ((group (car groups)))
           (or (and (member pin-object (schematic-connection-objects group)) group)
               (loop (cdr groups)))))))


(define (set-real-package-pin-connection! pin connections)
  (let ((connection (get-package-pin-connection (package-pin-object pin)
                                                connections)))
    (schematic-connection-add-pin! connection pin)
    pin))


;;; Search for connection by netname.
(define (get-net-map-pin-connection pin connections)
  (define netname (net-map-netname (package-pin-net-map pin)))

  (define (netname-matches? connection)
    (or (equal? netname (schematic-connection-name connection))
        (equal? netname (schematic-connection-override-name connection))))

  (let loop ((groups connections))
    (if (null? groups)
        (make-schematic-connection
         ;; id
         #f
         ;; Parent subschematic.
         #f
         ;; page
         #f
         ;; netname
         #f
         ;; override-netname
         netname
         ;; objects
         '()
         ;; pins
         '())
        (let ((group (car groups)))
          (if (netname-matches? group)
              group
              (loop (cdr groups)))))))


(define (set-net-map-package-pin-connection! pin connections)
  (let ((connection (get-net-map-pin-connection pin connections)))
    (schematic-connection-add-pin! connection pin)))


(define (set-package-pin-connection-properties! component connections)
  (define (real-pin? pin)
    (package-pin-object pin))

  (define (set-connection-properties! pin)
    ((if (real-pin? pin)
         set-real-package-pin-connection!
         set-net-map-package-pin-connection!) pin connections))

  (for-each set-connection-properties! (schematic-component-pins component)))


(define (page->subschematic page)
  "Creates a new subschematic record from PAGE."
  (let* ((connections (make-page-schematic-connections page))
         (components (map component->schematic-component
                          (filter component? (page-contents page))))
         (subschematic
          (make-subschematic
           ;; Assign the name later.
           #f
           ;; One page in the list of pages.
           (list page)
           ;; Page components.
           components
           ;; Page connections.
           connections)))
    (for-each (cut set-schematic-connection-parent! <> subschematic)
              connections)
    (for-each (cut set-schematic-component-parent! <> subschematic)
              components)
    (for-each
     (cut set-package-pin-connection-properties! <> connections)
     components)

    subschematic))


(define* (page-list->subschematic pages #:optional name)
  "Creates a new subschematic from the PAGES list.  If specified,
NAME is used as its hierarchical name."
  (let* ((subschematics (map page->subschematic pages))
         (components (append-map subschematic-components subschematics))
         (connections (make-subschematic-connections components))
         (subschematic (make-subschematic name pages components connections)))
    (for-each (cut set-schematic-connection-parent! <> subschematic)
              connections)
    (for-each (cut set-schematic-component-parent! <> subschematic)
              components)
    subschematic))
