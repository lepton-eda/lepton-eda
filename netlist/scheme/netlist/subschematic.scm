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
  #:use-module (netlist schematic-component)
  #:use-module (netlist schematic-connection)
  #:use-module (netlist subschematic-connection)

  #:export-syntax (make-subschematic subschematic?
                   subschematic-name set-subschematic-name!
                   subschematic-pages set-subschematic-pages!
                   subschematic-components set-subschematic-components!
                   subschematic-connections set-subschematic-connections!)

  #:export (page->subschematic
            subschematic-list->subschematic))

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
    subschematic))


(define (subschematic-list->subschematic name subschematics)
  "Creates a new subschematic from the SUBSCHEMATICS list.  NAME
is used as its hierarchical name."
  (let* ((pages (append-map subschematic-pages subschematics))
         (components (append-map subschematic-components subschematics))
         (connections (make-subschematic-connections components name))
         (subschematic (make-subschematic name pages components connections)))
    (for-each (cut set-schematic-connection-parent! <> subschematic) connections)
    subschematic))
