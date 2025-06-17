;;; Lepton EDA netlister
;;; Copyright (C) 2019-2022 Lepton EDA Contributors
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

;;; Subschematic is schematic of a list of pages usually defined
;;; in the "source=" attributes of a component.

(define-module (netlist subschematic)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)

  #:use-module (lepton gettext)
  #:use-module (lepton library)
  #:use-module (lepton log)
  #:use-module (lepton object)
  #:use-module (lepton page)
  #:use-module (netlist attrib refdes)
  #:use-module (netlist attrib compare)
  #:use-module (netlist config)
  #:use-module (netlist option)
  #:use-module (netlist package-pin)
  #:use-module (netlist schematic-component)
  #:use-module (netlist schematic-connection)
  #:use-module (netlist schematic-port)
  #:use-module (netlist subschematic-connection)
  #:use-module (symbol check duplicate)
  #:use-module (symbol check net-attrib)

  #:export-syntax (make-subschematic subschematic?
                   subschematic-name set-subschematic-name!
                   subschematic-parent set-subschematic-parent!
                   subschematic-pages set-subschematic-pages!
                   subschematic-components set-subschematic-components!
                   subschematic-connections set-subschematic-connections!)

  #:export (page-list->hierarchical-subschematic
            schematic-component-ports
            make-hierarchical-connections
            make-hierarchical-connection-name))

(define-record-type <subschematic>
  (make-subschematic name parent pages components connections)
  subschematic?
  (name subschematic-name set-subschematic-name!)
  (parent subschematic-parent set-subschematic-parent!)
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
           ;; Assign parent component later.
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


(define* (make-schematic-component-refdes* component #:optional hierarchical?)
  (define object (schematic-component-object component))
  (define attribs (schematic-component-attribs component))
  (define has-net? (not (null? (schematic-component-net-maps component))))
  (define graphical? (or (schematic-component-graphical? component)
                         (schematic-component-nc? component)))
  (define plain-symbol? (and (not has-net?)
                             (not graphical?)))
  (define hierarchy-tag
    (and hierarchical?
         (subschematic-name (schematic-component-parent component))))

  ;; First try to get refdes from attribs.
  (or (make-refdes attribs hierarchy-tag)
      ;; If no refdes found, make a mock one for non-special
      ;; symbols.  For graphical symbols, or for symbols having
      ;; the "net=" attribute, which are considered to be power
      ;; or some other special symbols, it is #f.
      (and plain-symbol? (make-mock-refdes object hierarchy-tag))))


(define (make-schematic-component-refdes component)
  (set-schematic-component-refdes!
   component
   (make-schematic-component-refdes*
    component
    (netlist-config-ref 'mangle-refdes))))


(define* (page-list->subschematic pages #:optional name)
  "Creates a new subschematic from the PAGES list.  If specified,
NAME is used as its hierarchical name."
  (let* ((subschematics (map page->subschematic pages))
         (components (append-map subschematic-components subschematics))
         (connections (make-subschematic-connections components))
         (subschematic (make-subschematic name #f pages components connections)))
    (for-each (cut set-schematic-connection-parent! <> subschematic)
              connections)
    (for-each (cut set-schematic-component-parent! <> subschematic)
              components)
    (for-each make-schematic-component-refdes components)
    subschematic))


(define (hierarchy-down-schematic name)
  (define quiet-mode (netlist-option-ref 'quiet))

  (let ((filename (get-source-library-file name)))
    (if filename
        (begin
          (unless quiet-mode
            (log! 'message (G_ "Loading subcircuit ~S.") filename))
          (file->page filename 'new-page))
        (begin
          (log! 'critical (G_ "Failed to load subcircuit ~S.") name)
          #f))))


(define (page-list->hierarchical-subschematic pages hierarchy-tag)
  (define (traverse-component-sources component)
    (and (schematic-component-sources component)
         (let* ((hierarchy-tag (make-schematic-component-refdes* component
                                                                 'hierarchical))
                (source-pages (filter-map hierarchy-down-schematic
                                          (schematic-component-sources component)))
                ;; Recursive processing of sources.
                (subschematic (page-list->hierarchical-subschematic source-pages hierarchy-tag)))
           (set-schematic-component-subschematic! component subschematic)
           (set-subschematic-parent! subschematic component)
           component)))

  (let ((subschematic (page-list->subschematic pages hierarchy-tag)))
    ;; Traverse pages obtained from files defined in the 'source='
    ;; attributes of schematic components.
    (for-each traverse-component-sources
              (subschematic-components subschematic))

    subschematic))
(define (warn-no-pinlabel pin)
  (or (package-pin-label pin)
      (begin
        (log! 'critical
              (G_ "Pin ~S of the component ~S has no \"pinlabel\" attribute.")
              (package-pin-number pin)
              (schematic-component-refdes (package-pin-parent pin)))
        #f)))


(define (warn-no-inner-pins component)
  (log! 'critical (G_ "Port component ~S has no pins.")
        (schematic-component-refdes component))
  #f)

;;; Warn if no port found in the subcircuit. Return #f.
(define (warn-no-port pin)
  (log! 'critical
        (G_ "Source schematic of the component ~S has no port with \"refdes=~A\".")
        (schematic-component-refdes (package-pin-parent pin))
        (package-pin-label pin))
  #f)


(define (warn-one-pin-multiple-components pin)
  (log! 'critical
        (G_ "There are several subschematic components for the pin with \"pinlabel=~A\" of the component ~S.")
        (package-pin-label pin)
        (component-basename (schematic-component-object (package-pin-parent pin)))))


(define (warn-duplicate-pinlabel arg)
  (if (list? arg)
      (begin
        (log! 'critical
              (G_ "Pins with numbers ~A of the component ~S have the same \"pinlabel\" attribute.")
              (string-join (map package-pin-number  arg) ", ")
              (schematic-component-refdes (package-pin-parent (car arg))))
        ;; Return first duplicate pin.
        (car arg))
      arg))


(define (component-refdes<? x y)
  (string<
   (schematic-component-simple-refdes x)
   (schematic-component-simple-refdes y)))


(define (component-refdes=? x y)
  (string=
   (schematic-component-simple-refdes x)
   (schematic-component-simple-refdes y)))


(define (pinlabel<? x y)
  (string< (package-pin-label x) (package-pin-label y)))


(define (pinlabel=? x y)
  (string= (package-pin-label x) (package-pin-label y)))


(define (check-schematic-component-pins pins)
  (map warn-duplicate-pinlabel
       (list->duplicate-list (filter warn-no-pinlabel pins)
                             pinlabel<?
                             pinlabel=?)))


(define (schematic-component-port-pairs component)
  (let ((pins
         (check-schematic-component-pins (schematic-component-pins component)))
        (components
         (list->duplicate-list
          (filter schematic-component-simple-refdes
                  (subschematic-components
                   (schematic-component-subschematic component)))
          component-refdes<?
          component-refdes=?)))
    (let loop ((pins pins)
               (components components)
               (result '()))
      (if (null? pins)
          result
          (if (null? components)
              (begin
                (warn-no-port (car pins))
                (loop (cdr pins) components result))
              ;; both pins and components exist
              (let* ((c (car components))
                     (c* (if (list? c) (car c) c))
                     (p (car pins)))
                ;; A port component inside subcircuit (inner
                ;; component) is considered to be matching to the
                ;; outer (parent) component port pin if the refdes
                ;; of the former is the same as the value of the
                ;; "pinlabel=" attribute of the latter.
                (if (string= (package-pin-label p) (schematic-component-simple-refdes c*))
                    (if (list? c)
                        (begin (warn-one-pin-multiple-components p)
                               (loop (cdr pins) (cdr components) (cons (cons p c*) result)))
                        ;; c is simple component
                        (loop (cdr pins) (cdr components) (cons (cons p c*) result)))
                    ;; different pinlabel and component refdes
                    (if (string> (package-pin-label p) (schematic-component-simple-refdes c*))
                        ;; If the pinlabel of pin pinlabel is greater than the component
                        ;; refdes, we consider the component being internal one.
                        ;; Then just continue with the next component, drop the current one.
                        (loop pins (cdr components) result)
                        ;; Otherwise, it's obvious that the pin has no correspondent component.
                        (begin
                          (warn-no-port p)
                          (loop (cdr pins) components result))))))))))


(define (pin-component-pair->schematic-port p)
  (let* ((outer-pin (car p))
         (component (cdr p))
         (inner-pins (schematic-component-pins component)))
    (if (null? inner-pins)
        (warn-no-inner-pins component)
        ;; FIXME: It's assumed that only one pair of matching pins
        ;; found, that is, the port has only one pin.
        (let* ((inner-pin (car inner-pins))
               (port (make-schematic-port inner-pin outer-pin)))
          ;; Update schematic component representing the port.
          (set-schematic-component-port! component port)
          ;; Return new <schematic-port> created.
          port))))


(define (schematic-component-ports component)
  (filter-map pin-component-pair->schematic-port
              (schematic-component-port-pairs component)))


(define (subschematic-ports subschematic)
  (define (collect-components subschematic)
    (append (subschematic-components subschematic)
            (append-map collect-components
                        (filter-map schematic-component-subschematic
                                    (subschematic-components subschematic)))))

  (append-map schematic-component-ports
              (filter schematic-component-subschematic
                      (collect-components subschematic))))


(define (connected-port-connections? ls1 ls2)
  (define (connected-to-ls2? x)
    (member x ls2))
  (any connected-to-ls2? ls1))


(define (reconnect-connections ls groups)
  (define (is-ls-connected-to? group)
    (connected-port-connections? ls group))

  (receive (connected unconnected)
      (partition is-ls-connected-to? groups)
    (let ((result
           `(,@unconnected
             ,(if (null? connected)
                  ls
                  (apply append ls connected)))))
      result)))


(define (group-connections ls)
  (fold reconnect-connections '() ls))


(define (make-port-connection group)
  (define (merge-objects ls)
    (delete-duplicates (append-map schematic-connection-objects ls)))

  (define (merge-pins ls)
    (delete-duplicates (append-map schematic-connection-pins ls)))

  (let* ((id #f)
         (page #f)
         (netnames (group-hierarchical-name group
                                            (netlist-config-ref
                                             'netname-attribute-priority)))
         (objects (merge-objects group))
         (pins (merge-pins group))
         (connection (make-schematic-connection
                      id
                      ;; Parent subschematic.
                      #f
                      page
                      netnames
                      #f
                      objects
                      pins)))

    (for-each
     (cut set-package-pin-port-connection! <> connection)
     pins)
    connection))


(define (collect-connections subschematic)
  (append (subschematic-connections subschematic)
          (append-map collect-connections
                      (filter-map schematic-component-subschematic
                                  (subschematic-components subschematic)))))


(define (make-hierarchical-connection-name c)
  (car (schematic-connection-name c)))


(define (name<? a b prefer-netname?)
  (define (unnamed? x)
    (eq? 'unnamed (car x)))
  (define (length< a b)
    (< (length a) (length b)))
  (define (length= a b)
    (= (length a) (length b)))
  (define (ls->str x)
    (string-join (cdr x)))
  (define (refdes-list< a b)
    (refdes<? (ls->str a) (ls->str b)))
  (define (preferred? x)
    (eq? (if prefer-netname? 'netname 'net)
         (car x)))
  (define (ls< a b)
    (or (length< a b)
        (and (length= a b)
             (if (preferred? a)
                 (or (not (preferred? b))
                     (and (preferred? b)
                          (refdes-list< a b)))
                 (and (not (preferred? b))
                      (refdes-list< a b))))))
  (if (unnamed? a)
      (and (unnamed? b)
           (ls< a b))
      (or (unnamed? b)
          (ls< a b))))


(define (connection-hierarchical-name connection)
  (let* ((netnames (schematic-connection-name connection))
         (nets (schematic-connection-override-name connection))
         (tag (subschematic-name (schematic-connection-parent connection)))
         (names (append (map (cut list 'netname <>) netnames)
                        (map (cut list 'net <>) nets))))
    (map (cut append <> tag)
         (if (null? names) '((unnamed)) names))))


(define (group-hierarchical-name group prefer-netname?)
  (define (name<?* a b)
    (name<? a b prefer-netname?))
  (sort
   (delete-duplicates (append-map connection-hierarchical-name group))
   name<?*))


(define (make-hierarchical-connections subschematic)
  (define (port->ls p)
    (list (package-pin-named-connection (schematic-port-inner-pin p))
          (package-pin-named-connection (schematic-port-outer-pin p))))

  (define port-connection-pairs
    (map port->ls (subschematic-ports subschematic)))

  (define port-connections
    (apply append port-connection-pairs))

  (define (no-port? connection)
    (not (member connection port-connections)))

  (let* ((connections (collect-connections subschematic))
         (simple-connections (filter no-port? connections))
         (new-port-connections
          (delete-duplicates (group-connections port-connection-pairs)))
         (new-connections (append new-port-connections
                                  (map list simple-connections))))
    (map make-port-connection new-connections)))
