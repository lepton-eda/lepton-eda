;;; Lepton EDA netlister
;;; Copyright (C) 2016-2017 gEDA Contributors
;;; Copyright (C) 2017-2019 Lepton EDA Contributors
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

(define-module (netlist traverse)

  ; Import C procedures and variables
  #:use-module (netlist core gettext)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (geda attrib)
  #:use-module (geda object)
  #:use-module (geda log)
  #:use-module (lepton library)
  #:use-module (lepton page)
  #:use-module (netlist config)
  #:use-module (netlist hierarchy)
  #:use-module (netlist mode)
  #:use-module (netlist net)
  #:use-module (netlist option)
  #:use-module (netlist package-pin)
  #:use-module (netlist pin-net)
  #:use-module (netlist schematic-component)
  #:use-module (netlist schematic-connection)
  #:use-module (netlist subschematic)
  #:use-module (symbol check net-attrib)

  #:export (page-list->subschematic))


;;; Tracks which objects have been visited so far, and how many
;;; times.
(define %visits '())
;;; Increment the current visit count for a particular OBJECT.
(define (visit! object)
  (set! %visits (cons object %visits)))
;;; Retrieve the current visit count for a particular OBJECT.
(define (visited? object)
  (member object %visits))
;;; Reset all visit counts. Simply clears the %visits completely.
(define (clear-visits!)
  (set! %visits '()))


(define (traverse-net pin-object)
  (define (traverse-net-object connection-objects starting object)
    (visit! object)
    (let ((nets (cons object connection-objects)))
      (if (or (net? object)
              starting)
          (let loop ((connections (object-connections object))
                     (nets nets))
            (if (null? connections)
                nets
                (loop (cdr connections)
                      (let ((conn (car connections)))
                        (if (visited? conn)
                            nets
                            (traverse-net-object nets #f conn))))))
          nets)))

  (clear-visits!)

  (if (null? (object-connections pin-object))
      ;; If there is no connections, we have an only pin. There is
      ;; no point to do something in this case.
      '()
      (reverse (traverse-net-object '() #t pin-object))))


(define (get-package-pin-connection pin-object connections)
  (let loop ((groups connections))
    (and (not (null? groups))
         (let ((group (car groups)))
           (or (and (member pin-object (schematic-connection-objects group)) group)
               (loop (cdr groups)))))))

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


(define (hierarchy-down-schematic name)
  (define quiet-mode (netlist-option-ref 'quiet))

  (let ((filename (get-source-library-file name)))
    (if filename
        (begin
          (unless quiet-mode
            (log! 'message (_ "Loading subcircuit ~S.") filename))
          (file->page filename 'new-page))
        (log! 'critical (_ "Failed to load subcircuit ~S.") name))))


(define (create-schematic-component-refdes component)
  (define object (schematic-component-object component))
  (define attribs (schematic-component-attribs component))
  (define net-maps (schematic-component-net-maps component))
  (define graphical? (or (schematic-component-graphical? component)
                         (schematic-component-nc? component)))
  (define hierarchy-tag
    (subschematic-name (schematic-component-parent component)))

  ;; Get refdes= of OBJECT depending on NETLIST-MODE.
  (define (get-refdes)
    (let ((refdes (and=> (assq-ref attribs 'refdes) car)))
      (case (netlist-mode)
        ((spice)
         (let ((slot (and=> (assq-ref attribs 'slot) car)))
           (if slot
               (string-append refdes "." slot)
               refdes)))
        ((geda) refdes)
        (else (error (_ "Netlist mode ~S is not supported.") (netlist-mode))))))

  (define (make-special-refdes)
    ;; If there is net=, it's a power or some other special
    ;; graphical symbol.  In such a case, refdes is #f.
    (and (null? net-maps)
         (not graphical?)
         ;; Otherwise, refdes is just missing.  Warn the user, and
         ;; make up an artificial refdes.
         (log! 'critical
               (_ "\nNon-graphical symbol ~S\nat ~A on page ~S\nhas neither refdes= nor net=.")
               (component-basename object)
               (component-position object)
               (page-filename (object-page object)))
         "U?"))

  (set-schematic-component-refdes!
   component
   (hierarchy-create-refdes
    ;; First try to get refdes from attribs.
    (or (get-refdes)
        ;; If no refdes found, make a mock one.
        (make-special-refdes))
    hierarchy-tag)))


(define (make-new-pin-net object)
  (make-pin-net
   ;; id
   (object-id object)
   ;; object
   object
   ;; name
   #f))


(define (nets-netnames nets)
  (filter-map
   (lambda (x) (let ((object (pin-net-object x)))
            (and (net? object)
                 (attrib-value-by-name object "netname"))))
   nets))


(define (set-real-package-pin-nets-properties! pin)
  (let* ((tag
          (subschematic-name (schematic-component-parent
                              (package-pin-parent pin))))
         (pin-object (package-pin-object pin))
         (nets (map make-new-pin-net (traverse-net pin-object)))
         (net-objects (filter (lambda (x) (net? (pin-net-object x))) nets))
         (pin-objects (filter (lambda (x) (pin? (pin-net-object x))) nets)))
    (set-package-pin-nets! pin nets)
    (set-package-pin-netname! pin (nets-netnames nets))
    (for-each (cut assign-net-netname! <> tag) net-objects)
    (for-each (cut assign-pin-properties! <> tag) pin-objects)
    pin))


(define (set-real-package-pin-connection! pin connections)
  (let ((connection (get-package-pin-connection (package-pin-object pin)
                                                connections)))
    (schematic-connection-add-pin! connection pin)
    pin))


(define (set-net-map-package-pin-nets-properties! pin)
  (let* ((parent-component (package-pin-parent pin))
         (tag (subschematic-name (schematic-component-parent parent-component)))
         (netname (create-net-name (net-map-netname (package-pin-net-map pin))
                                   tag
                                   'power-rail))
         (nets (list (make-pin-net (package-pin-id pin)
                                   (package-pin-object pin)
                                   netname))))
    (set-package-pin-nets! pin nets)))


(define (set-net-map-package-pin-connection! pin connections)
  (let ((connection (get-net-map-pin-connection pin connections)))
    (schematic-connection-add-pin! connection pin)))


(define (set-package-pin-nets-properties! component)
  (define (real-pin? pin)
    (package-pin-object pin))

  (define (set-nets-properties! pin)
    (if (real-pin? pin)
        (set-real-package-pin-nets-properties! pin)
        (set-net-map-package-pin-nets-properties! pin)))

  (for-each set-nets-properties! (schematic-component-pins component)))


(define (set-package-pin-connection-properties! component connections)
  (define (real-pin? pin)
    (package-pin-object pin))

  (define (set-connection-properties! pin)
    (if (real-pin? pin)
        (set-real-package-pin-connection! pin connections)
        (let* ((parent-component (package-pin-parent pin))
               (tag (cdr (subschematic-name (schematic-component-parent parent-component))))
               (netname (create-net-name (net-map-netname (package-pin-net-map pin))
                                         tag
                                         'power-rail)))

          (set-net-map-package-pin-connection! pin connections)
          (set-package-pin-name! pin netname))))

  (for-each set-connection-properties! (schematic-component-pins component)))


(define (page->subschematic* page hierarchy-tag)
  (let* ((subschematic (page->subschematic page))
         (connections (subschematic-connections subschematic))
         (components (subschematic-components subschematic)))
    (set-subschematic-name! subschematic
                            (cons (page-filename page) hierarchy-tag))
    (for-each
     (cut set-package-pin-connection-properties! <> connections)
     components)

    subschematic))


(define (page-list->subschematic pages hierarchy-tag)
  (define (traverse-component-sources component)
    (and (schematic-component-sources component)
         (let* ((hierarchy-tag (schematic-component-refdes component))
                (source-pages (map hierarchy-down-schematic
                                   (schematic-component-sources component)))
                ;; Recursive processing of sources.
                (subschematic (page-list->subschematic source-pages hierarchy-tag)))
           (set-schematic-component-subschematic! component subschematic)
           component)))

  (let* ((page-subschematics (map (cut page->subschematic* <> hierarchy-tag)
                                  pages))
         (subschematic (subschematic-list->subschematic page-subschematics
                                                        hierarchy-tag)))
    (set-subschematic-name! subschematic hierarchy-tag)

    (for-each create-schematic-component-refdes
              (subschematic-components subschematic))

    (for-each set-package-pin-nets-properties!
              (subschematic-components subschematic))

    ;; Traverse pages obtained from files defined in the 'source='
    ;; attributes of schematic components.
    (for-each traverse-component-sources
              (subschematic-components subschematic))
    subschematic))
