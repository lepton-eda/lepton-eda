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

  #:use-module (ice-9 match)
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
  #:use-module (netlist subschematic-connection)
  #:use-module (netlist verbose)
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


;;; Lookups for pinnumber and parent component's refdes for PIN.
;;; If they're somehow wrong, warns the users and sets new
;;; appropriate values.  Returns the pair (refdes . pinnumber),
;;; fixed if needed.
(define (pin-refdes-pinnumber-pair pin)
  (let ((refdes (attrib-value-by-name (object-component pin)
                                      "refdes"))
        (pinnumber (attrib-value-by-name pin "pinnumber")))
    (match `(,refdes . ,pinnumber)
      ;; Wrong case, neither refdes nor pinnumber found.
      ((#f . #f)
       (log! 'critical (_ "Missing attributes refdes= and pinnumber="))
       '("U?" . "?"))
      ;; Missing pin number while refdes exists.
      ((refdes . #f)
       (log! 'critical (_ "Missing pinnumber= for refdes=~A)") refdes)
       `(,refdes . "?"))
      ;; Otherwise, anything is OK, return it as is.  Even if
      ;; refdes=#f and pinnumber is non-#f, it is an acceptable case
      ;; for using with the "net=" attribute. Return it as is.
      (x x))))


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
(define (get-connection-by-netname netname connections tag)
  (let loop ((groups connections))
    (if (null? groups)
        (make-schematic-connection
         ;; id
         #f
         ;; hierarchical tag
         tag
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
          (if (or (equal? netname (schematic-connection-name group))
                  (equal? netname (schematic-connection-override-name group)))
              group
              (loop (cdr groups)))))))

(define (object-pins->package-pins object)
  (define (make-pin-attrib-list object)
    (define (add-attrib attrib)
      (cons (string->symbol (attrib-name attrib))
            (attrib-value attrib)))

    (map add-attrib (object-attribs object)))

  (define (object->package-pin pin-object)
    (let ((attribs (make-pin-attrib-list pin-object)))
      (make-package-pin (object-id pin-object)
                        ;; Primitive pin object.
                        pin-object
                        ;; Number.
                        (assq-ref attribs 'pinnumber)
                        ;; Add name later.
                        #f
                        ;; Add netname list later.
                        #f
                        ;; Label.
                        (assq-ref attribs 'pinlabel)
                        ;; Attributes.
                        attribs
                        ;; No net-map yet.
                        #f
                        ;; No nets yet.
                        #f
                        ;; Set parent component later.
                        #f
                        ;; No connection yet.
                        #f
                        ;; No netname connection yet.
                        #f)))

  (map object->package-pin
       (filter net-pin? (component-contents object))))


;;; Searches for pinnumers in NET-MAPS and, if found, updates
;;; corresponding pins in PIN-LIST, otherwise creates new pins and
;;; adds them to the list.
(define (net-maps->package-pins net-maps pin-list)
  (define (pinnumber->pin pinnumber pin-list)
    (and (not (null? pin-list))
         (let ((package-pinnumber (package-pin-number (car pin-list))))
           ;; FIXME: a pin may have no "pinnumber=", and we have
           ;; to deal with such cases. A test and drc check is
           ;; needed.
           (if (and package-pinnumber
                    (string=? package-pinnumber pinnumber))
               (car pin-list)
               (pinnumber->pin pinnumber (cdr pin-list))))))

  (define (make-or-update-net-map-pin net-map)
    (let ((pin (pinnumber->pin (net-map-pinnumber net-map)
                               pin-list)))
      (if pin
          ;; If pin exists, just assign net-map for it.
          (begin
            (set-package-pin-net-map! pin net-map)
            ;; Return #f to filter out existing pins.
            #f)
          ;; Otherwise, make a new virtual pin.
          (make-package-pin #f
                            #f
                            (net-map-pinnumber net-map)
                            #f
                            '()
                            #f
                            '()
                            net-map
                            #f
                            #f
                            #f
                            #f))))

  ;; Create virtual 'net-map' pins.
  (filter-map make-or-update-net-map-pin net-maps))


(define (get-sources graphical? inherited-attribs attached-attribs)
  (define (non-null* ls)
    (and (not (null? ls)) ls))

  ;; Given a list of strings, some of which may contain commas,
  ;; splits comma separated strings and returns the new combined
  ;; list
  (define (comma-separated->list ls)
    (append-map (lambda (s) (string-split s #\,)) ls))

  (and (not graphical?)
       (gnetlist-config-ref 'traverse-hierarchy)
       (let ((sources
              (or (non-null* (assq-ref attached-attribs 'source))
                  (non-null* (assq-ref inherited-attribs 'source)))))
         (and=> sources comma-separated->list))))


(define (hierarchy-down-schematic name)
  (define quiet-mode (netlist-option-ref 'quiet))

  (let ((filename (get-source-library-file name)))
    (if filename
        (begin
          (unless quiet-mode
            (log! 'message (_ "Loading subcircuit ~S.") filename))
          (file->page filename 'new-page))
        (log! 'error (_ "Failed to load subcircuit ~S.") name))))


(define (create-schematic-component-refdes component)
  (define object (schematic-component-object component))
  (define attribs (schematic-component-attribs component))
  (define net-maps (schematic-component-net-maps component))
  (define graphical? (or (schematic-component-graphical? component)
                         (schematic-component-nc? component)))
  (define hierarchy-tag (schematic-component-tag component))

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
   ;; priority
   #f
   ;; name
   #f
   ;; connection-package
   #f
   ;; connection-pinnumber
   #f))


(define (nets-netnames nets)
  (filter-map
   (lambda (x) (let ((object (pin-net-object x)))
            (and (net? object)
                 (attrib-value-by-name object "netname"))))
   nets))


(define (assign-net-netname! net tag)
  ;; The object is a net.  For nets we check the "netname="
  ;; attribute.
  (set-pin-net-name!
   net
   (create-net-name (attrib-value-by-name (pin-net-object net) "netname")
                    tag
                    ;; The below means just #f.
                    (not 'power-rail))))


(define (assign-pin-properties! pin tag)
  (let* ((object (pin-net-object pin))
         (refdes-pinnumber-pair (pin-refdes-pinnumber-pair object))
         (refdes (car refdes-pinnumber-pair))
         (pinnumber (cdr refdes-pinnumber-pair))
         (net-driven? (net-attrib-pin? object)))
    ;; The object is a pin, and it defines net name using
    ;; "net=".  Use hierarchy tag here to make this netname
    ;; unique.
    (set-pin-net-name!
     pin
     (create-net-name (netattrib-search-net (object-component object)
                                            pinnumber)
                      tag
                      'power-rail))
    (if net-driven?
        (set-pin-net-priority! pin net-driven?)
        (begin
          (set-pin-net-connection-package! pin
                                           (hierarchy-create-refdes refdes
                                                                    tag))
          (set-pin-net-connection-pinnumber! pin pinnumber)))))


(define (set-real-package-pin-connection-properties! pin connections)
  (let* ((tag (schematic-component-tag (package-pin-parent pin)))
         (pin-object (package-pin-object pin))
         (connection (get-package-pin-connection pin-object connections))
         (nets (map make-new-pin-net (traverse-net pin-object)))
         (net-objects (filter (lambda (x) (net? (pin-net-object x))) nets))
         (pin-objects (filter (lambda (x) (pin? (pin-net-object x))) nets)))
    (set-package-pin-connection! pin connection)
    (schematic-connection-add-pin! connection pin)
    (set-package-pin-nets! pin nets)
    (set-package-pin-netname! pin (nets-netnames nets))
    (for-each (cut assign-net-netname! <> tag) net-objects)
    (for-each (cut assign-pin-properties! <> tag) pin-objects)
    pin))


(define (set-net-map-package-pin-connection-properties! pin connections)
  (let* ((parent-component (package-pin-parent pin))
         (tag (schematic-component-tag parent-component))
         (refdes (schematic-component-refdes parent-component))
         (netname (create-net-name (net-map-netname (package-pin-net-map pin))
                                   tag
                                   'power-rail))
         (nets (list (make-pin-net (package-pin-id pin)
                                   (package-pin-object pin)
                                   #f
                                   netname
                                   refdes
                                   (package-pin-number pin))))
         (connection (get-connection-by-netname netname
                                                connections
                                                tag)))
    (set-package-pin-name! pin netname)
    (set-package-pin-nets! pin nets)
    (set-package-pin-connection! pin connection)
    (schematic-connection-add-pin! connection pin)))

(define (set-package-pin-connection-properties! component connections)
  (define (real-pin? pin)
    (package-pin-object pin))

  (define (set-properties! pin)
    (if (real-pin? pin)
        (set-real-package-pin-connection-properties! pin connections)
        (set-net-map-package-pin-connection-properties! pin connections)))

  (for-each set-properties! (schematic-component-pins component)))


(define (component->schematic-component object)
  ;; Makes attribute list of OBJECT using getter GET-ATTRIBS.
  (define (make-attrib-list get-attribs object)
    (define (add-attrib ls attrib)
      (let* ((name (string->symbol (attrib-name attrib)))
             (prev-value (assq-ref ls name))
             (new-value (attrib-value attrib)))
        (if prev-value
            (assq-set! ls name (cons new-value prev-value))
            (acons name (list new-value) ls))))

    (let loop ((in (get-attribs object))
               (out '()))
      (if (null? in)
          out
          (loop (cdr in)
                (add-attrib out (car in))))))

  (let* ((id (object-id object))
         (inherited-attribs (make-attrib-list inherited-attribs object))
         (attached-attribs (make-attrib-list object-attribs object))
         (net-maps (check-net-maps object))
         (component (make-schematic-component id
                                              #f ; get refdes later
                                              #f ; get hierarchy-tag later
                                              #f ; get sources later
                                              object
                                              inherited-attribs
                                              attached-attribs
                                              net-maps
                                              ;; get pins later
                                              '()
                                              ;; not a port initially
                                              #f
                                              ;; no subschematic
                                              #f))
         (graphical (or (schematic-component-graphical? component)
                        (schematic-component-nc? component)))
         (sources (get-sources graphical
                               inherited-attribs
                               attached-attribs))
         (real-pins (object-pins->package-pins object))
         (net-map-pins (net-maps->package-pins net-maps real-pins))
         (pins (append real-pins net-map-pins)))
    (set-schematic-component-sources! component sources)
    (set-schematic-component-pins/parent! component pins)
    component))


(define (page->subschematic page hierarchy-tag)
  (let ((connections (make-page-schematic-connections page hierarchy-tag))
        (components (map component->schematic-component
                         (filter component? (page-contents page)))))
    (for-each (cut set-schematic-component-tag! <> hierarchy-tag) components)
    (for-each create-schematic-component-refdes components)
    (for-each
     (cut set-package-pin-connection-properties! <> connections)
     components)
    (make-subschematic (page-filename page)
                       (list page)
                       components
                       connections)))


(define (subschematic-list->subschematic name subschematics)
  (let* ((pages (append-map subschematic-pages subschematics))
         (components (append-map subschematic-components subschematics))
         (subschematic (make-subschematic name pages components '())))
    (set-subschematic-connections! subschematic
                                   (make-subschematic-connections subschematic))
    subschematic))


;;; Traverses pages obtained from files defined in the 'source='
;;; attributes of COMPONENT with respect to HIERARCHY-TAG.
(define (traverse-component-sources component)
  (let* ((hierarchy-tag (schematic-component-refdes component))
         (source-pages (map hierarchy-down-schematic
                            (schematic-component-sources component)))
         (subschematic (page-list->subschematic source-pages hierarchy-tag)))
    (set-schematic-component-subschematic! component subschematic)
    component))


(define (page-list->subschematic pages hierarchy-tag)
  (let* ((page-subschematics (map (cut page->subschematic <> hierarchy-tag) pages))
         (subschematic (subschematic-list->subschematic hierarchy-tag
                                                        page-subschematics))
         (components (subschematic-components subschematic))
         (composites (filter schematic-component-sources components)))
    (for-each traverse-component-sources composites)
    subschematic))
