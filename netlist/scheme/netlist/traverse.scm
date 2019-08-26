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

  #:use-module ((ice-9 match))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (geda attrib)
  #:use-module (geda object)
  #:use-module (geda log)
  #:use-module (geda page)
  #:use-module (lepton library)
  #:use-module (netlist config)
  #:use-module (netlist hierarchy)
  #:use-module (netlist mode)
  #:use-module (netlist net)
  #:use-module (netlist option)
  #:use-module (netlist package-pin)
  #:use-module (netlist page)
  #:use-module (netlist pin-net)
  #:use-module (netlist schematic-component)
  #:use-module (netlist schematic-connection)
  #:use-module (netlist verbose)
  #:use-module (symbol check net-attrib)

  #:export (traverse))


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
         #f
         ;; pins
         '())
        (let ((group (car groups)))
          (if (or (equal? netname (schematic-connection-name group))
                  (equal? netname (schematic-connection-override-name group)))
              group
              (loop (cdr groups)))))))

(define (object-pins->package-pins object tag connections)
  (define (make-pin-attrib-list object)
    (define (add-attrib attrib)
      (cons (string->symbol (attrib-name attrib))
            (attrib-value attrib)))

    (map add-attrib (object-attribs object)))

  (define (nets-netnames nets)
    (filter-map
     (lambda (x) (let ((object (pin-net-object x)))
              (and (net? object)
                   (attrib-value-by-name object "netname"))))
     nets))

  (define (assign-net-netname! net)
    ;; The object is a net.  For nets we check the "netname="
    ;; attribute.
    (set-pin-net-name!
     net
     (create-net-name (attrib-value-by-name (pin-net-object net) "netname")
                      tag
                      ;; The below means just #f.
                      (not 'power-rail))))

  (define (assign-pin-properties! pin)
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

  (define (set-package-pin-connection-properties! pin)
    (let* ((pin-object (package-pin-object pin))
           (connection (get-package-pin-connection pin-object connections))
           (nets (map make-new-pin-net (traverse-net pin-object)))
           (net-objects (filter (lambda (x) (net? (pin-net-object x))) nets))
           (pin-objects (filter (lambda (x) (pin? (pin-net-object x))) nets)))
      (set-package-pin-connection! pin connection)
      (schematic-connection-add-pin! connection pin)
      (set-package-pin-nets! pin nets)
      (set-package-pin-netname! pin (nets-netnames nets))
      (for-each assign-net-netname! net-objects)
      (for-each assign-pin-properties! pin-objects)
      pin))

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
                        #f)))

  (map set-package-pin-connection-properties!
       (map object->package-pin
            (filter net-pin? (component-contents object)))))


;;; Searches for pinnumers in NET-MAPS and, if found, updates
;;; corresponding pins in PIN-LIST, otherwise creates new pins and
;;; adds them to the list.  ID, REFDES, and hierarchy TAG are used
;;; to create hierarchical net name.
(define (net-maps->package-pins net-maps id refdes tag pin-list connections)
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

  (define (make-net-map-pin net-map)
    (let* ((pinnumber (net-map-pinnumber net-map))
           (netname (create-net-name (net-map-netname net-map)
                                      tag
                                      'power-rail))
           (label #f)
           (object #f)
           (attribs '())
           (nets (list (make-pin-net id object #f netname refdes pinnumber)))
           (connection (get-connection-by-netname netname connections tag))
           (pin (make-package-pin id
                                  object
                                  pinnumber
                                  netname
                                  '()
                                  label
                                  attribs
                                  net-map
                                  nets
                                  #f
                                  #f)))
      (set-package-pin-connection! pin connection)
      (schematic-connection-add-pin! connection pin)
      pin))

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
          (make-net-map-pin net-map))))

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
  (let ((filename (get-source-library-file name)))
    (if filename
        (filename->page filename 'new-page)
        (log! 'error (_ "Failed to load subcircuit ~S.") name))))


(define (special-refdes object attribs net-maps graphical)
  ;; Get refdes= of OBJECT depending on NETLIST-MODE.
  (define (get-refdes attribs)
    (let ((refdes (and=> (assq-ref attribs 'refdes) car)))
      (case (netlist-mode)
        ((spice)
         (let ((slot (and=> (assq-ref attribs 'slot) car)))
           (if slot
               (string-append refdes "." slot)
               refdes)))
        ((geda) refdes)
        (else (error (_ "Netlist mode ~S is not supported.") (netlist-mode))))))

  ;; First try to get refdes from attribs.
  (or (get-refdes attribs)
      ;; If there is net=, it's a power or some other special
      ;; graphical symbol.  In such a case, refdes is #f.
      (and (null? net-maps)
           (not graphical)
           ;; Otherwise, refdes is just missing.  Warn the user, and
           ;; make up an artificial refdes.
           (log! 'critical
                 (_ "\nNon-graphical symbol ~S\nat ~A on page ~S\nhas neither refdes= nor net=.")
                 (component-basename object)
                 (component-position object)
                 (page-filename (object-page object)))
           "U?")))


(define (traverse-object object connections hierarchy-tag)
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
                                              hierarchy-tag
                                              #f ; get sources later
                                              object
                                              inherited-attribs
                                              attached-attribs
                                              ;; get pins later
                                              '()
                                              ;; not a port initially
                                              #f))
         (graphical (or (schematic-component-graphical? component)
                        (schematic-component-nc? component)))
         (refdes  (hierarchy-create-refdes (special-refdes object
                                                           attached-attribs
                                                           net-maps
                                                           graphical)
                                           hierarchy-tag))
         (sources (get-sources graphical
                               inherited-attribs
                               attached-attribs))
         (real-pins (object-pins->package-pins object hierarchy-tag connections))
         (net-map-pins (net-maps->package-pins net-maps
                                       id
                                       refdes
                                       hierarchy-tag
                                       real-pins
                                       connections))
         (pins (append real-pins net-map-pins)))
    (set-schematic-component-refdes! component refdes)
    (set-schematic-component-sources! component sources)
    (set-schematic-component-pins/parent! component pins)
    component))


(define (traverse-page page hierarchy-tag)
  (when hierarchy-tag
    (log! 'message (_ "Going to traverse source ~S") (page-filename page)))

  (let* ((connections (make-page-schematic-connections page hierarchy-tag)))
    (map (cut traverse-object <> connections hierarchy-tag)
         (filter component? (page-contents page)))))


;;; Traverses pages obtained from files defined in the 'source='
;;; attributes of COMPONENT with respect to HIERARCHY-TAG and
;;; NETLIST-MODE.
(define (traverse-component-sources component hierarchy-tag)
  (let ((hierarchy-tag (schematic-component-refdes component))
        (source-pages (map hierarchy-down-schematic
                           (schematic-component-sources component))))
    (traverse-pages source-pages hierarchy-tag)))

(define (traverse-pages pages hierarchy-tag)
  (let* ((schematic-components (append-map (cut traverse-page <> hierarchy-tag) pages))
         (composites (filter schematic-component-sources schematic-components))
         ;; Traverse underlying schematics.
         (underlying-components (append-map (cut traverse-component-sources
                                                 <>
                                                 hierarchy-tag)
                                            composites)))
    (append schematic-components underlying-components)))

(define (traverse toplevel-pages)
  (hierarchy-post-process (traverse-pages toplevel-pages
                                          '() ; toplevel hierarchy tag
                                          )))
