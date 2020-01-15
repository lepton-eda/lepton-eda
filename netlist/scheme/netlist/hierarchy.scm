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

(define-module (netlist hierarchy)
  ;; Import C procedures and variables.
  #:use-module (netlist core gettext)

  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:use-module (geda log)
  #:use-module (geda object)
  #:use-module (netlist attrib refdes)
  #:use-module (netlist config)
  #:use-module (netlist core gettext)
  #:use-module (netlist mode)
  #:use-module (netlist net)
  #:use-module (netlist package-pin)
  #:use-module (netlist pin-net)
  #:use-module (netlist rename)
  #:use-module (netlist schematic-component)
  #:use-module (netlist schematic-connection)
  #:use-module (netlist schematic-port)
  #:use-module (netlist subschematic)
  #:use-module (symbol check net-attrib)

  #:export (hierarchy-post-process))

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


(define (make-new-pin-net object)
  (make-pin-net
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


(define (assign-net-netname! net tag)
  ;; The object is a net.  For nets we check the "netname="
  ;; attribute.
  (set-pin-net-name!
   net
   (create-net-name (attrib-value-by-name (pin-net-object net) "netname")
                    tag
                    ;; The below means just #f.
                    (not 'power-rail))))


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


(define (assign-pin-properties! pin tag)
  (let* ((object (pin-net-object pin))
         (refdes-pinnumber-pair (pin-refdes-pinnumber-pair object))
         (pinnumber (cdr refdes-pinnumber-pair)))
    ;; The object is a pin, and it defines net name using
    ;; "net=".  Use hierarchy tag here to make this netname
    ;; unique.
    (set-pin-net-name!
     pin
     (create-net-name (netattrib-search-net (object-component object)
                                            pinnumber)
                      tag
                      'power-rail))))


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


(define (set-net-map-package-pin-nets-properties! pin)
  (let* ((parent-component (package-pin-parent pin))
         (tag (subschematic-name (schematic-component-parent parent-component)))
         (netname (create-net-name (net-map-netname (package-pin-net-map pin))
                                   tag
                                   'power-rail))
         (nets (list (make-pin-net (package-pin-object pin)
                                   netname))))
    (set-package-pin-nets! pin nets)))


(define (set-package-pin-nets-properties! component)
  (define (real-pin? pin)
    (package-pin-object pin))

  (define (set-nets-properties! pin)
    (if (real-pin? pin)
        (set-real-package-pin-nets-properties! pin)
        (set-net-map-package-pin-nets-properties! pin)))

  (for-each set-nets-properties! (schematic-component-pins component)))


(define (search-net-name nets)
  (define (simple-add-rename from to)
    (add-rename from to)
    ;; Return new name.
    to)

  (define (get-new-netname net prev-name)
    (let ((current-name (pin-net-name net)))
      (if (and prev-name current-name)
          ;; Both names defined.
          (if (and (net-attrib-pin? (pin-net-object net))
                   (gnetlist-config-ref 'netname-attribute-priority))
              ;; If netname= has priority over net=, but the pin
              ;; is a power (net= driven) pin, use netname=,
              ;; anyways.
              (simple-add-rename current-name prev-name)
              ;; Otherwise, do the rename anyways (this might cause problems).
              (simple-add-rename prev-name current-name))
          ;; One or both undefined: return either defined or #f.
          (or prev-name current-name))))

  (fold get-new-netname #f nets))


;;; Checks if OBJECT is a pin that should be treated as one
;;; defining a name of the net connected to it via the "net="
;;; attribute of its parent component object.  Such components
;;; (e.g. "gnd-1.sym") have to have no refdes, and their "net="
;;; components should correspond to pinnumbers of their existing
;;; pins.
(define (net-attrib-pin? object)
  (and (net-pin? object)
       (let ((refdes (attrib-value-by-name (object-component object)
                                           "refdes"))
             (pinnumber (attrib-value-by-name object "pinnumber")))
         (and pinnumber (not refdes)))))


;;; This function does renaming job for PIN.
(define (net-map-update-pin component pin)
  (define tag (subschematic-name (schematic-component-parent component)))

  (define (check-shorted-nets a b priority)
    (unless (string=? a b)
    (log! 'critical
          (_ "Rename shorted nets (~A= has priority): ~A -> ~A")
          priority
          a
          b)
    )
    (add-net-rename a b))

  (define (unnamed-net-or-unconnected-pin? name)
    (or (string-prefix? "unnamed_net" name)
        (string-prefix? "unconnected_pin" name)))

  (define (update-pin-netname pin netname)
    (let ((nets (package-pin-nets pin))
          (object #f))
      (set-package-pin-name! pin netname)
      (if (null? nets)
          (set-package-pin-nets! pin
                                 (list (make-pin-net object
                                                     netname)))
          (let ((net (car nets)))
            (set-pin-net-name! net netname)))))

  (let ((net-map (package-pin-net-map pin)))
    (and (not (or (schematic-component-port component)
                  (schematic-component-subschematic component)))
         net-map
         (package-pin-object pin)
         (let ((netname (create-net-name (net-map-netname net-map)
                                         tag
                                         'power-rail))
               (pin-netname (package-pin-name pin)))
           (if (and pin-netname
                    (not (unnamed-net-or-unconnected-pin? pin-netname)))
               (if (gnetlist-config-ref 'netname-attribute-priority)
                   (check-shorted-nets netname pin-netname 'netname)
                   (check-shorted-nets pin-netname netname 'net))
               (begin
                 (when (unnamed-net-or-unconnected-pin? pin-netname)
                   ;; Rename unconnected pins and unnamed nets.
                   (add-net-rename pin-netname netname))
                 (update-pin-netname pin netname)))))))


(define (update-component-net-mapped-pins component)
  (for-each (cut net-map-update-pin component <>)
            (schematic-component-pins component)))


(define (update-component-pins schematic-component)
  (define (update-package-pin-name pin)
    (set-package-pin-name! pin
                           (schematic-connection-override-name (package-pin-port-connection pin))))

  (for-each update-package-pin-name
            (schematic-component-pins schematic-component)))

(define %unnamed-net-counter 0)
(define (increment-unnamed-net-counter)
  (set! %unnamed-net-counter (1+ %unnamed-net-counter))
  %unnamed-net-counter)


(define (create-unnamed-netname tag)
  (define (hierarchical-default-name s)
    (create-net-name (string-append (gnetlist-config-ref 'default-net-name) s)
                     tag
                     ;; The below means just #f.
                     (not 'power-rail)))
  ((if (eq? (netlist-mode) 'spice) identity hierarchical-default-name)
   (number->string (increment-unnamed-net-counter))))


(define %unnamed-pin-counter 0)
(define (increment-unnamed-pin-counter)
  (set! %unnamed-pin-counter (1+ %unnamed-pin-counter))
  %unnamed-pin-counter)


(define (create-unconnected-netname)
  (string-append "unconnected_pin-"
                 (number->string (increment-unnamed-pin-counter))))


(define (make-special-netname objects hierarchy-tag)
  (if (and (= (length objects) 1)
           (net-pin? (car objects)))
      (create-unconnected-netname)
      (create-unnamed-netname hierarchy-tag)))


(define (nets-netname connection)
  (define (unnamed? name)
    (eq? 'unnamed (car name)))

  (define (net? name)
    (eq? 'net (car name)))

  (let ((name (make-hierarchical-connection-name connection)))
    (set-schematic-connection-override-name!
     connection
     (if (unnamed? name)
         (make-special-netname (schematic-connection-objects connection)
                               (cdr name))
         (create-net-name (cadr name) (cddr name) (net? name))))))


(define (compat-refdes schematic-component)
  (set-schematic-component-refdes! schematic-component
                                   (hierarchical-refdes->string
                                    (schematic-component-refdes schematic-component)))
  schematic-component)


(define (hierarchy-post-process components connections)
  (define subcircuit-components
    (filter schematic-component-subcircuit? components))

  (define (add-port-rename port)
    ;; Rename inner nets using outer net name and disable inner
    ;; port component refdes.
    (add-rename
     ;; Netname of nets connected to inner port pin.
     (package-pin-name (schematic-port-inner-pin port))
     ;; Get source net name, all outer nets are named
     ;; already.
     (search-net-name (package-pin-nets (schematic-port-outer-pin port))))
    port)

  (define (component-subcircuit-ports component)
    (map add-port-rename (schematic-component-ports component)))

  (define (disable-component-refdes component)
    (set-schematic-component-refdes! component #f))

  (define (fix-composite-component component)
    ;; Disable refdeses of all inner port components.
    (for-each disable-component-refdes
              (cons component
                    (map schematic-port-inner-component
                         (component-subcircuit-ports component)))))

  (define (net-map-pin? pin)
    (package-pin-net-map pin))

  (define (set-net-map-pin-name! pin)
    (and (net-map-pin? pin)
         (set-package-pin-name!
          pin
          (create-net-name (net-map-netname (package-pin-net-map pin))
                           (subschematic-name
                            (schematic-component-parent (package-pin-parent pin)))
                           'power-rail))))

  (for-each nets-netname connections)

  (for-each set-net-map-pin-name!
            (append-map schematic-component-pins components))

  (for-each set-package-pin-nets-properties! components)

  (for-each update-component-pins components)

  (for-each fix-composite-component subcircuit-components)

  (for-each update-component-net-mapped-pins components)

  ;; (rename-all components)

  (map compat-refdes components))
