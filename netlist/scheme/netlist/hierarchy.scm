;;; Lepton EDA netlister
;;; Copyright (C) 2017-2018 Lepton EDA Contributors
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
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (geda log)
  #:use-module (geda object)
  #:use-module (netlist config)
  #:use-module (netlist core gettext)
  #:use-module (netlist mode)
  #:use-module (netlist net)
  #:use-module (netlist rename)
  #:use-module (netlist schematic-component)
  #:use-module (netlist schematic-connection)
  #:use-module (netlist schematic-port)
  #:use-module (netlist package-pin)
  #:use-module (netlist pin-net)
  #:use-module (netlist verbose)
  #:use-module (symbol check net-attrib)

  #:export (hierarchy-create-refdes
            hierarchy-post-process
            net-attrib-pin?))

(define (hierarchy-create-refdes basename hierarchy-tag)
  (match hierarchy-tag
    ((? list? tag) `(,basename . ,tag))
    (non-list (error (_ "Invalid hierarchy tag.") hierarchy-tag))))

(define (hierarchy-disable-refdes netlist disabled-refdes)
  (define (disabled? refdes)
    (equal? refdes disabled-refdes))

  (define (disable-net-connected-to net)
    (when (and=> (pin-net-connection-package net) disabled?)
      (set-pin-net-connection-package! net #f)))

  (define (disable-nets-connected-to pin)
    (for-each disable-net-connected-to (package-pin-nets pin)))

  (define (disable-package-refdes package)
    (when (and=> (schematic-component-refdes package) disabled?)
      (set-schematic-component-refdes! package #f))
    (for-each disable-nets-connected-to (schematic-component-pins package)))

  (for-each disable-package-refdes netlist)
  ;; Return the modified netlist.
  netlist)


(define (hierarchy-make-schematic-port components outer-port-pin)
  (define parent-component-refdes
    (schematic-component-refdes (package-pin-parent outer-port-pin)))
  (define port-refdes (package-pin-label outer-port-pin))
  (define pinnumber (package-pin-number outer-port-pin))
  ;; Define hierarchical refdes for a source schematic port
  ;; corresponding to the given pin.
  (define hierarchy-refdes (hierarchy-create-refdes port-refdes
                                                    parent-component-refdes))

  (define (warn-no-port)
    (log! 'critical
          (_ "Source schematic of the component ~S has no port with \"refdes=~A\".")
          parent-component-refdes
          port-refdes)
    #f)

  (define (warn-no-pinlabel)
    (log! 'critical
          (_ "Pin ~S of the component ~S has no \"pinlabel\" attribute.")
          pinnumber
          parent-component-refdes)
    #f)

  (define (get-matching-inner-port-pin port-component)
    (and (equal? (schematic-component-refdes port-component)
                 hierarchy-refdes)
         (not (null? (schematic-component-pins port-component)))
         ;; Return the inner port pin found.
         ;; Well, we assume a port has only one pin.
         (car (schematic-component-pins port-component))))

  (if port-refdes
      ;; Not empty filtered list means that we have found the
      ;; matching inner pin.
      (let ((pins (filter-map get-matching-inner-port-pin
                              components)))
        (if (null? pins)
            ;; Warn if no port found in the subcircuit. Return #f.
            (warn-no-port)
            ;; Otherwise, rename inner nets using outer net name and
            ;; disable inner port component refdes.
            ;; FIXME: It's assumed that only one pair of
            ;; matching pins found.
            (let* ((inner-port-pin (car pins))
                   (port (make-schematic-port inner-port-pin outer-port-pin)))
              ;; Update schematic component representing the port.
              (set-schematic-component-port! (package-pin-parent inner-port-pin)
                                             port)
              ;; Return new <schematic-port> created.
              port)))
      ;; Warn if no pinlabel found on the outer pin. Return #f.
      (warn-no-pinlabel)))


(define (search-net-name nets)
  (define (simple-add-rename from to)
    (add-rename from to)
    ;; Return new name.
    to)

  (define (get-new-netname net prev-name)
    (let ((current-name (pin-net-name net)))
      (if (and prev-name current-name)
          ;; Both names defined.
          (if (pin-net-priority net)
              (if (gnetlist-config-ref 'netname-attribute-priority)
                  ;; netname= has priority over net=.
                  ;; Rename the current net to the previously
                  ;; found name (label= name) and return the
                  ;; latter.
                  (simple-add-rename current-name prev-name)
                  ;; net= has priority over netname=.
                  ;; Since the net has net= priority set, use
                  ;; its name instead of the name found
                  ;; previously.
                  (simple-add-rename prev-name current-name))
              ;; Do the rename anyways (this might cause problems).
              ;; Rename net which has the same label=.
              (simple-add-rename prev-name current-name))
          ;; One or both undefined: return either defined or #f.
          (or prev-name current-name))))

  (fold get-new-netname #f nets))


(define (remove-refdes-mangling netlist)
  (define (base-refdes refdes)
    (match refdes
      ((? list? refdes) (car refdes))
      (refdes refdes)))

  (define (fix-net-connections net)
    (set-pin-net-connection-package! net
                                     (base-refdes (pin-net-connection-package net))))

  (define (fix-pin-connections pin)
    (for-each fix-net-connections (package-pin-nets pin)))

  (define (fix-package package)
    (set-schematic-component-refdes! package
                         (base-refdes (schematic-component-refdes package)))
    (for-each fix-pin-connections (schematic-component-pins package))
    package)

  (for-each fix-package netlist)
  netlist)


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
(define (net-map-update-pin pin id refdes tag)
  (define (add-net-power-pin-override pin net-map tag)
    (define (power-pin? pin)
      (string=? "pwr" (assq-ref (package-pin-attribs pin) 'pintype)))

    (let ((connection (package-pin-connection pin))
          (name (create-net-name (net-map-netname net-map)
                                 tag
                                 'power-rail)))
      (when (power-pin? pin)
        (set-schematic-connection-override-name!
         connection
         (match (schematic-connection-override-name connection)
           ((? list? x) `(,name . ,x))
           (#f name)
           (x `(,name ,x)))))))

  (define (check-shorted-nets a b priority)
    (log! 'critical
          (_ "Rename shorted nets (~A= has priority): ~A -> ~A")
          priority
          a
          b)
    (add-net-rename a b))

  (define (unnamed-net-or-unconnected-pin? name)
    (or (string-prefix? "unnamed_net" name)
        (string-prefix? "unconnected_pin" name)))

  (define (update-pin-netname pin netname id refdes)
    (let ((nets (package-pin-nets pin))
          (pinnumber (package-pin-number pin))
          (net-priority (net-attrib-pin? (package-pin-object pin)))
          (object #f))
      (set-package-pin-name! pin netname)
      (if (null? nets)
          (set-package-pin-nets! pin
                                 (list (make-pin-net id
                                                     object
                                                     net-priority
                                                     netname
                                                     refdes
                                                     pinnumber)))
          (let ((net (car nets)))
            (set-pin-net-id! net id)
            (set-pin-net-priority! net net-priority)
            (set-pin-net-name! net netname)
            (set-pin-net-connection-package! net refdes)
            (set-pin-net-connection-pinnumber! net pinnumber)))))

  (let ((net-map (package-pin-net-map pin)))
    (add-net-power-pin-override pin net-map tag)
    (and refdes
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
                 (update-pin-netname pin netname id refdes)))))))


(define (update-component-net-mapped-pins component)
  (define pins (schematic-component-pins component))
  (define id (schematic-component-id component))
  (define refdes (schematic-component-refdes component))
  (define tag (schematic-component-tag component))

  (define (update-pin pin)
    (and (package-pin-object pin)
         (package-pin-net-map pin)
         (net-map-update-pin pin id refdes tag)))

  (for-each update-pin pins))


(define (update-component-pins schematic-component)
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

(define %netnames (make-hash-table))

(define (search-in-hash-table nets)
    (and (not (null? nets))
         (or (hash-ref %netnames (pin-net-id (car nets)))
             (search-in-hash-table (cdr nets)))))

(define (make-special-netname nets hierarchy-tag)
  (if (null? nets)
      (create-unconnected-netname)
      (create-unnamed-netname hierarchy-tag)))


(define (nets-netname nets hierarchy-tag)
  (or
   ;; If there is no netname, probably some of nets has been
   ;; already named.
   (search-net-name nets)
   ;; Didn't find a name.  Go looking for another net which
   ;; might have already been named, i.e. we don't want to
   ;; create a new unnamed net if the net has already been named
   ;; before.
   (search-in-hash-table nets)
   ;; Last resort. We have not found a name. Make a new one.
   (make-special-netname nets hierarchy-tag)))

(define (update-netnames-hash-table netname nets)
  (and netname
       (for-each
        (lambda (net) (hash-set! %netnames (pin-net-id net) netname))
        nets)))

(define (update-package-pin-name pin)
  (let* ((nets (package-pin-nets pin))
         (component (package-pin-parent pin))
         (hierarchy-tag (schematic-component-tag component))
         (netname (nets-netname nets hierarchy-tag)))
    (set-package-pin-name! pin netname)
    (update-netnames-hash-table netname nets)))

(define (hierarchy-post-process components)
  (define (outer-pin->schematic-port outer-port-pin)
    (let ((port (hierarchy-make-schematic-port components outer-port-pin)))
      (and port
           ;; Net renaming stuff.
           (add-rename
            ;; Netname of nets connected to inner port pin.
            (package-pin-name (schematic-port-inner-pin port))
            ;; Get source net name, all outer nets are named
            ;; already.
            (search-net-name (package-pin-nets outer-port-pin)))
           port)))

  (define (component-subcircuit-ports component)
    (filter-map outer-pin->schematic-port
                (schematic-component-pins component)))

  (define (fix-composite-component component)
    ;; Disable refdeses of all inner port components.
    (for-each (cut hierarchy-disable-refdes components <>)
              (map schematic-component-refdes
                   (cons component
                         (map schematic-port-inner-component
                              (component-subcircuit-ports component))))))

  (for-each update-component-pins components)

  (for-each update-component-net-mapped-pins components)

  (for-each fix-composite-component
            (filter schematic-component-subcircuit? components))

  (rename-all
   ((if (gnetlist-config-ref 'mangle-refdes)
        identity
        remove-refdes-mangling)
    components)))
