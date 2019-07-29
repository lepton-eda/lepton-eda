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
  #:use-module (netlist config)
  #:use-module (netlist core gettext)
  #:use-module (netlist net)
  #:use-module (netlist rename)
  #:use-module (netlist schematic-component)
  #:use-module (netlist schematic-connection)
  #:use-module (netlist package-pin)
  #:use-module (netlist pin-net)
  #:use-module (netlist verbose)
  #:use-module (symbol check net-attrib)

  #:export (hierarchy-create-refdes
            hierarchy-post-process
            search-net-name))

(define (hierarchy-create-refdes basename hierarchy-tag)
  (match hierarchy-tag
    (#f basename)
    ((? list? tag) `(,basename . ,tag))
    (tag `(,basename ,tag))))

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


(define (hierarchy-remove-all-composite netlist)
  (fold
   (lambda (package prev-netlist)
     (if (and (schematic-component-sources package)
              (schematic-component-refdes package))
         (hierarchy-disable-refdes prev-netlist (schematic-component-refdes package))
         prev-netlist))
   netlist
   netlist))


(define (hierarchy-setup-rename netlist refdes label nets)
  (define (rename-and-remove-connection package hierarchy-refdes)
    (and (schematic-component-refdes package)
         (equal? (schematic-component-refdes package)
                   hierarchy-refdes)
         (not (null? (schematic-component-pins package)))
         ;; Well, we assume a port has only one pin.
         (let ((pin (car (schematic-component-pins package))))
           ;; Skip overhead of special I/O symbol.
           (add-rename (package-pin-name pin)
                       ;; Get source net name, all nets are named already.
                       (search-net-name nets))
           (hierarchy-disable-refdes netlist (schematic-component-refdes package))
           ;; Return package with no refdes.
           package)))
  ;; Search for hierarchical refdes created from LABEL and REFDES
  (let ((hierarchy-refdes (hierarchy-create-refdes label refdes)))
    ;; Not empty filtered list means that we have found and disabled it.
    (not (null? (filter-map (cut rename-and-remove-connection
                                 <>
                                 hierarchy-refdes)
                            netlist)))))


(define (search-net-name nets)
  (define (log/add-rename from to)
    (unless (search-rename from to #t)
      (log! 'critical
            (_ "Found duplicate net name, renaming ~A to ~A")
            from
            to))
    (add-rename from to)
    ;; Return new name.
    to)

  (define (simple-add-rename from to)
    (add-rename from to)
    ;; Return new name.
    to)

  (define (get-new-netname net prev-name)
    (let ((current-name (pin-net-name net))
          (has-priority? (pin-net-priority net)))
      (if (and prev-name current-name)
          ;; Both names defined.
          (if (string=? prev-name current-name)
              ;; No doubts I know which one to return ;-)
              prev-name
              ;; Otherwise the decision depends on config priority
              ;; settings.
              (if has-priority?
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
                  (log/add-rename prev-name current-name)))
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


;;; This function does renaming job for PIN.
(define (net-map-update-pin pin id refdes tag)
  (define (add-net-power-pin-override pin net-map tag)
    (define (power-pin? pin)
      (string=? "pwr" (assq-ref (package-pin-attribs pin) 'pintype)))

    (let ((connection (package-pin-connection pin))
          (name (create-netattrib (net-map-netname net-map) tag)))
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
          (net-priority #t)
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
         (let ((netname (create-netattrib (net-map-netname net-map) tag))
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


(define (hierarchy-post-process components)
  (define (fix-pin pin refdes)
    (let ((label (package-pin-label pin))
          (pinnumber (package-pin-number pin))
          (nets (package-pin-nets pin)))
     (if label
         (unless (hierarchy-setup-rename components refdes label nets)
           (log! 'critical
                 (_ "Source schematic of the component ~S has no port with \"refdes=~A\".")
                 refdes
                 label))
         (log! 'critical
               (_ "Pin ~S of the component ~S has no \"pinlabel\" attribute.")
               pinnumber
               refdes))))

  (define (fix-composite-package package)
    (when (schematic-component-sources package)
      (for-each (cut fix-pin <> (schematic-component-refdes package))
                (schematic-component-pins package))))

  (for-each update-component-net-mapped-pins components)

  (for-each fix-composite-package components)

  ((if (gnetlist-config-ref 'mangle-refdes)
       identity
       remove-refdes-mangling)
   (hierarchy-remove-all-composite components)))
