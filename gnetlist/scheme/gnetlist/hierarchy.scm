;;; Lepton EDA netlister
;;; Copyright (C) 2017 Lepton EDA Contributors
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

(define-module (gnetlist hierarchy)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (geda log)
  #:use-module (gnetlist config)
  #:use-module (gnetlist core gettext)
  #:use-module (gnetlist rename)
  #:use-module (gnetlist package)
  #:use-module (gnetlist package-pin)
  #:use-module (gnetlist pin-net)
  #:use-module (gnetlist verbose)

  #:export (hierarchy-create-refdes
            hierarchy-post-process
            search-net-name))

(define (hierarchy-create-refdes basename hierarchy-tag)
  (let ((reverse-refdes-order? (gnetlist-config-ref 'reverse-refdes-order))
        (refdes-separator (gnetlist-config-ref 'refdes-separator)))
    (if hierarchy-tag
        (and basename
             (if reverse-refdes-order?
                 (string-append basename refdes-separator hierarchy-tag)
                 (string-append hierarchy-tag refdes-separator basename)))
        basename)))


(define (hierarchy-disable-refdes netlist disabled-refdes)
  (define (disabled? refdes)
    (string=? refdes disabled-refdes))

  (define (disable-net-connected-to net)
    (when (and=> (pin-net-connection-package net) disabled?)
      (set-pin-net-connection-package! net #f)))

  (define (disable-nets-connected-to pin)
    (for-each disable-net-connected-to (package-pin-nets pin)))

  (define (disable-package-refdes package)
    (when (and=> (package-refdes package) disabled?)
      (set-package-refdes! package #f))
    (for-each disable-nets-connected-to (package-pins package)))

  (for-each disable-package-refdes netlist)
  ;; Return the modified netlist.
  netlist)


(define (hierarchy-remove-all-composite netlist)
  (fold
   (lambda (package prev-netlist)
     (if (and (package-composite? package)
              (package-refdes package))
         (hierarchy-disable-refdes prev-netlist (package-refdes package))
         prev-netlist))
   netlist
   netlist))


(define (hierarchy-setup-rename netlist refdes label nets)
  (define (rename-and-remove-connection package hierarchy-refdes)
    (and (package-refdes package)
         (string=? (package-refdes package)
                   hierarchy-refdes)
         (not (null? (package-pins package)))
         ;; Well, we assume a port has only one pin.
         (let ((pin (car (package-pins package))))
           ;; Skip overhead of special I/O symbol.
           (add-rename (package-pin-name pin)
                       ;; Get source net name, all nets are named already.
                       (search-net-name nets))
           (hierarchy-disable-refdes netlist (package-refdes package))
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
  ;; Separator is yet always '/'
  (define (base-refdes refdes legend)
    (and refdes
         (if (gnetlist-config-ref 'reverse-refdes-order)
             (let ((pos (string-index refdes #\/)))
               (if pos
                   (string-take refdes pos)
                   refdes))
             (let ((pos (string-rindex refdes #\/)))
               (if pos
                   (string-drop refdes (1+ pos))
                   refdes)))))

  (define (fix-net-connections net)
    (set-pin-net-connection-package! net
                                     (base-refdes (pin-net-connection-package net)
                                                  "U")))

  (define (fix-pin-connections pin)
    (for-each fix-net-connections (package-pin-nets pin)))

  (define (fix-package package)
    (set-package-refdes! package
                         (base-refdes (package-refdes package) "u"))
    (for-each fix-pin-connections (package-pins package))
    package)

  (for-each fix-package netlist)
  netlist)


(define (hierarchy-post-process netlist)
  (define (fix-pin pin refdes)
    (let ((label (package-pin-label pin))
          (pinnumber (package-pin-number pin))
          (nets (package-pin-nets pin)))
     (if label
         (unless (hierarchy-setup-rename netlist refdes label nets)
           (log! 'critical
                 (_ "Source schematic of the component ~S has no port with \"refdes=~A\".")
                 refdes
                 label))
         (log! 'critical
               (_ "Pin ~S of the component ~S has no \"pinlabel\" attribute.")
               pinnumber
               refdes))))

  (define (fix-composite-package package)
    (when (package-composite? package)
      (for-each (cut fix-pin <> (package-refdes package)) (package-pins package))))

  (for-each fix-composite-package netlist)

  (hierarchy-remove-all-composite
   ((if (gnetlist-config-ref 'mangle-refdes)
        identity
        remove-refdes-mangling) netlist)))
