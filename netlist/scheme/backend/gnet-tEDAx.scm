;;; Lepton EDA netlister
;;; tEDAx plug-in for lepton-netlist
;;; Copyright (C) 2018 Bdale Garbee
;;; Copyright (C) 2018-2020 Lepton EDA Contributors
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

;; --------------------------------------------------------------------------
;; The tEDAx format is documented at http://repo.hu/projects/tedax/
;; --------------------------------------------------------------------------

(use-modules (srfi srfi-1)
             (lepton attrib)
             (lepton object)
             (netlist schematic)
             (netlist schematic toplevel)
             (netlist schematic-connection))

;;; Escape spaces and tabs in string S.
(define (escape-whitespaces s)
  (define (escape-spaces s)
    (string-join (string-split s #\space) "\\ "))

  (define (escape-tabs s)
    (string-join (string-split s #\tab) "\\\t"))

  (escape-tabs (escape-spaces s)))

;;
;; return device attribute
;;
(define (tEDAx:get-device package)
  (gnetlist:get-package-attribute package "device"))

;;
;; return footprint attribute (UNKNOWN if not defined)
;;
(define (tEDAx:get-pattern package)
  (let ((pattern (gnetlist:get-package-attribute package "footprint")))
    (if (unknown? pattern) "UNKNOWN" pattern)))

;;
;; returns value attribute (empty if not defined)
;;
(define (tEDAx:get-value package)
  (let ((value (gnetlist:get-package-attribute package "value")))
    (if (unknown? value) "" value)))

;;
;; emit header
;;
(define (tEDAx:header)
  (format #t "tEDAx v1\nbegin netlist v1 netlist\n\n"))

;;
;; emit trailer
;;
(define (tEDAx:trailer)
  (format #t "end netlist\n"))

;;
;; emit component related lines
;;
(define (tEDAx:components ls)
  (for-each
   (lambda (package)
     (let ((package-name (escape-whitespaces package))
           (pattern (escape-whitespaces (tEDAx:get-pattern package)))
           (device (escape-whitespaces (tEDAx:get-device package)))
           (value (escape-whitespaces (tEDAx:get-value package))))
       (format #t "\tfootprint ~A ~A\n\tdevice ~A ~A\n\tvalue ~A ~A\n\n"
               package-name
               pattern
               package-name
               device
               package-name
               value)))
   ls))

;;
;; emit network related lines for current net
;;
(define (tEDAx:display-connections netname nets)
  (define package car)
  (define pinnumber cdr)
  (string-join
   (map
    (lambda (net)
      (format #f "\tconn ~A ~A ~A\n"
	(escape-whitespaces netname)
        (escape-whitespaces (package net))
        (pinnumber net)))
    nets)
   ""))

;;
;; iterate over all nets
;;
(define (tEDAx:connections netnames)
  (for-each
   (lambda (netname)
     (format #t "~A\n"
       (tEDAx:display-connections netname (get-all-connections netname))))
   netnames))

;;; Output all net attributes for all CONNECTIONS in the format
;;; "nettag connection-name attrib-name attrib-value".
;;; Attributes "netname=" are filtered out from the list.
(define (tEDAx:netattributes connections)
  (define (output-list ls)
    (unless (null? ls)
      (apply format #t "\tnettag ~A ~A ~A\n"
             (map escape-whitespaces ls))))

  (define connection-name schematic-connection-override-name)

  (define (conn-attrib->name-ls conn attrib)
    (if (string= (attrib-name attrib) "netname")
        ;; If attrib is "netname=", skip it.
        '()
        ;; Otherwise, create the list in the form:
        ;; '(connection-name attrib-name attrib-value)
        (list (connection-name conn)
              (attrib-name attrib)
              (attrib-value attrib))))

  (define (connection-nets conn)
    (filter net? (schematic-connection-objects conn)))

  (define (connection-attribs conn)
    (append-map object-attribs (connection-nets conn)))

  (define (connection->name-attrib-lists conn)
    (map (lambda (attrib) (conn-attrib->name-ls conn attrib))
         (connection-attribs conn)))

  (define list-of-conn-attrib-lists
    (append-map connection->name-attrib-lists connections))

  (for-each output-list list-of-conn-attrib-lists))

;;;
;;; emit netlist in tEDAx interchange format
;;;
(define (tEDAx output-filename)
  (let ((nets (schematic-nets (toplevel-schematic)))
        (packages (schematic-package-names (toplevel-schematic)))
        (connections (schematic-connections (toplevel-schematic))))
    (tEDAx:header)
    (tEDAx:components packages)
    (tEDAx:netattributes connections)
    (tEDAx:connections nets)
    (tEDAx:trailer)))

;; --------------------------------------------------------------------------
