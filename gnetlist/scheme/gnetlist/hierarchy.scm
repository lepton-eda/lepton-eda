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
  #:use-module (gnetlist config)
  #:use-module (gnetlist package)
  #:use-module (gnetlist package-pin)
  #:use-module (gnetlist pin-net)

  #:export (hierarchy-create-refdes
            hierarchy-remove-all-composite))

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
