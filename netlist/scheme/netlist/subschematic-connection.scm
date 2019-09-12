;;; Lepton EDA netlister
;;; Copyright (C) 2019 Lepton EDA Contributors
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

(define-module (netlist subschematic-connection)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (netlist package-pin)
  #:use-module (netlist subschematic)
  #:use-module (netlist schematic-component)
  #:use-module (netlist schematic-connection)

  #:export (make-subschematic-connections))

(define (connection-netnames conn)
  (define (any->ls x)
    (if (list? x) x (list x)))
  (filter identity (append (any->ls (schematic-connection-name conn))
                           (any->ls (schematic-connection-override-name conn)))))


(define (connected-netname? conn1 conn2)
  (define (is-member? x)
    (not (not (member x (connection-netnames conn1)))))
  (any is-member? (connection-netnames conn2)))


(define (connected? c ls)
  (any (lambda (x) (connected-netname? c x)) ls))


(define (connected-lists? ls1 ls2)
  (and (not (null? ls1))
       (or (connected? (car ls1) ls2)
           (connected-lists? (cdr ls1) ls2))))


(define (reconnect-connections ls groups)
  (define (is-ls-connected-to? group)
    (connected-lists? ls group))

  (receive (connected unconnected)
      (partition is-ls-connected-to? groups)
    (let ((result
           `(,@unconnected
             ,(if (null? connected)
                  ls
                  (apply append ls connected)))))
      result)))


(define (group-connections ls)
  (fold reconnect-connections '() (map list ls)))


(define (subschematic-pins subschematic)
  (append-map schematic-component-pins
              (subschematic-components subschematic)))


(define (subschematic-pin-connections subschematic)
  (delete-duplicates (map package-pin-connection
                          (subschematic-pins subschematic))))


(define (make-netname-connection name group)
  (define (any->ls x)
    (if (list? x) x (list x)))

  (define (merge-names ls)
    (delete-duplicates
     (filter identity
             (append-map any->ls
                         (map schematic-connection-name ls)))))

  (define (merge-override-names ls)
    (delete-duplicates
     (filter identity
             (append-map any->ls
                         (map schematic-connection-override-name ls)))))

  (define (merge-objects ls)
    (delete-duplicates (append-map schematic-connection-objects ls)))

  (define (merge-pins ls)
    (delete-duplicates (append-map schematic-connection-pins ls)))

  (let* ((id #f)
         (page #f)
         (netnames (merge-names group))
         (net-names (merge-override-names group))
         (objects (merge-objects group))
         (pins (merge-pins group))
         (connection (make-schematic-connection
                      id
                      ;; Parent subschematic.
                      #f
                      page
                      netnames
                      net-names
                      objects
                      pins)))
    (for-each
     (cut set-package-pin-named-connection! <> connection)
     pins)
    connection))


(define (make-subschematic-connections subschematic)
  (let* ((name (subschematic-name subschematic))
         (named-connections
          (map (cut make-netname-connection name <>)
               (group-connections (subschematic-pin-connections subschematic)))))
    named-connections))
