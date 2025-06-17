;;; Lepton EDA netlister
;;; Copyright (C) 2017-2021 Lepton EDA Contributors
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

(define-module (netlist schematic-connection)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (lepton attrib)
  #:use-module (lepton object)
  #:use-module (lepton page)
  #:use-module (netlist package-pin)

  #:export-syntax (make-schematic-connection schematic-connection?
                   schematic-connection-id set-schematic-connection-id!
                   schematic-connection-parent set-schematic-connection-parent!
                   schematic-connection-page set-schematic-connection-page!
                   schematic-connection-name set-schematic-connection-name!
                   schematic-connection-override-name set-schematic-connection-override-name!
                   schematic-connection-objects set-schematic-connection-objects!
                   schematic-connection-pins set-schematic-connection-pins!)

  #:export (make-page-schematic-connections
            schematic-connection-add-pin!
            set-schematic-connection-printer!))


(define-record-type <schematic-connection>
  (make-schematic-connection id parent page name override-name objects pins)
  schematic-connection?
  ;; ID. Dunno, why it is here...
  (id schematic-connection-id set-schematic-connection-id!)
  ;; Parent subschematic of the connection.
  (parent schematic-connection-parent set-schematic-connection-parent!)
  ;; Parent page for direct connections.  Has no sense for named
  ;; or hierarchical connections.
  (page schematic-connection-page set-schematic-connection-page!)
  ;; Net name of the connection taken from the "netname="
  ;; attributes of the connection objects.
  (name schematic-connection-name set-schematic-connection-name!)
  ;; Net name of the connection taken from the "net=" attributes
  ;; of the connection objects.
  (override-name schematic-connection-override-name set-schematic-connection-override-name!)
  ;; Objects of the connection.  They can be net or pin primitives.
  (objects schematic-connection-objects set-schematic-connection-objects!)
  ;; Pins of the connections. Now they are <package-pin> objects.
  (pins schematic-connection-pins set-schematic-connection-pins!))


;;; Sets default printer for <schematic-connection>
(set-record-type-printer!
 <schematic-connection>
 (lambda (record port) (format port "#<schematic-connection ~A>" (schematic-connection-id record))))

(define (set-schematic-connection-printer! format-string . args)
  "Adjust pretty-printing of <schematic-connection> records.
FORMAT-STRING must be in the form required by the procedure
`format'. The following ARGS may be used:
  'id
  'parent
  'page
  'name
  'override-name
  'objects
  'pins
Any other unrecognized argument will lead to yielding '?' in the
corresponding place.
Example usage:
  (set-schematic-connection-printer! \"<schematic-connection-~A (~A)>\" 'id 'name)"
  (set-record-type-printer!
   <schematic-connection>
   (lambda (record port)
     (apply format port format-string
            (map
             (lambda (arg)
               (match arg
                 ('id (schematic-connection-id record))
                 ('parent (schematic-connection-parent record))
                 ('page (schematic-connection-page record))
                 ('name (schematic-connection-name record))
                 ('override-name (schematic-connection-override-name record))
                 ('objects (schematic-connection-objects record))
                 ('pins (schematic-connection-pins record))
                 (_ #\?)))
             args)))))

;;; This is a helper object property to get object connections
;;; once.
(define object-conns (make-object-property))

(define (connected-to? object1 object2)
  (not (not (memv object1 (object-conns object2)))))

(define (connected-to-ls? object1 ls)
  (and (not (null? ls))
       (or (connected-to? object1 (car ls))
           (connected-to-ls? object1 (cdr ls)))))

(define (reconnect-groups object groups)
  (define (object-connected-to? group)
    (connected-to-ls? object group))

  (receive (connected unconnected)
      (partition object-connected-to? groups)
    (let ((result
           `(,@unconnected
             ,(if (null? connected)
                  (list object)
                  (apply append (list object) connected)))))
      result)))

;;; Transforms list of objects LS into the list of lists of
;;; interconnected objects.
(define (group-connections ls)
  (fold reconnect-groups '() ls))


(define (schematic-connection->netnames schematic-connection)
  (filter-map (lambda (attrib)
                (and (string=? (attrib-name attrib) "netname")
                     (attrib-value attrib)))
              (append-map object-attribs schematic-connection)))

(define (connections->netname-groups connections)
  (map (lambda (schematic-connection)
         (cons (schematic-connection->netnames schematic-connection) schematic-connection))
       connections))

(define (get-schematic-connection-netname netnames)
  (match netnames
    ((c) c)
    ((a b ...) netnames)))

(define (get-schematic-connection page schematic-connection-ls)
  (let* ((netnames (car schematic-connection-ls))
         (objects (cdr schematic-connection-ls))
         (id (object-id (car objects))))
    (make-schematic-connection id
                               ;; No parent subschematic yet.
                               #f
                               page
                               ;; netname
                               (if (null? netnames)
                                   '()
                                   (get-schematic-connection-netname netnames))
                               ;; override-netname
                               #f
                               objects
                               '())))

;;; Return the list of COMPONENT pins.
(define (component-pins component)
  (let loop ((ls (component-contents component))
             (pins '()))
    (if (null? ls)
        pins
        (let ((object (car ls))
              (rest (cdr ls)))
          (loop rest
                (if (net-pin? object)
                    (cons object pins)
                    pins))))))

(define (make-page-schematic-connections page)
  "Create <schematic-connection> records from PAGE primitives."

  (define (page-connectable-objects page)
    (let loop ((ls (page-contents page))
               (nets '())
               (components '()))
      (if (null? ls)
          (values nets
                  ;; pins
                  (append-map component-pins components))
          (let ((object (car ls)))
            (loop (cdr ls)
                  (if (net? object)
                      (cons object nets)
                      nets)
                  (if (component? object)
                      (cons object components)
                      components))))))

  (define (populate-object-connections object)
    (set! (object-conns object)
          (object-connections object)))

  (let-values (((nets pins) (page-connectable-objects page)))
    (for-each populate-object-connections nets)
    (for-each populate-object-connections pins)
    (map (cut get-schematic-connection page <>)
         (connections->netname-groups
          (group-connections (append nets pins))))))


(define (schematic-connection-add-pin! connection pin)
  "Add <package-pin> object PIN to <schematic-connection> object
CONNECTION."
  (set-schematic-connection-pins!
   connection
   (cons pin (schematic-connection-pins connection)))
  (set-package-pin-connection! pin connection)
  ;; Return value.
  connection)
