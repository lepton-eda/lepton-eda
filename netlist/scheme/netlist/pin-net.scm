;;; Lepton EDA netlister
;;; Copyright (C) 2016-2017 gEDA Contributors
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

(define-module (netlist pin-net)
  ;; Import C procedures and variables.
  #:use-module (netlist core gettext)

  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (geda log)
  #:use-module (geda object)
  #:use-module (netlist net)

  #:export (make-pin-net pin-net?
            pin-net-id set-pin-net-id!
            pin-net-object set-pin-net-object!
            pin-net-name set-pin-net-name!
            set-pin-net-printer!
            assign-net-netname!
            assign-pin-properties!))

(define-record-type <pin-net>
  (make-pin-net id object name)
  pin-net?
  (id pin-net-id set-pin-net-id!)
  (object pin-net-object set-pin-net-object!)
  (name pin-net-name set-pin-net-name!))

;;; Sets default printer for <pin-net>
(set-record-type-printer!
 <pin-net>
 (lambda (record port) (format port "#<pin-net ~A>" (pin-net-id record))))

(define (set-pin-net-printer! format-string . args)
  "Adjust pretty-printing of <pin-net> records.
FORMAT-STRING must be in the form required by the procedure
`format'. The following ARGS may be used:
  'id
  'object
  'name
Any other unrecognized argument will lead to yielding '?' in the
corresponding place.
Example usage:
  (set-pin-net-printer! \"<pin-net-~A (~A)>\" 'id 'name)"
  (set-record-type-printer!
   <pin-net>
   (lambda (record port)
     (apply format port format-string
            (map
             (lambda (arg)
               (match arg
                 ('id (pin-net-id record))
                 ('object (pin-net-object record))
                 ('name (pin-net-name record))
                 (_ #\?)))
             args)))))




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
