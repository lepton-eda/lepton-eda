;;; gEDA - GPL Electronic Design Automation
;;; gnetlist - gEDA Netlist
;;; Copyright (C) 2007-2010 John P. Doty
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

;; Netlister for symbolic circuit analysis using Mathematica.
;; See the Mathematica notebook gEDA.nb (obtainable at www.noqsi.com)
;; for usage.


(define (netname-connections->pin-voltages netname)
  (define package car)
  (define pinnumber cadr)
  (define (netname-connection->voltage-string connection)
    (format #f "v[\"~A\",\"~A\"]=v[\"~A\"];\n"
            (package connection)
            (pinnumber connection)
            netname))
  (string-join (map netname-connection->voltage-string
                    (gnetlist:get-all-connections netname))
               ""))


(define (connections->node-currents connections)
  (define package car)
  (define pinnumber cadr)
  (define (connection->node-current-string connection)
    (format #f "i[\"~A\",\"~A\"]" (package connection) (pinnumber connection)))
  (string-join (map connection->node-current-string connections) "+"))


(define (netnames->current-string netnames)
  (define (netname->current-string netname)
    (and (not (string=? netname "GND"))
         (format #f "~A==0"
                 (connections->node-currents (gnetlist:get-all-connections netname)))))
  (string-join (filter-map netname->current-string netnames) ",\n"))

(define (mathematica:write-device-value device value refdes)
  (format #t "~A[value->~A][\"~A\"]"
          (string-downcase device)
          value
          refdes))

(define (mathematica:write-device-model model refdes)
   (format #t "~A[\"~A\"]" model refdes))


(define (mathematica:write-model refdes)
   (let ((device (gnetlist:get-package-attribute refdes "device"))
         (value (gnetlist:get-package-attribute refdes "value"))
         (model (gnetlist:get-package-attribute refdes "model")))
      (if (equal? model "unknown")
         (if (equal? value "unknown")
            (mathematica:write-device-value device (string-downcase refdes)
               refdes)
            (mathematica:write-device-value device value refdes)
         )
         (mathematica:write-device-model model refdes)
      )
   )
)

(define (mathematica:write-models refdeses first)
   (if (not (null? refdeses))
      (let ((refdes (car refdeses)))
         (if (not first)
             (display ",\n"))
         (mathematica:write-model refdes)
         (mathematica:write-models (cdr refdeses) #f)
      )
   )
)

(define (mathematica:list-voltages netnames first)
   (if (not (null? netnames))
      (let ((netname (car netnames)))
         (if (not (equal? netname "GND"))
            (begin
              (if (not first)
                  (display ",\n"))
                (format #t "v[\"~A\"]" netname)
                (mathematica:list-voltages (cdr netnames) #f)
             )
            (mathematica:list-voltages (cdr netnames) first)
         )
      )
   )
)


(define (mathematica:list-pin-currents pins)
   (if (not (null? pins))
      (let ((pin (car pins)))
        (display ",\n")
        (format #t "i[\"~A\",\"~A\"]" (car pin) (cadr pin))
         (mathematica:list-pin-currents (cdr pins))
      )
   )
)


(define (mathematica:list-currents netnames)
   (if (not (null? netnames))
      (let ((netname (car netnames)))
         (mathematica:list-pin-currents
            (gnetlist:get-all-connections netname))
         (mathematica:list-currents (cdr netnames))
      )
   )
)


(define (mathematica output-filename)
  (with-output-to-port (gnetlist:output-port output-filename)
    (lambda ()
      (let ((nets (gnetlist:get-all-unique-nets "dummy")))
        (for-each display
                  (map netname-connections->pin-voltages nets))
        (display "nodeEquations={\n")
        (display (netnames->current-string nets))
        (display "};\n")
        (display "modelEquations={\n")
        (mathematica:write-models packages #t)
        (display "};\n")
        (display "variables={\n")
        (mathematica:list-voltages nets #t)
        (mathematica:list-currents nets)
        (display "};\n")))))
