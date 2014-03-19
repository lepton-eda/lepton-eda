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

(define (mathematica:quoted thing)
   (write-char #\")
   (display thing)
   (write-char #\")
)

(define (mathematica:write-pin-voltages netname pins)
   (if (not (null? pins))
      (let ((pin (car pins)))
         (display "v[")
         (mathematica:quoted (car pin))
         (display ",")
         (mathematica:quoted (car (cdr pin)))
         (display "]=v[")
         (mathematica:quoted netname)
         (display "];")
         (newline)
         (mathematica:write-pin-voltages netname (cdr pins))
      )
   )
)


(define (mathematica:write-voltages netnames)
  (if (not (null? netnames))
      (let ((netname (car netnames)))
         (mathematica:write-pin-voltages netname
            (gnetlist:get-all-connections netname))
         (mathematica:write-voltages (cdr netnames)))))


(define (mathematica:write-node-currents pins)
   (let ((pin (car pins)))
      (display "i[")
      (mathematica:quoted (car pin))
      (display ",")
      (mathematica:quoted (car (cdr pin)))
      (display "]")
      (if (not (null? (cdr pins )))
         (begin
            (display "+")
            (mathematica:write-node-currents (cdr pins))
         )
      )
   )
)

(define (mathematica:,newline)
   (display ",")
   (newline)
)


(define (mathematica:write-currents netnames first)
   (if (not (null? netnames))
      (let ((netname (car netnames)))
         (if (not (equal? netname "GND"))
            (begin
               (if (not first)
                  (mathematica:,newline)
                )
               (mathematica:write-node-currents
                  (gnetlist:get-all-connections netname))
               (display "==0")
               (mathematica:write-currents (cdr netnames) #f)
             )
            (mathematica:write-currents (cdr netnames) first)
         )
      )
   )
)

(define (mathematica:write-device-value device value refdes)
   (display (string-downcase device))
   (display "[value->")
   (display value)
   (display "][")
   (mathematica:quoted refdes)
   (display "]")
)

(define (mathematica:write-device-model model refdes)
   (display model)
   (display "[")
   (mathematica:quoted refdes)
   (display "]")
 )


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
            (mathematica:,newline)
         )
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
                  (mathematica:,newline)
                )
                (display "v[")
                (mathematica:quoted netname)
                (display "]")
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
         (mathematica:,newline)
         (display "i[")
         (mathematica:quoted (car pin))
         (display ",")
         (mathematica:quoted (car (cdr pin)))
         (display "]")
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
  (set-current-output-port (gnetlist:output-port output-filename))
  (let ((nets (gnetlist:get-all-unique-nets "dummy")))
     (mathematica:write-voltages nets)
     (display "nodeEquations={")
     (newline)
     (mathematica:write-currents nets #t)
     (display "};")
     (newline)
     (display "modelEquations={")
     (newline)
     (mathematica:write-models packages #t)
     (display "};")
     (newline)
     (display "variables={")
     (newline)
     (mathematica:list-voltages nets #t)
     (mathematica:list-currents nets)
     (display "};")
     (newline)
   )
  (close-output-port (current-output-port))
)
