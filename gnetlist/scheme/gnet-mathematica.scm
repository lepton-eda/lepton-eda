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
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; Netlister for symbolic circuit analysis using Mathematica.
;; See the Mathematica notebook gEDA.nb (obtainable at www.noqsi.com)
;; for usage.

(define (mathematica:quoted thing port)
   (write-char #\" port)
   (display thing port)
   (write-char #\" port)
)

(define (mathematica:write-pin-voltages netname pins port)
   (if (not (null? pins))
      (let ((pin (car pins)))
         (display "v[" port)
	 (mathematica:quoted (car pin) port)
	 (display "," port)
	 (mathematica:quoted (car (cdr pin)) port)
	 (display "]=v[" port)
	 (mathematica:quoted netname port)
	 (display "];" port)
	 (newline port)
         (mathematica:write-pin-voltages netname (cdr pins) port)
      )
   )
)


(define (mathematica:write-voltages netnames port)
  (if (not (null? netnames))
      (let ((netname (car netnames)))
         (mathematica:write-pin-voltages netname 
	    (gnetlist:get-all-connections netname) port)
         (mathematica:write-voltages (cdr netnames) port))))


(define (mathematica:write-node-currents pins port)
   (let ((pin (car pins)))
      (display "i[" port)
      (mathematica:quoted (car pin) port)
      (display "," port)
      (mathematica:quoted (car (cdr pin)) port)
      (display "]" port)
      (if (not (null? (cdr pins )))
      	 (begin
	    (display "+" port)
	    (mathematica:write-node-currents (cdr pins) port)
	 )
      )
   )
)

(define (mathematica:,newline port)
   (display "," port)
   (newline port)
)


(define (mathematica:write-currents netnames first port)
   (if (not (null? netnames))
      (let ((netname (car netnames)))
         (if (not (equal? netname "GND"))
            (begin
	       (if (not first)
	          (mathematica:,newline port)
		)
	       (mathematica:write-node-currents 
	          (gnetlist:get-all-connections netname) port)
 	       (display "==0" port)
	       (mathematica:write-currents (cdr netnames) #f port)
             )
	    (mathematica:write-currents (cdr netnames) first port)
         )
      )
   )
)

(define (mathematica:write-device-value device value refdes port)
   (display (string-downcase device) port)
   (display "[value->" port)
   (display value port)
   (display "][" port)
   (mathematica:quoted refdes port)
   (display "]" port)
)

(define (mathematica:write-device-model model refdes port)
   (display model port)
   (display "[" port)
   (mathematica:quoted refdes port)
   (display "]" port)
 )


(define (mathematica:write-model refdes port)
   (let ((device (gnetlist:get-package-attribute refdes "device"))
         (value (gnetlist:get-package-attribute refdes "value"))
         (model (gnetlist:get-package-attribute refdes "model")))
      (if (equal? model "unknown")
         (if (equal? value "unknown")
	    (mathematica:write-device-value device (string-downcase refdes)
	       refdes port)
            (mathematica:write-device-value device value refdes port)
	 )
	 (mathematica:write-device-model model refdes port)
      )
   )
)

(define (mathematica:write-models refdeses first port)
   (if (not (null? refdeses))
      (let ((refdes (car refdeses)))
         (if (not first)
	    (mathematica:,newline port)
	 )
	 (mathematica:write-model refdes port)
	 (mathematica:write-models (cdr refdeses) #f port)
      )
   )
)

(define (mathematica:list-voltages netnames first port)
   (if (not (null? netnames))
      (let ((netname (car netnames)))
         (if (not (equal? netname "GND"))
            (begin
	       (if (not first)
	          (mathematica:,newline port)
		)
		(display "v[" port)
		(mathematica:quoted netname port)
		(display "]" port)
	        (mathematica:list-voltages (cdr netnames) #f port)
             )
	    (mathematica:list-voltages (cdr netnames) first port)
         )
      )
   )
)


(define (mathematica:list-pin-currents pins port)
   (if (not (null? pins))
      (let ((pin (car pins)))
         (mathematica:,newline port)
	 (display "i[" port)
         (mathematica:quoted (car pin) port)
         (display "," port)
         (mathematica:quoted (car (cdr pin)) port)
         (display "]" port)
	 (mathematica:list-pin-currents (cdr pins) port)
      )
   )
)

	       
(define (mathematica:list-currents netnames port)
   (if (not (null? netnames))
      (let ((netname (car netnames)))
	 (mathematica:list-pin-currents 
	    (gnetlist:get-all-connections netname) port)
	    (mathematica:list-currents (cdr netnames) port)
      )
   )
)
 

(define (mathematica output-filename)
  (let ((port (open-output-file output-filename)) 
  	(nets (gnetlist:get-all-unique-nets "dummy")))
     (mathematica:write-voltages nets port)
     (display "nodeEquations={" port)
     (newline port)
     (mathematica:write-currents nets #t port)
     (display "};" port)
     (newline port)
     (display "modelEquations={" port)
     (newline port)
     (mathematica:write-models packages #t port)
     (display "};" port)
     (newline port)
     (display "variables={" port)
     (newline port)
     (mathematica:list-voltages nets #t port)
     (mathematica:list-currents nets port)
     (display "};" port)
     (newline port)
   )
)
