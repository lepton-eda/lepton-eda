;;; $Id$
;;;
;;; gEDA - GNU Electronic Design Automation
;;;
;;; Copyright (C) 2003, 2005 Dan McMahill
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

;; --------------------------------------------------------------------------
;; This is gnetlist backend for the SWITCAP switched capacitor
;; simulator.  This backend was written by Dan McMahill
;; 'mcmahill at alum dot mit dotedu' who used the SPICE backend by
;; S. Gieltjes as a starting point.

;; The following is needed to make guile 1.8.x happy.
(use-modules (ice-9 rdelim))

;; ----------------------------------------------------------------------------
;; Utility functions used by this netlister
;; ----------------------------------------------------------------------------

;; This procedure takes a net name as determined by gnetlist and
;; modifies it to be a valid SWITCAP net name.
;;
(define switcap:map-net-names
  (lambda (net-name)
    (let ((rx (make-regexp "^unnamed_net"))
	  (net-alias net-name)
          )
      ;; XXX we should use a dynamic regexp based on the current value
      ;; for the unnamed net base string.

      (cond 
       ;; Change "GND" to "0"
       ((string=? net-name "GND") (set! net-alias "0"))
       ;; remove the 'unnamed_net' part
       ((regexp-exec rx net-name) (set! net-alias (substring net-name 11)))
       (else net-name)
       )

      ;; Truncate to 7 characters
      (if (> (string-length net-alias) 7)
	  (set! net-alias (substring net-alias 0 7))
	  )
      ;; Convert to all upper case
      (string-upcase net-alias)

      )
    )
  )

;; This procedure takes a refdes as determined by gnetlist and
;; modifies it to be a valid SWITCAP refdes.  In particular,
;; we need to make sure that
;;
;; - the first character is correct for the component type
;;
;; - we do not exceed 8 characters.  Note the 8 comes from
;;   the first character which denotes component type plus
;;   7 for the unique identifier part.
;;
;; - we are all caps (switcap is not case sensitive)
;;
(define switcap:map-refdes
  (lambda (refdes)
    (let ((refdes-alias refdes)
          )

      ;; Convert to all upper case
      (string-upcase refdes-alias)

      ;; Make sure the first character is correct for
      ;; this component type
      (cond   
       ( (string=? (get-device refdes) "SWITCAP-switch")
	 (if (not (string=? (substring refdes-alias 0 1) "S"))
	     (set! refdes-alias (string-append "S" refdes-alias))))

       ( (string=? (get-device refdes) "SWITCAP-capacitor") 
	 (if (not (string=? (substring refdes-alias 0 1) "C"))
	     (set! refdes-alias (string-append "C" refdes-alias))))

       ( (string=? (get-device refdes) "SWITCAP-vcvs") 
	 (if (not (string=? (substring refdes-alias 0 1) "E"))
	     (set! refdes-alias (string-append "E" refdes-alias))))

       ( (string=? (get-device refdes) "SWITCAP-vsrc") 
	 (if (not (string=? (substring refdes-alias 0 1) "V"))
	     (set! refdes-alias (string-append "V" refdes-alias))))

       )
      
      ;; Truncate to 8 characters (1 for the first character and 
      ;; 7 for the identifier)
      (if (> (string-length refdes-alias) 8)
	  (set! refdes-alias (substring refdes-alias 0 8))
	  )

      ;; set to #t for debugging
      (if #f
	  (let ()
	    (display "(switcap:map-refdes ")
	    (display refdes)
	    (display ") ===> " )
	    (display refdes-alias )
	    (display "\n")
	    )
	  )

      refdes-alias
      )
    )
  )

;; 
;; Given a reference designator, pin number, and output port
;; write out the net name
;;
(define switcap:write-pin-net
  (lambda (package pin port)
    (display (gnetlist:alias-net (car (gnetlist:get-nets package pin))) port)
    )
  )

;; 
;; Given a reference designator, attribute name, and output port
;; write out the attribute with warnings if the attribute has
;; not been set
;;
(define switcap:write-attrib
  (lambda (package attrib port)
      (let ((val (gnetlist:get-package-attribute package attrib)))
        (if (string=? val "unknown") 
	    (begin
	      (display "*** WARNING ***\n")
	      (display "Required attribute \"")
	      (display attrib)
	      (display "\" is not set on component \"")
	      (display package)
	      (display "\".  Please correct this.\n\n")
	      ))
	(display val port)
	val
	)))

;; ----------------------------------------------------------------------------
;; Individual component netlist functions
;; ----------------------------------------------------------------------------

;;
;; capacitor
;;
;; Form is:
;;
;; C### (N1 N2) value;
;;
;; 
(define switcap:write-cap
  (lambda (package port)            
    ( begin
      ;; Write out the refdes
      (display "     " port)
      (display (gnetlist:alias-refdes package) port)
      (display " " port)

      ;; Write out the nodes
      ;; Write out the nodes
      (display "(" port)
      (switcap:write-pin-net package "1" port)
      (display " " port)
      (switcap:write-pin-net package "2" port)
      (display ") " port)

      ;; Write the value
      (switcap:write-attrib package "value" port)
      
      ;; finish the line
      (display ";\n" port)
      )))

;;
;; switch
;;
;; Form is:
;;
;; S### (N1 N2) clk;
;;
;; 
(define switcap:write-switch
  (lambda (package port)            
    ( begin
      ;; Write out the refdes
      (display "     " port)
      (display (gnetlist:alias-refdes package) port)
      (display " " port)

      ;; Write out the nodes
      (display "(" port)
      (switcap:write-pin-net package "1" port)
      (display " " port)
      (switcap:write-pin-net package "2" port)
      (display ") " port)

      ;; Write the clock
      (switcap:write-attrib package "clock" port)

      ;; finish the line
      (display ";\n" port)
      )))

;;
;; voltage controlled voltage source
;;
;; Form is:
;;
;; E### (OUTP OUTM INP INM) gain;
;;
;; 
(define switcap:write-vcvs
  (lambda (package port)            
    ( begin
      ;; Write out the refdes
      (display "     " port)
      (display (gnetlist:alias-refdes package) port)
      (display " " port)

      ;; Write out the nodes
      (display "(" port)
      (switcap:write-pin-net package "1" port)
      (display " " port)
      (switcap:write-pin-net package "2" port)
      (display " " port)
      (switcap:write-pin-net package "3" port)
      (display " " port)
      (switcap:write-pin-net package "4" port)
      (display ") " port)

      ;; Write the clock
      (switcap:write-attrib package "gain" port)

      ;; finish the line
      (display ";\n" port)
      )))

;;
;; voltage source
;;
;; Form is:
;;
;; V### (OUTP OUTM);
;;
;; 
(define switcap:write-vsrc
  (lambda (package port)            
    ( begin
      ;; Write out the refdes
      (display "     " port)
      (display (gnetlist:alias-refdes package) port)
      (display " " port)

      ;; Write out the nodes
      (display "(" port)
      (switcap:write-pin-net package "1" port)
      (display " " port)
      (switcap:write-pin-net package "2" port)
      (display ")" port)

      ;; finish the line
      (display ";\n" port)
      )))

;;
;; Timing block -- clock definition
;;
;; Form is:
;;
;; CLOCK clock_name period (phi_start phi_stop)
;;
;; 
(define switcap:write-clock
  (lambda (package port)            
    ( begin
      (display "     CLOCK " port)
      (display (string-append package " ") port)
      (switcap:write-attrib package "PERIOD" port)
      (display " (" port)
      (switcap:write-attrib package "PSTART" port)
      (display " " port)
      (switcap:write-attrib package "PSTOP" port)
      (display ");\n" port)
      )))

;;
;; Timing block -- master clock period
;;
;; Form is:
;;
;; PERIOD clock_period;
;;
;; 
(define switcap:write-timing
  (lambda (package port)            
    ( begin
      (display "     PERIOD " port)
      (switcap:write-attrib package "PERIOD" port)

      ;; finish the line
      (display ";\n" port)
      )))

;;
;; Title
;;
;; Form is:
;;
;; TITLE: my title;
;;
;; Can only have 64 characters in the title 
;; XXX - need to truncate to 64 chars
(define switcap:write-title
  (lambda (package port)            
    ( begin
      (display "TITLE:" port)
      (switcap:write-attrib package "TITLE" port)

      ;; finish the line
      (display ";\n\n" port)
      )))

;;
;; Options
;;
;; Form is:
;;
;; OPTIONS; OPT1; OPT2; ...; END;
;;
;; valid options are:
;;   WIDTH132
;;   NOLIST
;;   REPORT
;;   NOOVRLAY
;;   GRID
(define switcap:write-options
  (lambda (package port)            
    ( begin
      (display "OPTIONS; " port)
      (switcap:write-attrib package "OPTIONS" port)

      ;; finish the line
      (display " END;\n\n" port)
      )))

;;
;; Analysis block
;;
;; For now, we only support writing the analysis blocks in a file
;; and including that via a switcap-analysis-1.sym instantiation
;;
(define switcap:write-analysis
  (lambda (package port)            
    (display "/* reading analysis from \"" port)
    (let ((fname (switcap:write-attrib package "file" port)))
      (display "\" */ \n" port)
      (switcap:cat-file (open-input-file fname) port)
      )))

(define switcap:cat-file
  (lambda (ip op)
    (define line "")
    (set! line (read-line ip))
    (if (not (eof-object? line))
	(begin 
	  (write-line line op)
	  (switcap:cat-file ip op)
	)
	(close-port ip))
    ))

;; ----------------------------------------------------------------------------
;; Netlist functions for each of the blocks in the switcap netlist
;; ----------------------------------------------------------------------------


;; 
;; Switcap netlist header
;;
(define switcap:write-top-header
  (lambda (port)
    (display "/* Switcap netlist produced by gnetlist (part of gEDA) */\n" port)  
    (display "/* See http://www.geda.seul.org for more information.  */\n" port)  
    (display "/* Switcap backend written by Dan McMahill             */\n" port)
    (display "\n\n" port)
    )
  )

;;
;; The main TITLE and OPTIONS block dispatcher
;;
(define switcap:write-title-block
  (lambda (port ls)
     (if (not (null? ls))
      (let ((package (car ls)))
        (cond   
          ( (string=? (get-device package) "SWITCAP-options")
              (switcap:write-options package port))
          ( (string=? (get-device package) "SWITCAP-title")
              (switcap:write-title package port))
	  )
        (switcap:write-title-block port (cdr ls)) ))))  

;;
;; The main TIMING block dispatcher
;;
(define switcap:write-timing-block
  (lambda (port ls)
     (if (not (null? ls))
      (let ((package (car ls)))
        (cond   
          ( (string=? (get-device package) "SWITCAP-clock")
              (switcap:write-clock package port))
          ( (string=? (get-device package) "SWITCAP-timing")
              (switcap:write-timing package port))
	  )
        (switcap:write-timing-block port (cdr ls)) ))))  

;;
;; The main CIRCUIT block netlist dispatcher.  
;;
(define switcap:write-netlist
  (lambda (port ls)
     (if (not (null? ls))
      (let ((package (car ls)))
        (cond   
          ( (string=? (get-device package) "SWITCAP-switch")
              (switcap:write-switch package port))
          ( (string=? (get-device package) "SWITCAP-capacitor") 
              (switcap:write-cap package port))
          ( (string=? (get-device package) "SWITCAP-vcvs") 
              (switcap:write-vcvs package port))
          ( (string=? (get-device package) "SWITCAP-vsrc") 
              (switcap:write-vsrc package port))
	  )
        (switcap:write-netlist port (cdr ls)) ))))  

;;
;; The main ANALYSIS block dispatcher
;;
(define switcap:write-analysis-block
  (lambda (port ls)
     (if (not (null? ls))
      (let ((package (car ls)))
        (cond   
          ( (string=? (get-device package) "SWITCAP-analysis")
              (switcap:write-analysis package port))
	  )
        (switcap:write-analysis-block port (cdr ls)) ))))  


;; ----------------------------------------------------------------------------
;; Switcap netlist generation -- top level
;; ----------------------------------------------------------------------------
(define switcap
  (lambda (output-filename)
    (let ((port (open-output-file output-filename)))

      ;; initialize the net-name aliasing
      (gnetlist:build-net-aliases switcap:map-net-names all-unique-nets)

      ;; initialize the refdes aliasing
      (gnetlist:build-refdes-aliases switcap:map-refdes packages)

      (switcap:write-top-header port)
      (switcap:write-title-block port packages)
      (display "TIMING;\n" port)
      (switcap:write-timing-block port packages)
      (display "END;\n\n" port)
      (display "CIRCUIT;\n" port)
      (switcap:write-netlist port packages)
      (display "END;\n\n" port)
      (switcap:write-analysis-block port packages)
      (display "\n\n/* End of SWITCAP netlist */\n" port)
      (display "END;\n" port)
      (close-output-port port))))


;; --------------------------------------------------------------------------
