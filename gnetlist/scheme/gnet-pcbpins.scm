;;; gEDA - GPL Electronic Design Automation
;;; gnetlist - gEDA Netlist
;;; Backend for propagating pin names from gschem to footprints in pcb
;;; Copyright (C) 2005-2010 Dan McMahill
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


(use-modules (ice-9 regex))

;; A comma or close parenthesis will cause problems with the pcb
;; action script, so if one of the arguments to ChangePinName contains
;; one it should be quoted.  Any quote characters within the argument
;; are escaped.
;;
;; At present, this function only quotes if there is a comma or close
;; parenthesis present in the string.
(define pcbpins:quote_string
  (lambda (s)
    (if (string-match "[,)]" s)
        (string-join (list "\""
                           (regexp-substitute/global #f "\"" s 'pre "\\\"" 'post)
                           "\"")
                     "")
        s)))

;; write out the pins for a particular component
(define pcbpins:component_pins
  (lambda (package pins)
    (if (and (not (null? package)) (not (null? pins)))
        (begin
          (let (
                (pin (car pins))
                (label #f)
                (pinnum #f)
                )
            (display "ChangePinName(")
            (display (pcbpins:quote_string package))
            (display ", ")

            (set! pinnum (gnetlist:get-attribute-by-pinnumber package pin "pinnumber"))

            (display (pcbpins:quote_string pinnum))
            (display ", ")

            (set! label (gnetlist:get-attribute-by-pinnumber package pin "pinlabel"))
            (if (string=? label "unknown")
                (set! label pinnum)
                )
            (display (pcbpins:quote_string label))
            (display ")\n")
            )
          (pcbpins:component_pins package (cdr pins))
          )
        )
    )
  )


;; write out the components
(define pcbpins:components
   (lambda (packages symcnt)
      (if (not (null? packages))
         (begin
           (let ((package (car packages)))

             ;;
             (display "\n# Start of element ")
             (display package)
             (newline)

             ;; write the pins
             (pcbpins:component_pins package (gnetlist:get-pins package))
             )
           (pcbpins:components (cdr packages) (+ symcnt 1))
           )
         )
      )
   )

;; The top level netlister for pcbpins
(define (pcbpins output-filename)
  (set-current-output-port (gnetlist:output-port output-filename))

  ;; write the header
  (display "# Pin name action command file\n")

  ;; write the components
  (pcbpins:components packages 1)

  ;; close netlist
  (close-output-port (current-output-port)))
