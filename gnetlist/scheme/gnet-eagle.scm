;;; gEDA - GPL Electronic Design Automation
;;; gnetlist - gEDA Netlist
;;; Copyright (C) 2004-2010 Braddock Gaskill (braddock@braddock.com,
;;;                                           adapted PCB code to Eagle)
;;; Copyright (C) 1998-2010 Ales Hvezda
;;; Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
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

;; EAGLE netlist format

;; This procedure takes a net name as determined by gnetlist and
;; modifies it to be a valid eagle net name.
;;
(define eagle:map-net-names
  (lambda (net-name)
    (let ((net-alias net-name)
          )
      ;; Convert to all upper case because Eagle seems
      ;; to do that internally anyway and we'd rather do
      ;; it here to catch shorts created by not preserving
      ;; case.  Plus we can eliminate lots of ECO changes
      ;; that will show up during backannotation.
      (string-upcase net-alias)
      )
    )
  )

(define eagle:components
   (lambda (packages)
      (if (not (null? packages))
         (begin
            (let ((pattern (gnetlist:get-package-attribute (car packages)
                                                           "pattern"))
            ;; The above pattern should stay as "pattern" and not "footprint"
                  (package (car packages))
                  (lib (gnetlist:get-package-attribute (car packages) "lib"))
                  (value (gnetlist:get-package-attribute (car packages) "value"))
                  (device (gnetlist:get-package-attribute (car packages) "device"))
                  )
               (if (not (string=? pattern "unknown"))
                  (display pattern))
               (display "ADD '")
               (display package)
               (display "' ")
;;             (display "' TQFP144@atmel (0 0)")
;;;            (write-char #\tab)
               (display (gnetlist:get-package-attribute package "footprint"))
               (display "@")
               (if (not (string=? lib "unknown"))
                   (display lib)
                   (display "smd-ipc"))
               (display " (1 1);")
               (newline)
               (if (not (string=? value "unknown"))
                   (begin
                     (display "VALUE '")
                     (display package)
                     (display "' '")
                     (display value)
                     (display "';")
                     (newline)
                     )
                   (if (not (string=? device "unknown"))
                       (begin
                         (display "VALUE '")
                         (display package)
                         (display "' '")
                         (display device)
                         (display "';")
                         (newline)
                         )
                   ))
               )
            (eagle:components (cdr packages))))))

(define (eagle:display-connections nets)
  (let ((k ""))
    (for-each (lambda (in-string)
                (set! k (string-append k in-string)))
              (map (lambda (net)
                     (string-append "   '" (car net) "' '" (car (cdr net)) "'\r\n"))
                   nets))
    (string-append k ";\n")))


; This function is replaced with the above one. Due to non existent
; verification, this function is left commented out.
; /spe, 2002-01-08
;(define (eagle:display-connections nets)
;  (if (not (null? nets))
;      (string-append " " (car (car nets)) "." (car (cdr (car nets)))
;       (eagle:display-connections (cdr nets)))
;      "\n"))



(define eagle:write-net
   (lambda (netnames)
      (if (not (null? netnames))
         (let ((netname (car netnames)))
            (display "SIGNAL '")
            (display (gnetlist:alias-net netname))
            (display "'")
            (newline)
;            (display (gnetlist:wrap
;                     (eagle:display-connections
;                      (gnetlist:get-all-connections netname))
;                     78
;                     ""))
            (display (eagle:display-connections
                       (gnetlist:get-all-connections netname)))
            (eagle:write-net (cdr netnames))))))

(define (eagle output-filename)
  (with-output-to-port (gnetlist:output-port output-filename)
    (lambda ()
      ;; initialize the net-name aliasing
      (gnetlist:build-net-aliases eagle:map-net-names all-unique-nets)

      ;; print out the header
      (display "   ;\n")

      ;; print out the parts
      (eagle:components packages)

      ;; print out the net information
      (eagle:write-net (gnetlist:get-all-unique-nets "dummy")))))
