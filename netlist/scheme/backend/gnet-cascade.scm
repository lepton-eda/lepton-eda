;;; Lepton EDA netlister
;;; Backend for cascade (http://rfcascade.sourceforge.net)
;;; Copyright (C) 2003-2010 Dan McMahill
;;; Copyright (C) 2003-2017 gEDA Contributors
;;; Copyright (C) 2018 Lepton EDA Contributors
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

(use-modules (ice-9 match)
             (srfi srfi-1)
             (netlist error)
             (netlist schematic)
             (netlist schematic toplevel))

;; Locate and print out the global defaults if the element exists
(define cascade:write-defaults-top
  (lambda (pkgs)
    (if (not (null? pkgs))
        (let ( (pkg (car pkgs)) )
          (if (string=? (get-device pkg) "cascade-defaults-top")
              (begin
                (display "# Initial global defaults\n")
                (display "defaults ")
                (map (lambda (attrib)
                       (let ((val (gnetlist:get-package-attribute pkg attrib)))
                         (if (not (string=? val "unknown"))
                             (display (string-append attrib "=" val " "))
                             )
                         )
                       )
                     (list "rin" "RIN" "rout" "ROUT" "rho" "RHO")
                     )
                (newline)
                (newline)
                )
              (cascade:write-defaults-top (cdr pkgs))
              )
          )
        )
    )
  )

(define (cascade:next-package package pinnumber)
  (match (get-nets package pinnumber)
    ((netname (p1 . n1) (p2 . n2) . rest)
     (if (string=? p1 package) p2 p1))
    (_ #f)))

;; Locate and print out the "source" line and return the refdes of
;; the first element in the cascade
(define (cascade:write-source pkgs)
  (define (write-source-statement package)
    (format #t "source ~A\n"
            (string-join
             (filter-map
              (lambda (attrib)
                (let ((val (gnetlist:get-package-attribute package attrib)))
                  (and (not (unknown? val))
                       (format #f "~A=~A" attrib val))))
              (list "c" "C" "cn0" "CN0" "cn" "CN" "bw" "BW"))
             " ")))

  (if (null? pkgs)
      '()
      (let ((package (car pkgs)))
        (if (string=? (get-device package) "cascade-source")
            (begin
              (write-source-statement package)
              ;; Return next package. It is connected to pin 1,
              ;; since the source package has only one pin.
              (cascade:next-package package "1"))
            (cascade:write-source (cdr pkgs))))))

;; recursively follow the cascade and print out each element as its
;; found
(define (cascade:follow-cascade pkg)
  (when pkg
    ;; Is this a "defaults" element or a normal element?
    ;; If its a defaults element, then print "defaults"
    ;; instead of the reference designator because thats
    ;; a keyword for cascade.
    (format #t
            "~A ~A\n"
            (if (string=? (get-device pkg) "cascade-defaults")
                "defaults "
                pkg)

            ;; spit out all the relevant attributes for element or
            ;; defaults lines
            (string-join
             (filter-map
              (lambda (attrib)
                (let ((val (gnetlist:get-package-attribute pkg attrib)))
                  (and (not (unknown? val))
                       (format #f "~A=~A" attrib val))))
              (list "g" "G" "gp" "GP" "gv" "GV" "nf" "NF" "iip3"
                    "IIP3" "r" "R" "rin" "RIN" "rout" "ROUT"
                    "rho" "RHO")) " "))

    (cascade:follow-cascade (cascade:next-package pkg "2"))))

;; The top level netlister for cascade
(define cascade
   (lambda (output-filename)
     (message
      (format #f
              "
----------------------------------------
Lepton EDA netlister Cascade Backend
----------------------------------------

Writing to ~S...
"
              (if output-filename
                  (string-append "output file " output-filename)
                  "stdout")))

     (let ((first_block #f)
           (packages (schematic-package-names (toplevel-schematic))))

        ;; write the header
        (display "# Cascade (http://rfcascade.sourceforge.net)\n")
        (display "# Created with Lepton EDA netlister\n\n")

        ;; Write out an initial "defaults" line if it exists
        (cascade:write-defaults-top packages)

        ;; Write out the "source" line and keep track of what its
        ;; connected to.  If we couldn't find the source, then
        ;; exit out.
        (display "# Source definition\n")
        (set! first_block (cascade:write-source packages))
        (when (null? first_block)
          (netlist-error 1 "You must include a source element in your schematic!~%"))

        ;; write the components
        (display "\n# Cascaded system\n")
        (cascade:follow-cascade first_block)

        ;; write the footer
        (newline)
        (display "# End of netlist created by Lepton EDA netlister\n\n")
        )

      (message "done\n")
      )
   )
