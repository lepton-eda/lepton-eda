;;; gEDA - GPL Electronic Design Automation
;;; gnetlist - gEDA Netlist
;;; Copyright (C) 2011 Dan White <dan@whiteaudio.com>
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
;;
;; --------------------------------------------------------------------------
;;
;; Backend to determine the dependencies of a given schematic.
;;
;; Output is Makefile lines relevant to the input schematic(s):
;;
;; foo.sch: subsheetA.sch subsheetB.sch
;; foo.cir: foo.sch subsheetA.cir subsheetB.cir
;;
;; See the following for the intended usage:
;; http://www.gnu.org/software/make/manual/make.html#Automatic-Prerequisites
;;
;; Backend example description
;; ---------------------------
;; A two-page schematic (idac-1.sch idac-2.sch) contains two sub-schematic
;; symbols: inv1-1.sym and idac_8p-1.sym.  `inv1-1' is implemented with a
;; single page indicated by the symbol's attribute `source=inv1-1.sch'.  The
;; second block `idac_8p' is a three-page schematic whose symbol has a
;; `source=idac_8p-1.sch,idac_8p-2.sch,idac_8p-3.sch' attribute.  Additionally,
;; the top-level schematic pages include two `spice-include-1.sym' components
;; with `file=' attribute values `dactune.pwl' and `idac.param'.
;;
;; Such a top-level schematic depends on the following files:
;;      inv1-1.sch      - `inv1' sub-schematic
;;      idac_8p-1.sch   - `idac_8p' sub-schematic, page 1
;;      idac_8p-2.sch   - ... page 2
;;      idac_8p-3.sch   - ... page 3
;;
;;      Note: The symbol files inv1-1.sym etc. are also dependencies and not
;;            currently handled.
;;
;; For top-level SPICE simulation of `idac.cir' with hierarchical subckts, the
;; additional files are dependencies:
;;      inv1.cir    - from `gnetlist -g spice-sdb -o inv1.cir inv1-1.sch'
;;      idac_8p.cir - `gnetlist -g spice-sdb -o idac_8p.cir idac_8p-*.sch'
;;      dactune.pwl - additional SPICE include file
;;      idac.param  - ...
;;
;; Calling this backend as:
;;      gnetlist -g makedepend -o idac.d idac-*.sch
;;
;; generates the `idac.d' file contents as:
;;
;; idac-1.sch idac-2.sch: inv1-1.sch idac_8p-1.sch idac_8p-2.sch idac_8p-3.sch
;; idac.cir: idac-1.sch idac-2.sch inv1.cir idac_8p.cir dactune.pwl idac.param
;;
;; Makefile snippet for use:
;; ###
;; modules=idac
;; depends=$(addsuffix .d, $(modules))
;;
;; depend: $(depends)
;; idac.d: $(wildcard idac-*.sch)
;;     gnetlist -g makedepend -o $@ $^
;;
;; include $(depends)
;; ###
;; --------------------------------------------------------------------------
;;


(use-modules (ice-9 regex))
(use-modules (srfi srfi-1))



; Split a filename into 3 parts:
; 1- base name
; 2- page number
; 3- extension
;
; base_nam3-#.ext -> [[:alnum:]_]+-[:digit:]+.ext
(define makedepend-scheme
  "([[:alnum:]_]+)-([[:digit:]]+).([[:alpha:]]+)$")

(define (makedepend:split-filename makedepend-scheme name)
  (let ((match (string-match makedepend-scheme name)))
    (if match
      (let* ((base (match:substring match 1))
             (page (match:substring match 2))
             (ext  (match:substring match 3)))
        (list base page ext))
      (begin
        (display "ERROR: Schematic file name must take the form: BASE-PAGENUM.EXT\n"
                 (current-error-port))
        (primitive-exit 1)))
  )
)



;;
;; Returns a list of all values found for the given attribute name
;; over all packages in the input files.
;;
(define (makedepend:get-all-attr-values attribute packages)
  ;split individual values (gschem wants a single comma-sep list for source=)
  (append-map (lambda (str) (string-split str #\,))
    ;ignore non-existent values
    (delete #f
      ;collect values from all packages into a list
      (append-map
        ;get all values for a given refdes
        (lambda (x) (gnetlist:get-all-package-attributes x attribute))
        packages)))
)



(define (makedepend:output-make-command input-files sources files)
  (let* (;lazy version, use first filename only for naming scheme
         (scheme-split (makedepend:split-filename makedepend-scheme (car input-files)))
         (base (first scheme-split))
         (page (second scheme-split))
         (ext  (third scheme-split))
        )

    ;schematic deps
    (format #t "~a: ~a\n"
            (string-join input-files " ")
            (string-join sources " "))

    ;netlist deps
    (format #t "~a.cir: ~a ~a\n"
            base
            (string-join input-files " ")
            (string-join files " "))
  )
)



(define (makedepend output-filename)
  (set-current-output-port (gnetlist:output-port output-filename))
  (let* (
         (source-attrs (makedepend:get-all-attr-values "source" packages))
         (file-attrs (makedepend:get-all-attr-values "file" packages))
         (input-files (gnetlist:get-input-files))
        )
    (makedepend:output-make-command input-files source-attrs file-attrs)
  )
  (close-output-port (current-output-port))
)

;; vim:shiftwidth=2
