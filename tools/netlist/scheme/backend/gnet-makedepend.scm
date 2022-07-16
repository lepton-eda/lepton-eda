;;; Lepton EDA netlister
;;; Copyright (C) 2011-2012 Dan White <dan@whiteaudio.com>
;;; Copyright (C) 2016-2017 gEDA Contributors
;;; Copyright (C) 2017-2020 Lepton EDA Contributors
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


(use-modules (ice-9 regex)
             (srfi srfi-1)
             (sxml transform)
             (sxml match)
             (lepton page)
             (netlist schematic)
             (netlist schematic-component)
             (netlist schematic toplevel))


; Split a filename into 3 parts:
; 1- base name
; 2- page number
; 3- extension
;
; base_nam3-#.ext -> [[:alnum:]_]+-[:digit:]+.ext
(define makedepend-scheme
  "([[:alnum:]_-]+)-([[:digit:]]+).([[:alpha:]]+)$")

(define (makedepend:basename makedepend-scheme name)
  (let ((match (string-match makedepend-scheme name)))
    (if match
        ;; Numbered schematic files.
        (match:substring match 1)

        ;; Otherwise only one file.
        (let ((dot-position (string-rindex name #\.)))
          (if dot-position
              (string-take name dot-position)
              name)))))


;;; Makes a list of dependencies for toplevel pages.
(define (schematic-sxml->dependency-list tree)
  (define (page-name p)
    (and (page? p)
         (basename (page-filename p))))

  (define (sxml-page-name page)
    (sxml-match
     page
     ((page (@ (page-name ,name)) ,deps ...) name)))

  (define (sxml-page-prerequisites page)
    (sxml-match page ((page (@ (prerequisites ,pr)) ,deps ...) pr)))

  ;; Package prerequisites are the names of its subcircuit pages
  ;; defined in its source= attribs and the subcircuit names the
  ;; pages depend on, i.e. their prerequisites. Here we append all
  ;; that info and remove duplicates.
  (define (package-prerequisites pages)
    (delete-duplicates
     (append (map sxml-page-name pages)
             (append-map sxml-page-prerequisites pages))))

  ;; Form circuit name simply by first page. See the
  ;; makedepend:basename function above for how it works.
  (define (circuit-name pages)
    (and (not (null? pages))
         (string-append (makedepend:basename makedepend-scheme
                                             (sxml-page-name (first pages)))
                        ".cir")))

  (define (sxml-package-name package)
    (sxml-match
     package
     ((package (@ (package-name ,name)) ,deps ...) name)))

  ;; SXML package-name attribute is #f if package is not composite
  ;; and has no file= attribute attached to it, so we filter such
  ;; packages out.
  (define (page-prerequisites packages)
    (filter-map sxml-package-name packages))

  (define (sxml-package-rules package)
    (sxml-match
     package
     ((package (@ (rules ,r)) ,deps ...) r)))

  (define (page-rules packages)
    (delete-duplicates
     (append-map sxml-package-rules packages)))

  (define (sxml-page-rules page)
    (sxml-match
     page
     ((page (@ (rules ,r)) ,deps ...) r)))

  (define (package-rules name pages)
    (if name
        (delete-duplicates
         `((,name ,(package-prerequisites pages)) . ,(append-map sxml-page-rules pages)))
        ;; Circuit name is #f when it has no dependencies, so we
        ;; don't produce any rules here.
        '()))

  (define (get-first-file-attrib package)
    (and (schematic-component? package)
         (schematic-component-attribute package 'file)))

  ;; Add the last rule for the toplevel fake package. This will
  ;; produce a line of the form: "all: package.cir".
  (define (make-toplevel-rule package)
    `(("all" (,(sxml-package-name package))) ,@(sxml-package-rules package)))

  (pre-post-order
   tree
   `((*TOP* . ,(lambda (x . t)
                 (let ((package (car t)))
                   (make-toplevel-rule package))))
     (page . ,(lambda (x pg . deps)
                `(page (@ (page-name ,(page-name pg))
                          (prerequisites ,(page-prerequisites deps))
                          (rules ,(page-rules deps))))))
     (package . ,(lambda (x pk . deps)
                   (let ((cirname (circuit-name deps)))
                     `(package (@ (package-name ,(or cirname
                                                     (get-first-file-attrib pk)))
                                  (rules ,(package-rules cirname deps)))))))
     (*text* . ,(lambda (x t) t))
     (*default* . ,(lambda (x . t) t)))))


(define (format-dependency-list ls)
  (for-each (lambda (x) (format #t
                           "~A: ~A\n"
                           (car x)
                           (string-join (cadr x))))
            ls))

(define (makedepend output-filename)
  (let ((dep-list (schematic-sxml->dependency-list
                   (schematic-tree (toplevel-schematic)))))
    (format-dependency-list dep-list)))

;; vim:shiftwidth=2
