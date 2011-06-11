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
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
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
  (let* ((match (string-match makedepend-scheme name))
         (base (match:substring match 1))
         (page (match:substring match 2))
         (ext  (match:substring match 3))
         )
    (list base page ext)
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



(define (makedepend:output-make-command input-files sources files port)
  (let* (;lazy version, use first filename only for naming scheme
         (scheme-split (makedepend:split-filename makedepend-scheme (car input-files)))
         (base (first scheme-split))
         (page (second scheme-split))
         (ext  (third scheme-split))
        )

    ;schematic deps
    (format port "~a: ~a\n"
            (string-join input-files " ")
            (string-join sources " "))

    ;netlist deps
    (format port "~a.cir: ~a ~a\n"
            base
            (string-join input-files " ")
            (string-join files " "))
  )
)



(define (makedepend output-filename)
  (let* ((port (open-output-file output-filename))
         (source-attrs (makedepend:get-all-attr-values "source" packages))
         (file-attrs (makedepend:get-all-attr-values "file" packages))
         (input-files (gnetlist:get-input-files))
        )
    (makedepend:output-make-command input-files source-attrs file-attrs port)
    (close-output-port port)
  )
)

;; vim:shiftwidth=2
