;;; Lepton EDA netlister
;;; Copyright (C) 1998-2010 Ales Hvezda
;;; Copyright (C) 1998-2017 gEDA Contributors
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


;; --------------------------------------------------------------------------
;;
;; Bill of Material backend written by Matt Ettus starts here
;;

;;; Bill Of Materials Generator
;;; You must have a file called attribs in the pwd
;;; The file should be a text list of attributes you want listed,
;;; One per line.  No comments are allowed in the file.
;;; Questions? Contact matt@ettus.com
;;; This software is released under the terms of the GNU GPL

(use-modules (ice-9 rdelim)
             (netlist backend-getopt)
             (netlist error)
             (netlist schematic)
             (netlist schematic toplevel))


(define (bom:error filename)
  "Prints an error, if the bom backend cannot find an appropriate
source to read attributes, and exits with return code 1."
  (netlist-error
   1
   "ERROR: Attribute file '~A' not found. You must do one of the following:
         - Create an 'attribs' file
         - Specify an attribute file using -Oattrib_file=<filename>
         - Specify which attributes to include using -Oattribs=attrib1,attrib2,... (no spaces)
"
   filename))


(define (bom:printlist ls)
  "Outputs the given list LS to the standard output as a tab
separated list."
  (format #t "~A\n" (string-join ls "\t")))


(define (bom:read-attrib-list)
  "Reads text from the standard input and translates it into a
list of attribute names. The attribute names must be delimited by
spaces, tabs, or newlines."
  (define delimiters
    (string->char-set " \n\t"))

  (define (non-empty-string? s)
    (not (string-null? s)))

  (filter non-empty-string?
          (string-split (read-string) delimiters)))


(define (bom:parseconfig filename attribs)
  "Reads attribute names from the list ATTRIBS which must be a CSV
list. If ATTRIBS is #f, reads FILENAME for the list of
attributes. If the file is not found, outputs an error."
  (if attribs
      (string-split attribs #\,)
      (if (file-exists? filename)
          (with-input-from-file filename bom:read-attrib-list)
          (bom:error filename))))


(define (bom:components ls attriblist)
  "Outputs a tab separated list of attribute values for each
attribute name in ATTRIBLIST for components from the list
LS. The component instances in LS having the attribute \"nobom=1\"
are filtered out."
  (define (no-bom-package? package)
    (string=? "1" (gnetlist:get-package-attribute package "nobom")))

  (define (component-attrib-values package attriblist)
    (map (lambda (attrib)
           (gnetlist:get-package-attribute package attrib))
         attriblist))

  (define (output-component-attrib-values package)
    (and (not (no-bom-package? package))
         (bom:printlist
          (cons package (component-attrib-values package attriblist)))))

  (for-each output-component-attrib-values ls))


(define (bom output-filename)
  "Outputs BOM (Bill of Materials) to OUTPUT-FILENAME.
First line is a list of attribute names, all others are lists of
corresponding attribute values for all schematic
components. Attributes for components having the attribute
\"nobom=1\" are not output.
The following gnetlist options may be used to provide the list of
attribute names:
  - -Oattribs=<attrib-name-list>, where <attrib-name-list> must be a
    CSV list,
  - -Oattrib_file=<filename>.
The second option defines the file from which the attribute names
must be read, if the first option was not given. If no option is
given, the procedure tries to read the attribute names from file
\"attribs\" in the current directory.
If the attribute names are read from a source file, they must be
separated by spaces, tabs, or newlines.
An error will be displayed, if no attribute name source is found."
  (let* ((options (backend-getopt
                   (gnetlist:get-backend-arguments)
                   '((attrib_file (value #t)) (attribs (value #t)))))
         (option-filename
          (backend-option-ref options 'attrib_file "attribs"))
         (option-attribs (backend-option-ref options 'attribs))
         (attriblist (bom:parseconfig option-filename option-attribs)))
    (bom:printlist (cons "refdes" attriblist))
    (bom:components (schematic-package-names (toplevel-schematic))
                    attriblist)))

;;
;; Bill of Material backend written by Matt Ettus ends here
;;
;; --------------------------------------------------------------------------
