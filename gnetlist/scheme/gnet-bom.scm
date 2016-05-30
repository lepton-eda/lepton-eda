;;; gEDA - GPL Electronic Design Automation
;;; gnetlist - gEDA Netlist
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

(use-modules (ice-9 rdelim) ;; guile-1.8 fix
             (gnetlist backend-getopt))

(define (bom:error)
  (format
   (current-error-port)
   "ERROR: Attribute file '~A' not found. You must do one of the following:
         - Create an 'attribs' file
         - Specify an attribute file using -Oattrib_file=<filename>
         - Specify which attributes to include using -Oattribs=attrib1,attrib2,... (no spaces)
"
          filename)
  (primitive-exit 1))

(define (bom:open-input-file options file-name attrib-list)
  (if (file-exists? file-name)
      (open-input-file file-name)
      (if attrib-list
          #f
          (bom:error))))

(define bom
  (lambda (output-filename)
    (let* ((options (backend-getopt
                     (gnetlist:get-backend-arguments)
                     '((attrib_file (value #t)) (attribs (value #t)))))
           (option-filename
            (backend-option-ref options 'attrib_file "attribs"))
           (option-attribs (backend-option-ref options 'attribs))
           (port (bom:open-input-file options option-filename option-attribs))
           (attriblist (bom:parseconfig port option-attribs)))
      (and attriblist
           (with-output-to-port (gnetlist:output-port output-filename)
             (lambda ()
               (bom:printlist (cons "refdes" attriblist))
               (bom:components packages attriblist)))))))

(define (bom:printlist ls)
  (format #t "~A\n" (string-join ls "\t")))

(define (bom:read-attrib-list)
  (define delimiters
    (string->char-set " \n\t"))

  (define (non-empty-string? s)
    (not (string=? "" s)))

  (filter non-empty-string?
          (string-split (read-string) delimiters)))

; Parses attrib file or argument. Returns a list of read attributes.
(define (bom:parseconfig port attribs)
  (if attribs (string-split attribs #\,)
      (and port
           (with-input-from-port port bom:read-attrib-list))))

(define (bom:components ls attriblist)
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

;;
;; Bill of Material backend written by Matt Ettus ends here
;;
;; --------------------------------------------------------------------------
