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
             (srfi srfi-26)
             (netlist schematic toplevel))

(define bom2:open-input-file
  (lambda (options)
    (let ((filename (backend-option-ref options 'attrib_file "attribs")))
      (if (file-exists? filename)
          (open-input-file filename)
          (if (backend-option-ref options 'attribs) #f
              (netlist-error
               1
               "ERROR: Attribute file '~A' not found. You must do one of the following:
         - Create an 'attribs' file
         - Specify an attribute file using -Oattrib_file=<filename>
         - Specify which attributes to include using -Oattribs=attrib1,attrib2,... (no spaces)
"
               filename))))))

(define bom2
  (lambda (output-filename)
    (let* ((options (backend-getopt
                     (gnetlist:get-backend-arguments)
                     '((attrib_file (value #t)) (attribs (value #t)))))
           (attriblist (bom2:parseconfig (bom2:open-input-file options) options)))
      (and attriblist
           (begin
             (bom2:printlist (append (cons 'refdes attriblist) (list "qty")) #\:)
             (newline)
             (bom2:printbom (bom2:components (schematic-package-names (toplevel-schematic))
                                             attriblist)
                            0))))))

(define bom2:printbom
  (lambda (bomlist count)
    (if (not (null? bomlist))
      (if (not (null? (caar bomlist)))
        (begin
          (display (caaar bomlist))
          (if (not (null? (cdaar bomlist)))
            (write-char #\,))
          (bom2:printbom (cons (cons (cdaar bomlist)(cdar bomlist))(cdr bomlist)) (+ count 1))
        )
        (begin
          (display #\:)
          (bom2:printlist (cdar bomlist) #\:)
          (display #\:)
          (display count)
          (newline)
          (bom2:printbom (cdr bomlist) 0)
        )))))

(define bom2:printlist
  (lambda (ls delimiter)
    (if (null? ls)
        #f
        (begin
          (display (car ls))
          (if (not (null? (cdr ls)))
            (write-char delimiter))
          (bom2:printlist (cdr ls) delimiter)))))

; Parses attrib file. Returns a list of read attributes.
(define bom2:parseconfig
  (lambda (port options)
    (let ((attribs (backend-option-ref options 'attribs)))
      (if attribs (string-split attribs #\,)
          (and port
               (let ((read-from-file (read-delimited " \n\t" port)))
                 (cond ((eof-object? read-from-file)
                        '())
                       ((= 0 (string-length read-from-file))
                        (bom2:parseconfig port options))
                       (else
                        (cons read-from-file (bom2:parseconfig port options))))))))))

(define bom2:match-list?
  (lambda (l1 l2)
    (cond
      ((and (null? l1)(null? l2))#t)
      ((null? l1) #f)
      ((null? l2) #f)
      ((not (string=? (car l1)(car l2)))#f)
      (#t (bom2:match-list? (cdr l1)(cdr l2))))))

(define bom2:match?
  (lambda (uref attriblist bomlist)
    (if (null? bomlist)
      (list (cons (list uref) attriblist))
      (if (bom2:match-list? attriblist (cdar bomlist))
;;        (cons (cons (cons uref (caar bomlist)) (cdar bomlist))(cdr bomlist))
        (cons (cons (merge (list uref) (caar bomlist) string<? ) (cdar bomlist))(cdr bomlist))
        (cons (car bomlist)(bom2:match? uref attriblist (cdr bomlist)))))))

(define (bom2:in-bom? package)
  (string=? "unknown"
            (gnetlist:get-package-attribute package "nobom")))

(define (bom2:components-impl ls attriblist bomlist)
  (if (null? ls)
      bomlist
      (let* ((package (car ls))
             (attribs (bom2:find-attribs package attriblist)))
        (bom2:components-impl (cdr ls) attriblist
                              (if (bom2:in-bom? package)
                                  (bom2:match? package attribs bomlist)
                                  bomlist)))))

(define (bom2:components ls attriblist)
   (bom2:components-impl ls attriblist '()))

(define (bom2:find-attribs package attriblist)
  (map (cut gnetlist:get-package-attribute package <>) attriblist))

;;
;; Bill of Material backend written by Matt Ettus ends here
;;
;; --------------------------------------------------------------------------
