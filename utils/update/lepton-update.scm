#!/usr/bin/env sh
export GUILE_LOAD_COMPILED_PATH="@ccachedir@:${GUILE_LOAD_COMPILED_PATH}"
exec @GUILE@ -s "$0" "$@"
!#

;;; lepton-update - Update schematics and symbols in the gEDA/gaf
;;; file format.
;;;
;;; Copyright (C) 2021 Lepton EDA Contributors.
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
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

(eval-when (expand load eval)
  (unless (getenv "LIBLEPTON")
    (add-to-load-path "@LEPTON_SCHEME_MODULE_DIRECTORY@")))

(use-modules (ice-9 match)
             (ice-9 rdelim)
             (ice-9 regex)
             (srfi srfi-11)
             (lepton ffi)
             (lepton srfi-37))

;;; Initialize liblepton library.
(liblepton_init)

(primitive-eval '(use-modules (lepton attrib)
                              (lepton file-system)
                              (lepton object)
                              (lepton page)
                              (lepton version)))

(define (usage)
  (define program-name (basename (car (program-arguments))))

  (format #t "Usage: ~A [OPTION ...] FILE ...\n"
          program-name
          program-name)
  (exit 0))


;;; Define uninterned symbol to aid with setting of boolean
;;; 'visible?' value.
(define *visible?* (make-symbol "*visible?*"))

(define* (copy-attrib! object
                       #:key
                       (anchor #f)
                       (align #f)
                       (angle #f)
                       (string #f)
                       (size #f)
                       (visible? *visible?*)
                       (show #f))
  (let ((new-object (copy-object object)))
    (set-text! object
               (or anchor (text-anchor object))
               (or align (text-align object))
               (or angle (text-angle object))
               (or string (text-string object))
               (or size (text-size object))
               (if (eq? visible? *visible?*)
                   (text-visible? object)
                   visible?)
               (or show (text-attribute-mode object)))))

(define (set-attrib-name! attrib name)
  (set-text-string! attrib
                    (string-append name "=" (attrib-value attrib))))

(define (fix-symbol-attrib object)
  (and (attribute? object)
       (let ((name (attrib-name object))
             (value (attrib-value object)))
         (cond
          ((string-match "slot[0-9]+$" name)
           ;; It is a slot#=# attribute.
           (let ((num (string-drop name 4)))
             (set-attrib-name! object "slotdef")
             (set-attrib-value! object (string-append num ":" value))))
          ((string-match "pin[0-9]+$" name)
           ;; Numbers in name and value of pin#=# attributes are
           ;; always the same, e.g. pin10=10.  So we just change
           ;; attribute name here.
           (set-attrib-name! object "pinnumber")
           ;; Make 'pinseq' attribute.
           (let ((pinseq (copy-attrib! object #:show 'both #:visible? #f))
                 (pin (attrib-attachment object)))
             (set-attrib-name! pinseq "pinseq")
             (attach-attribs! pin pinseq)))
          ((string= name "uref")
           ;; 'uref' => 'refdes'
           (set-attrib-name! object "refdes"))
          ((string= name "type")
           ;; 'type' => 'pintype'
           (set-attrib-name! object "pintype"))
          ((string= name "label")
           ;; 'label' => 'pinlabel'
           (set-attrib-name! object "pinlabel"))
          (else #f)))))

(define (fix-schematic-attrib object)
  (and (attribute? object)
       (let ((name (attrib-name object))
             (value (attrib-value object)))
         (cond
          ((string= name "label")
           ;; 'label' => 'netname'
           (set-attrib-name! object "netname"))
          ((string= name "uref")
           ;; 'uref' => 'refdes'
           (set-attrib-name! object "refdes"))
          (else #f)))))


(define (files-ok? filename backup)
  (let ((msg
         (cond
          ((not (file-exists? filename))
           (format #f "File ~A does not exist.  Skipping.\n" filename))
          ((directory? filename)
           (format #f "~A is a directory, not a file.  Skipping.\n" filename))
          ((file-exists? backup)
           (format #f "Found backup file: ~A.  Skipping ~A\n" backup filename))
          (else #f))))
    (when msg
      (display msg (current-error-port)))
    (not msg)))

(define (filename->fix-func filename)
  (cond
   ((string-suffix-ci? ".sch" filename)
    fix-schematic-attrib)
   ((string-suffix-ci? ".sym" filename)
    fix-symbol-attrib)
   (else (format (current-error-port) "Could not determine the type of file ~S\n" filename)
         (exit 1))))

(define (page->file page filename)
  (with-output-to-file filename
    (lambda () (display (page->string page)))))

(define (update filename)
  (let ((backup (string-append filename ".bak"))
        (fix-func (filename->fix-func filename)))
    (when (files-ok? filename backup)
      (rename-file filename backup)
      (format (current-error-port) "Updating: ~A (backup: ~A)\n" filename backup)

      (let ((page (file->page backup)))
        (for-each fix-func (page-contents page))
        (page->file page filename)))))


(define with-toplevel (@@ (lepton core toplevel) %with-toplevel))
(define make-toplevel (@@ (lepton core toplevel) %make-toplevel))


;;; Main program.
(when (= (length (program-arguments)) 1) (usage))

;;; Parse lepton-schematic command-line options, displaying usage
;;; message or version information as required.
(define (parse-commandline)
  "Parse command line options.  Return the list of non-option
arguments which should represent the list of schematic and symbol
files to process."
  (reverse
   (args-fold
    (cdr (program-arguments))
    (list
     (option '(#\h #\? "help") #f #f
             (lambda (opt name arg seeds)
               (usage)))
     (option '(#\V "version") #f #f
             (lambda (opt name arg seeds)
               (display-lepton-version #:print-name #t #:copyright #t)
               (exit 0))))
    (lambda (opt name arg seeds)
      (format #t
              (G_ "ERROR: Unknown option ~A.
Run `~A --help' for more information.\n")
              (if (char? name)
                  (string-append "-" (char-set->string (char-set name)))
                  (string-append "--" name))
              (basename (car (program-arguments))))
      (exit 1))
    (lambda (op seeds) (cons op seeds))
    '())))


(with-toplevel
 (make-toplevel)
 (lambda ()
   (let ((files (parse-commandline)))
     (for-each update files))))
