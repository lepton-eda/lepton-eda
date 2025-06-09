;;; Lepton EDA Symbol Checker
;;; Scheme API
;;; Copyright (C) 2017-2022 Lepton EDA Contributors
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

(define-module (symcheck report)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)

  #:use-module (lepton gettext)
  #:use-module (symbol blame)
  #:use-module (symbol check log)
  #:use-module (symcheck option)

  #:export (check-report))

(define (quiet) (symcheck-option-ref 'quiet))
(define (verbose) (if (quiet) -1 (symcheck-option-ref-length 'verbose)))

(define (report-blame object)
  "Reports errors (blames) collected for OBJECT."
  (define (output-to-log?)
    (eq? (current-check-log-destination) 'log))

  (define (report blame)
    (match blame
      (('info . msg)
       (when (or (> (verbose) 2) (output-to-log?))
         (check-log! 'info (format #f (G_ "Info: ~A") msg)))
       '(1 0 0 0))
      (('warning . msg)
       (when (or (> (verbose) 1) (output-to-log?))
         (check-log! 'warning (format #f (G_ "Warning: ~A") msg)))
       '(0 1 0 0))
      (('error . msg)
       (when (or (> (verbose) 0) (output-to-log?))
         (check-log! 'critical (format #f (G_ "ERROR: ~A") msg)))
       '(0 0 1 0))
      (_
       (check-log! 'critical (format #f (G_ "Unrecognized info: ~A\n") blame))
       '(0 0 0 1))))

  (map report (or (object-blames object) '())))

(define (report-blames objects)
  "Reports object errors (blames) and returns their count as a
list of the form:
  (info-count warning-count error-count unrecognized-count)"
  (match (append-map report-blame objects)
    (((info warning error unrecognized) ...)
     `(,(apply + info)
       ,(apply + warning)
       ,(apply + error)
       ,(apply + unrecognized)))))

(define (check-report objects)
  "Reports check statistics for given list of OBJECTS."
  (define (report-statistics info-count
                             warning-count
                             error-count
                             unrecognized-count)

    (unless (quiet)
      (unless (zero? warning-count)
        (check-log! 'message (N_ "~A warning found"
                                 "~A warnings found"
                                 warning-count)
                    warning-count)
        (when (< (verbose) 2)
          (check-log! 'message (G_ "(use -vv to view details)"))))

      (if (zero? error-count)
          (check-log! 'message (G_ "No errors found"))
          (begin
            (check-log! 'message (N_ "~A ERROR found"
                                     "~A ERRORS found"
                                     error-count)
                        error-count)
            (when (< (verbose) 1)
              (check-log! 'message (G_ "(use -v to view details)"))))))

    ;; return code
    (if (zero? error-count)
        (if (zero? warning-count) 0 1)
        2))

  (apply report-statistics (report-blames objects)))
