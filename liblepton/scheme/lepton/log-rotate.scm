;; Lepton EDA library - Scheme API
;; Copyright (C) 2016 Peter Brett <peter@peter-b.co.uk>
;; Copyright (C) 2017-2020 Lepton EDA Contributors
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
;;

(define-module (lepton log-rotate)
  #:use-module (lepton os)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 hash-table)
  #:export (cleanup-old-logs!))


;; Log horizon in seconds.
(define LOG-THRESHOLD (* 24 60 60))
;; Maximum number of files to preserve per tool that are older than
;; LOG-THRESHOLD seconds old
(define LOG-COUNT-OUTSIDE 50)
;; Maximum number of files to preserve per tool that are newer than
;; LOG-THRESHOLD seconds old
(define LOG-COUNT-INSIDE 250)
;; Probability that a clean-up will be run (it's sort of costly so
;; probably don't want to run it too often)
(define LOG-ROTATE-PROBABILITY (/ 1 25))

;;;; get-log-directory
;;
;; Returns the current user's log directory.
;;
;; FIXME This should be obtained from the code that decides where to
;; create and populate log files.
(define (log-directory)
  (string-append (user-cache-dir) file-name-separator-string "logs"))

;;;; get-log-toolname path
;;
;; Return the tool name for the log file at path, or #f if the path
;; doesn't look like a log file.
;;
;; FIXME This should also come from the code that decides how log
;; files are named.
(define logpath->toolname
  (let ((log-regexp (make-regexp "(.*)-[0-9]+-[0-9]+.log$")))
    (lambda (logpath)
      (let* ((name (basename logpath))
             (match (regexp-exec log-regexp name)))
        (and match (match:substring match 1))))))

;;;; fold-files leaf-proc initial-value path
;;
;; Folds leaf-proc over all the files found within path, starting with
;; initial-value.
(define (fold-files leaf-proc initial-value path)
  (define (enter? path stat result) #t)
  (define (id path stat result) result)
  (define (error path stat errno result) result)
  (file-system-fold enter? leaf-proc id id id error
                    initial-value
                    path))

;;;; log-files-by-tool
;;
;; Return a list of log files, grouped by tool and sorted by
;; modification time.
(define (log-files-by-tool)

  ;; Use with fold-files to build a hashtable where each key is a
  ;; tool name (e.g. "lepton-schematic") and each value is a list
  ;; of log files for that tool.
  (define (build-log-hash-table path stat table)
    (let ((tool (logpath->toolname path))
          (info (list path (stat:mtime stat))))

      ;; If this looks like a log file, add it to the tool's list of log
      ;; files, creating a new one if appropriate
      (if tool
          (hash-set! table tool
                     (cons info (hash-ref table tool '()))))

      ;; Pass the table on
      table))

  (define (mtime<? a b)
    (< (cadr a) (cadr b)))

  (define (hash-unpack key value)
    (list key
          (sort value mtime<?)))

  (let* ((tool-table (make-hash-table)))

    ;; Call fold-files for side-effects
    (fold-files build-log-hash-table
                tool-table
                (log-directory))

    ;; Unpack into list-based structure, sorting the lists of log
    ;; files.
    (hash-map->list hash-unpack tool-table)))

;;;; files-to-delete lst
;;
;; Given a list where each element is in the form (file mtime), return
;; a new list containing paths that should be deleted.  The lst should
;; be sorted by mtime (ascending).
(define (files-to-delete log-files)

  (define mtime-age
    (let ((now (current-time)))
      (lambda (mtime) (- now mtime))))

  (define (collect-deletable item result)
    (let* ((path (car item))
           (mtime (cadr item))
           (age (mtime-age mtime))
           (count (car result))
           (deletable (cadr result)))
      (cond
       ;; The file is inside the time threshold and there are
       ;; sufficiently few new files for it to be kept.
       ((and (< count LOG-COUNT-INSIDE)
             (< age LOG-THRESHOLD))
        (list (1+ count) deletable))

       ;; There are sufficiently few old files for it to be kept.
       ((and (< count LOG-COUNT-OUTSIDE))
        (list (1+ count) deletable))

       ;; The file should be deleted
       (else
        (list count (cons path deletable))))))

  (cadr
   (fold-right collect-deletable '(0 ()) log-files)))

;; Delete old logs
(define (cleanup-old-logs-impl!)

  (define (cleanup-old-tool-logs! tool-logs)
    (for-each delete-file (files-to-delete tool-logs)))

  (for-each (compose cleanup-old-tool-logs! cadr)
            (log-files-by-tool)))


; public:
; Clean up old log files
;
( define ( cleanup-old-logs! )
( let*
  (
  ( state ( random-state-from-platform ) )
  ( theta ( random:uniform state ) )
  )

  ( if ( < theta LOG-ROTATE-PROBABILITY )
    ( cleanup-old-logs-impl! )
  )

) ; let
) ; cleanup-old-logs!()
