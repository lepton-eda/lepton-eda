(define-module (symcheck check)
  #:use-module ((ice-9 rdelim)
                #:select (read-string)
                #:prefix rdelim:)
  #:use-module (geda log)
  #:use-module (geda page)
  #:use-module (geda repl)
  #:use-module (symcheck option)
  #:use-module (symbol blame)
  #:use-module (symbol check)
  #:use-module (symbol check log)
  #:use-module (symbol gettext)

  #:export (check-all-symbols))

(define (usage)
  (format #t
          (_ "Usage: ~A [OPTIONS] FILENAME ...
  -h, --help        Print usage
  -q, --quiet       Quiet mode
  -v, --verbose     Verbose mode (cumulative: errors, warnings, info)
                    Use this to get the actual symbol error messages
FILENAME ... are the symbols to check.
")
          (car (program-arguments)))
  (primitive-exit 0))


;;; Reads file NAME and outputs a page named NAME
(define (file->page name)
  (with-input-from-file name
    (lambda ()
      (unless (symcheck-option-ref 'quiet)
        (log! 'message (_ "Loading schematic ~S") name))
      (string->page name (rdelim:read-string)))))


(define (check-all-symbols)
  (define (report-symbol-statistics page)
    (unless (symcheck-option-ref 'quiet)
      (check-log! 'message (_ "Checking: ~A\n") (page-filename page)))
    (check-symbol page)
    (blame-statistics `(,page . ,(page-contents page))))

  ;; Symcheck logs to stdout by default.
  (set-check-log-destination! 'stdout)

  (let ((files (symcheck-option-ref '()))
        (help (symcheck-option-ref 'help))
        (interactive (symcheck-option-ref 'interactive)))
    (if help
        (usage)
        (if (null? files)
            (error (format #f
                           (_ "No schematic files specified for processing.
Run `~A --help' for more information.
")
                           (car (program-arguments))))
            (let ((pages (map file->page files)))
              (if interactive
                  (lepton-repl)

                  ;; now report the info/warnings/errors to the user
                  (primitive-exit (apply + (map report-symbol-statistics pages)))))))))
