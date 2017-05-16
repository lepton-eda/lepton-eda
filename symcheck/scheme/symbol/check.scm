(define-module (symbol check)
  #:use-module ((ice-9 rdelim)
                #:select (read-string)
                #:prefix rdelim:)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (symbol gettext)
  #:use-module (symcheck option)
  #:use-module (symbol blame)
  #:use-module (geda page)
  #:use-module (geda log)
  #:use-module (geda object)
  #:use-module (geda attrib)
  #:use-module (geda repl)
  #:use-module (symbol check attrib)
  #:use-module (symbol check pin)
  #:use-module (symbol check pin-attrib)
  #:use-module (symbol check primitive)
  #:use-module (symbol check slot)
  #:use-module (ice-9 regex)

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

(define-public (check-symbol page)

  (let ((quiet (symcheck-option-ref 'quiet))
        (verbose (symcheck-option-ref-length 'verbose))
        (objects (page-contents page)))

    (when (not quiet)
      (log! 'message (_ "Checking: ~A\n") (page-filename page)))

    (let ((rest (filter-map check-primitive objects)))
      (receive (pins attribs)
          (partition symbol-pin? rest)

        ;; Check symbol attributes; common checks.
        (for-each check-attribute attribs)

        (receive (floating-attribs attached-attribs)
            (partition floating-attrib? attribs)

          ;; Create preliminary symbol structure.
          (attribs->symbol-attribs page floating-attribs)

          ;; Check pinseq attributes.
          (check-duplicates/pinseq pins)
          (check-duplicates/pinnumber pins)

          ;; Check symbol pinnumber attribute
          (let* ((nets (sort (net-numbers objects) string<?))
                 (pinnumbers (filter-map check-pin-pinnumber objects))
                 (pinnumber-values (sort (map attrib-value pinnumbers) string<?)))

            (check-duplicate-net-pinnumbers page nets)
            (check-duplicate-net-pinnumber-numbers page pinnumber-values nets))

          ;; Check symbol slotting attributes.
          (check-slots page pins objects))))))

;;; Reads file NAME and outputs a page named NAME
(define (file->page name)
  (with-input-from-file name
    (lambda ()
      (unless (symcheck-option-ref 'quiet)
        (log! 'message (_ "Loading schematic ~S") name))
      (string->page name (rdelim:read-string)))))


(define (check-all-symbols)
  (define (report-symbol-statistics page)
    (check-symbol page)
    (report-blame-statistics `(,page . ,(page-contents page))))

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
