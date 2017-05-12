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
  #:use-module (symbol check obsolete)
  #:use-module (symbol check pin-attrib)
  #:use-module (symbol check primitive)
  #:use-module (symbol check slot)
  #:use-module (ice-9 regex)

  #:export (check-all-symbols))

;;; Check all pintype attributes for all symbol pins.
(define (check-symbol-pintype objects)
  (for-each check-pin-pintype objects))

;;; Check symbol required attributes.
(define (check-symbol-required-attributes page objects)
  (for-each
    (lambda (object)
      (check-pin-required-attribs object "pinlabel")
      (check-pin-required-attribs object "pintype"))
    objects))

;;; Check for old pin#=# and slot#=# attributes.
(define (check-symbol-obsolete-attribs objects)
  (for-each check-obsolete-attrib objects))

;;; Check symbol slotting attributes.
(define (check-symbol-slots numpins page objects)
  (check-slots page numpins objects))

;;; Check symbol pinnumber attribute
(define (check-symbol-pinnumber page objects)
  (let* ((nets (sort (net-numbers objects) string<?))
         (pinnumbers (filter-map check-pin-pinnumber objects))
         (pinnumber-values (sort (map attrib-value pinnumbers) string<?)))

    (check-duplicate-net-pinnumbers page nets)
    (check-attrib-duplicates pinnumbers)
    (check-duplicate-net-pinnumber-numbers page pinnumber-values nets)
    ;; Return pins.
    (filter pin? objects)))

;;; Check symbol pinseq attribute
(define (check-symbol-pinseq objects)
  (check-attrib-duplicates (filter-map check-pin-pinseq objects)))

;;; Checks device= attribute. If schematic symbol is graphical,
;;; also checks for device= value which should be 'none'.  This is
;;; a special check required by some netlister backends.
(define (check-symbol-device is-graphical page objects)
  (let ((device-list (filter-floating-attribs 'device objects)))
    (unless (null? device-list)
      (when is-graphical
        ;; Check for "device=none" for graphical symbols.
        (let ((device (car device-list)))
          (if (string=? (attrib-value device) "none")
              (blame-object device
                            'info
                            (format #f (_ "Found graphical symbol, device=none")))
              (blame-object device
                            'warning
                            (format #f (_"Found graphical symbol, device= should be set to none")))))))))


;;; Check if symbol is graphical.
(define (check-symbol-is-graphical? objects)
  (not (null? (filter graphical-attrib? objects))))

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

    (let ((rest (filter check-primitive objects)))
      (receive (pins attribs)
          (partition pin? rest)

        ;; Check symbol attributes; common checks.
        (for-each check-attribute attribs)

        (receive (floating-attribs attached-attribs)
            (partition floating-attrib? attribs)

          (check-symbol-device (check-symbol-is-graphical? floating-attribs)
                               page
                               floating-attribs)
          ;; Create preliminary symbol structure.
          (attribs->symbol-attribs page floating-attribs))))


    ; check for missing attributes
    (check-symbol-required-attributes page objects)

    ; check for pintype attribute (and multiples) on all pins
    (check-symbol-pintype objects)

    ; check for pinseq attribute (and multiples) on all pins
    (check-symbol-pinseq objects)

    ; check for pinnumber attribute (and multiples) on all pins
    ; check for slotdef attribute on all pins (if numslots exists)
    (check-symbol-slots (check-symbol-pinnumber page objects)
                        page
                        objects)

    ; check for old pin#=# and slot#=# attributes
    (check-symbol-obsolete-attribs objects)

    ;; now report the info/warnings/errors to the user
    (report-blame-statistics `(,page . ,objects))))

;;; Reads file NAME and outputs a page named NAME
(define (file->page name)
  (with-input-from-file name
    (lambda ()
      (unless (symcheck-option-ref 'quiet)
        (log! 'message (_ "Loading schematic ~S") name))
      (string->page name (rdelim:read-string)))))


(define (check-all-symbols)
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
                  (primitive-exit (apply + (map check-symbol pages)))))))))
