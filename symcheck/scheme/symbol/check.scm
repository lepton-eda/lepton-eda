(define-module (symbol check)
  #:use-module ((ice-9 rdelim)
                #:select (read-string)
                #:prefix rdelim:)
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
  #:use-module (symbol check alignment)
  #:use-module (symbol check attrib)
  #:use-module (symbol check connection)
  #:use-module (symbol check obsolete)
  #:use-module (symbol check pin-attrib)
  #:use-module (symbol check slot)
  #:use-module (symbol check text)
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
    objects)

  (check-required-attribs page "refdes" objects)
  (check-required-attribs page "footprint" objects))

;;; Check symbol for connections completely disallowed within it.
(define (check-symbol-connections objects)
  (for-each check-connections objects))

;;; Check symbol for nets or buses completely disallowed within it.
(define (check-symbol-nets-buses objects)
  (for-each check-net/bus objects))

;;; Check for old pin#=# and slot#=# attributes.
(define (check-symbol-obsolete-attribs objects)
  (for-each check-obsolete-attrib objects))

;;; Check symbol slotting attributes.
(define (check-symbol-slots numpins page objects)
  (check-slots page numpins objects))


;;; Check for whether all symbol pins are on grid
(define (check-symbol-pins-on-grid objects)
  (for-each check-pin-alignment
            (filter pin? objects)))


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

(define (check-symbol-device is-graphical page objects)
  (let ((device-list (filter-floating-attribs 'device objects)))
    (if (null? device-list)
        (blame-object page
                      'error
                      (format #f (_ "Missing ~A= attribute\n") 'device))
        (check-device-attribs is-graphical device-list))))


;;; Check if symbol is graphical.
(define (check-symbol-is-graphical? objects)
  (not (null? (filter graphical-attrib? objects))))

(define (check-symbol-text objects)
  (for-each (lambda (object) (check-text-string object))
            objects))

;;; Check symbol attributes
(define (check-symbol-attribs objects)
  (for-each
   (lambda (object)
     (check-text-visibility object)
     (check-attribute object))
   objects))

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

    ; overall symbol structure test
    (check-symbol-attribs objects)

    ; test all text elements
    (check-symbol-text objects)

    ; check for device attribute
    (check-symbol-device (check-symbol-is-graphical? objects)
                         page
                         objects)

    ; check for missing attributes
    (check-symbol-required-attributes page objects)

    ; check for pintype attribute (and multiples) on all pins
    (check-symbol-pintype objects)

    ; check for pinseq attribute (and multiples) on all pins
    (check-symbol-pinseq objects)

    ; check for whether all pins are on grid
    (check-symbol-pins-on-grid objects)

    ; check for pinnumber attribute (and multiples) on all pins
    ; check for slotdef attribute on all pins (if numslots exists)
    (check-symbol-slots (check-symbol-pinnumber page objects)
                        page
                        objects)

    ; check for old pin#=# and slot#=# attributes
    (check-symbol-obsolete-attribs objects)

    ; check  nets or buses within the symbol (completely disallowed)
    (check-symbol-nets-buses objects)

    ; check for connections with in a symbol (completely disallowed)
    (check-symbol-connections objects)

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
