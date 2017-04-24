(define-module (symbol check)

  ; Import C procedures and variables
  #:use-module (symbol core check)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (symbol gettext)
  #:use-module (symbol blame)
  #:use-module (geda page)
  #:use-module (geda log)
  #:use-module (geda object)
  #:use-module (geda attrib)
  #:use-module (symbol check alignment)
  #:use-module (symbol check attrib)
  #:use-module (symbol check connection)
  #:use-module (symbol check obsolete)
  #:use-module (symbol check pin-attrib)
  #:use-module (symbol check slot)
  #:use-module (symbol check text)
  #:use-module (ice-9 regex))

(define-public (check-all-symbols)
  (apply + (map check-symbol (active-pages))))

;;; Check all pintype attributes for all symbol pins.
(define-public (check-symbol-pintype page)
  (for-each check-pin-pintype (page-contents page)))

;;; Check symbol required attributes.
(define-public (check-symbol-required-attributes page)
  (for-each
    (lambda (object)
      (check-pin-required-attribs object "pinlabel")
      (check-pin-required-attribs object "pintype"))
    (page-contents page))

  (check-required-attribs page "refdes")
  (check-required-attribs page "footprint"))

;;; Check symbol for connections completely disallowed within it.
(define (check-symbol-connections page)
  (for-each check-connections (page-contents page)))

;;; Check symbol for nets or buses completely disallowed within it.
(define (check-symbol-nets-buses page)
  (for-each check-net/bus (page-contents page)))

;;; Check for old pin#=# and slot#=# attributes.
(define (check-symbol-obsolete-attribs page)
  (for-each check-obsolete-attrib (page-contents page)))

;;; Check symbol slotting attributes.
(define (check-symbol-slots numpins page)
  (check-slots page numpins (page-contents page)))


;;; Check for whether all symbol pins are on grid
(define (check-symbol-pins-on-grid page)
  (for-each check-pin-alignment
            (filter pin? (page-contents page))))


;;; Check symbol pinnumber attribute
(define (check-symbol-pinnumber page)
  (let* ((objects (page-contents page))
         (nets (sort (net-numbers objects) string<?))
         (pinnumbers (filter-map check-pin-pinnumber objects))
         (pinnumber-values (sort (map attrib-value pinnumbers) string<?)))

    (check-duplicate-net-pinnumbers page nets)
    (check-attrib-duplicates pinnumbers)
    (check-duplicate-net-pinnumber-numbers page pinnumber-values nets)
    ;; Return pins.
    (filter pin? objects)))

;;; Check symbol pinseq attribute
(define (check-symbol-pinseq page)
  (check-attrib-duplicates (filter-map check-pin-pinseq (page-contents page))))

(define-public (check-symbol-device is-graphical page)
  (let ((device-list (filter-floating-attribs 'device (page-contents page))))
    (if (null? device-list)
        (blame-object page
                      'error
                      (format #f (_ "Missing ~A= attribute\n") 'device))
        (check-device-attribs is-graphical device-list))))


;;; Check if symbol is graphical.
(define-public (check-symbol-is-graphical? page)
  (not (null? (filter graphical-attrib? (page-contents page)))))

(define-public (check-symbol-text page)
  (for-each (lambda (object) (check-text-string object))
            (page-contents page)))

;;; Check symbol attributes
(define-public (check-symbol-attribs page)
  (for-each
   (lambda (object)
     (check-text-visibility object)
     (check-attribute object))
   (page-contents page)))


(define-public (check-symbol page)

  (let ((quiet (%check-get-quiet-mode))
        (verbose (%check-get-verbose-mode)))

    (when (not quiet)
      (log! 'message (_ "Checking: ~A\n") (page-filename page)))

    ; overall symbol structure test
    (check-symbol-attribs page)

    ; test all text elements
    (check-symbol-text page)

    ; check for device attribute
    (check-symbol-device (check-symbol-is-graphical? page) page)

    ; check for missing attributes
    (check-symbol-required-attributes page)

    ; check for pintype attribute (and multiples) on all pins
    (check-symbol-pintype page)

    ; check for pinseq attribute (and multiples) on all pins
    (check-symbol-pinseq page)

    ; check for whether all pins are on grid
    (check-symbol-pins-on-grid page)

    ; check for pinnumber attribute (and multiples) on all pins
    ; check for slotdef attribute on all pins (if numslots exists)
    (check-symbol-slots (check-symbol-pinnumber page) page)

    ; check for old pin#=# and slot#=# attributes
    (check-symbol-obsolete-attribs page)

    ; check for nets or buses within the symbol (completely disallowed)
    (check-symbol-nets-buses page)

    ; check for connections with in a symbol (completely disallowed)
    (check-symbol-connections page)

    ;; now report the info/warnings/errors to the user
    (when (not quiet)
      ;; done, now print out the messages
      (apply report-statistics
             (report-blames `(,page . ,(page-contents page)))))

    ))

(define (report-statistics info-count
                           warning-count
                           error-count
                           unrecognized-count)

  (let ((verbose (%check-get-verbose-mode)))

    (unless (zero? warning-count)
      (log! 'message (N_ "~A warning found"
                         "~A warnings found"
                         warning-count)
            warning-count)
      (when (< verbose 2)
        (log! 'message (_ "(use -vv to view details)"))))

    (if (zero? error-count)
        (log! 'message (_ "No errors found"))
        (begin
          (log! 'message (N_ "~A ERROR found"
                             "~A ERRORS found"
                             error-count)
                error-count)
          (when (< verbose 1)
            (log! 'message (_ "(use -v to view details)")))))

    (primitive-exit
     ;; return code
     (if (zero? error-count)
         (if (zero? warning-count) 0 1)
         2))))
