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
  #:use-module (symbol check attrib)
  #:use-module (symbol check pin-attrib)
  #:use-module (symbol check text))

(define-public (check-all-symbols)
  (apply + (map check-symbol (active-pages))))

;;; Check all pintype attributes for all symbol pins.
(define-public (check-symbol-pintype page)
  (for-each check-pin-pintype (page-contents page)))

(define-public check-symbol-missing-attribute %check-symbol-missing-attribute)

(define-public check-symbol-missing-attributes %check-symbol-missing-attributes)

(define-public check-symbol-connections %check-symbol-connections)

(define-public check-symbol-nets-buses %check-symbol-nets-buses)

(define-public check-symbol-oldslot %check-symbol-oldslot)

(define-public check-symbol-oldpin %check-symbol-oldpin)

(define-public check-symbol-slotdef %check-symbol-slotdef)

(define-public check-symbol-pins-on-grid %check-symbol-pins-on-grid)

(define-public check-symbol-pinnumber %check-symbol-pinnumber)

(define-public check-symbol-pinseq %check-symbol-pinseq)

(define-public check-symbol-device %check-symbol-device)

(define-public check-symbol-graphical %check-symbol-graphical)

(define-public check-symbol-text %check-symbol-text)

(define symbol-check-glist-append %symbol-check-glist-append)


;;; Check symbol attributes
(define-public (check-symbol-attribs page)
  (for-each
   (lambda (object)
     (check-text-visibility object)
     (check-attribute object))
   (page-contents page)))


(define check-info-messages %check-info-messages)
(define check-warning-messages %check-warning-messages)
(define check-error-messages %check-error-messages)

; Print out results of symbol check
(define-public (check-symbol-output-results)
  (let ((verbose (%check-get-verbose-mode)))

    (if (> verbose 2)
      (for-each (cut log! 'message (_ "Info: ~A") <>)
                (check-info-messages)))

    (if (> verbose 1)
      (for-each (cut log! 'message (_ "Warning: ~A") <>)
                (check-warning-messages)))

    (if (> verbose 0)
      (for-each (cut log! 'message (_ "ERROR: ~A") <>)
                (check-error-messages)))))

(define-public (check-symbol page)

  (let ((quiet (%check-get-quiet-mode))
        (verbose (%check-get-verbose-mode)))

    (when (not quiet)
      (log! 'message (_ "Checking: ~A\n") (page-filename page)))

    ; overall symbol structure test
    (check-symbol-attribs page)

    ; test all text elements
    (check-symbol-text page)

    ; check for graphical attribute
    (check-symbol-graphical page)

    ; check for device attribute
    (check-symbol-device page)

    ; check for missing attributes
    (check-symbol-missing-attributes page)

    ; check for pintype attribute (and multiples) on all pins
    (check-symbol-pintype page)

    ; check for pinseq attribute (and multiples) on all pins
    (check-symbol-pinseq page)

    ; check for pinnumber attribute (and multiples) on all pins
    (check-symbol-pinnumber page)

    ; check for whether all pins are on grid
    (check-symbol-pins-on-grid page)

    ; check for slotdef attribute on all pins (if numslots exists)
    (check-symbol-slotdef page)

    ; check for old pin#=# attributes
    (check-symbol-oldpin page)

    ; check for old slot#=# attributes
    (check-symbol-oldslot page)

    ; check for nets or buses within the symbol (completely disallowed)
    (check-symbol-nets-buses page)

    ; check for connections with in a symbol (completely disallowed)
    (check-symbol-connections page)

    (let ((error-count   (length (check-error-messages)))
          (warning-count (length (check-warning-messages))))
      ; now report the info/warnings/errors to the user
      (when (not quiet)
        ;; done, now print out the messages
        (check-symbol-output-results)
        (apply report-statistics (map +
                                      `(0 ,warning-count ,error-count 0)
                                      (report-blames (page-contents page)))))


    ; return code
    (if (not (zero? error-count))
        2
        (if (not (zero? warning-count)) 1 0)))))

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
            (log! 'message (_ "(use -v to view details)")))))))
