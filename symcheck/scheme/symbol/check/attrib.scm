(define-module (symbol check attrib)
  #:use-module (srfi srfi-1)
  #:use-module (geda page)
  #:use-module (geda object)
  #:use-module (geda attrib)
  #:use-module (symbol gettext)
  #:use-module (symbol blame)
  #:use-module (symbol check duplicate)

  #:export (filter-floating-attribs
            graphical-attrib?
            check-attribute
            check-device-attribs
            check-required-attribs
            check-attrib-duplicates))

(define (filter-floating-attribs name object-list)
  "Filters OBJECT-LIST to contain only attributes named NAME which
must be a symbol."
  (define (floating-attrib-with-name object)
    (and (attribute? object)
         (not (attrib-attachment object)) ; floating
         (eq? (string->symbol (attrib-name object)) name)))

  (filter floating-attrib-with-name object-list))

(define (graphical-attrib? object)
  "Checks if object is attribute 'graphical=1'."
  (and (attribute? object)
       (not (attrib-attachment object)) ; floating
       (string=? (attrib-name object) "graphical")
       (string=? (attrib-value object) "1")))

(define (check-device-attribs is-graphical? device-list)
  "Checks device= attributes in DEVICE-LIST. If the list contains
more than one attribute, adds error on that. If schematic symbol
is graphical, that is, IS-GRAPHICAL? is #t, also checks for
device= value which should be 'none' for graphical symbols."
  (if (null? (cdr device-list))
      (let* ((device (car device-list))
             (value (attrib-value device)))
        (blame-object device 'info (format #f (_ "Found ~A=~A\n") 'device value))
        (when is-graphical?
          ;; Check for "device=none" for graphical symbols.
          (if (string=? value "none")
              (blame-object device 'info
                            (format #f
                                    (_ "Found graphical symbol, ~A=~A\n")
                                    'device
                                    value))
              (blame-object device 'warning
                            (format #f
                                    (_"Found graphical symbol, device= should be set to none\n"))))))
      (for-each (lambda (object)
                  (blame-object object
                                'error
                                (format #f (_ "Conflicting attribute: ~A.\n") 'device)))
                device-list)))

(define (check-required-attribs page attr-name)
  "Checks required toplevel attributes ATTR-NAME on PAGE."
  (define (filter-attrib object)
    (and (attribute? object)
         (string=? (attrib-name object) attr-name)
         (blame-object object
                       'info
                       (format #f (_ "Found ~A=~A\n") attr-name (text-string object)))
         object))

  (let ((attrib-list (filter filter-attrib (page-contents page))))
    (if (null? attrib-list)
        (blame-object page
                      'warning
                      (format #f (_ "Missing ~A= attribute\n") attr-name))
        (unless (null? (cdr attrib-list))
          (blame-object page
                        'error
                        (format #f (_ "Multiple ~A= attributes found\n") attr-name))))))

(define (check-attribute object)
  "Checks attribute OBJECT."
  (and (attribute? object)
   (let ((aname  (string->symbol (attrib-name object)))
         (avalue (attrib-value object)))

     (case aname
       ((type name)
        (blame-object object
                      'error
                      (format #f
                              (_ "Found forbidden ~A= attribute: [~A=~A]\n")
                              aname
                              aname
                              avalue)))

       ((uref label email)
        (blame-object object
                      'warning
                      (format #f
                              (_ "Found obsolete ~A= attribute: [~A=~A]\n")
                              aname
                              aname
                              avalue)))

       ;; Valid pin attributes.
       ((pinlabel pintype pinseq pinnumber)
        (if (or (not (attrib-attachment object))
                (not (pin? (attrib-attachment object))))
            (blame-object object
                          'error
                          (format #f
                                  (_ "Found misplaced pin attribute: [~A=~A]\n")
                                  aname
                                  avalue))))

       ;; Valid attributes.
       ((device graphical description author
                comment numslots slotdef footprint
                documentation refdes slot net
                value symversion dist-license use-license)
        ;; Check if they are floating (not attached to anything).
        (if (attrib-attachment object)
            (blame-object object
                          'error
                          (format #f
                                  (_ "Found wrongly attached attribute: [~A=~A]\n")
                                  aname
                                  avalue))))

       ;; All other attributes are unknown.
       (else (blame-object object
                           'warning
                           (format #f
                                   (_ "Found unknown ~A= attribute: [~A=~A]\n")
                                   aname
                                   aname
                                   avalue)))))))


;;; Sorts attrib list LS and transforms it into a list where
;;; attributes with duplicated values are gathered together into
;;; sublists.
(define (attrib-duplicates ls)
  (define (attrib-value<? a b)
    (string<? (attrib-value a) (attrib-value b)))

  (define (attrib-value=? a b)
    (string=? (attrib-value a) (attrib-value b)))

  (list->duplicate-list ls attrib-value<? attrib-value=?))


(define (check-attrib-duplicates ls)
  "Checks for duplicated attributes in LS."
  (define (blame-duplicate object)
    (blame-object object
                  'error
                  (format #f
                          (_ "Found duplicate ~A=~A attribute in the symbol\n")
                          (attrib-name object)
                          (attrib-value object))))
  (define (blame-if-list ls)
    (when (list? ls)
      (for-each blame-duplicate ls)))

  (for-each blame-if-list (attrib-duplicates ls)))
