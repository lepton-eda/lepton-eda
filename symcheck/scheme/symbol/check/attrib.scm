(define-module (symbol check attrib)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (geda page)
  #:use-module (geda object)
  #:use-module (geda attrib)
  #:use-module (symbol gettext)
  #:use-module (symbol blame)
  #:use-module (symbol check duplicate)
  #:use-module (symbol check obsolete)

  #:export (floating-attrib?
            filter-floating-attribs
            check-attribute
            check-attrib-duplicates
            attribs->symbol-attribs))

(define required-attribs '(refdes footprint device))

(define (floating-attrib? object)
  "Returns #t if OBJECT is attribute and is floating, otherwise
returns #f."
  (and (attribute? object)
       (not (attrib-attachment object))))

(define (filter-floating-attribs name object-list)
  "Filters OBJECT-LIST to contain only attributes named NAME which
must be a symbol."
  (define (floating-attrib-with-name object)
    (and (floating-attrib? object)
         (eq? (string->symbol (attrib-name object)) name)))

  (filter floating-attrib-with-name object-list))

;;; Returns #t if OBJECT is correct graphical= attribute,
;;; otherwise returns #f and blames it.
(define (check-special/graphical object)
  (let ((value (attrib-value object)))
    (or (string=? value "1")
        (and (blame-object object
                           'warning
                           (format #f
                                   (_ "Set 'graphical=1' if you want the symbol to be graphical, current value: ~A")
                                   value))
             #f))))


;;; Checks device= attribute. If schematic symbol is graphical,
;;; also checks for device= value which should be 'none'.  This is
;;; a special check required by some netlister backends
;;; (e.g. spice-sdb).
(define (check-special/device is-graphical device-list)
  (and is-graphical
       device-list
       (let ((device (car device-list)))
         (unless (string=? (attrib-value device) "none")
           ;; Check for "device=none" for graphical symbols.
           (blame-object device
                         'warning
                         (format #f (_"Graphical symbols should have device=none")))))))

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
        (if (or (floating-attrib? object)
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
        (unless (floating-attrib? object)
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


(define (check-floating-attrib-duplicates ls)
  "Checks for duplicated attributes in object list LS."
  (define (blame-duplicate object)
    (blame-object object
                  'error
                  (format #f
                          (_ "Duplicate floating attribute: ~A")
                          (attrib-name object))))
  (unless (null? (cdr ls))
    (for-each blame-duplicate ls))
  (car ls))

(define (check-attrib entry)
  (match entry
    (('graphical . objects)
     `(graphical . ,(check-special/graphical (check-floating-attrib-duplicates objects))))
    (('slotdef . objects)
     `(slotdef . ,objects))
    (('net . objects)
     `(net . ,objects))
    ((name . objects)
     `(,name . ,(check-obsolete-floating-attrib
                 (check-floating-attrib-duplicates objects))))
    (_ (error "Invalid attribute list."))))

(define (attribs->attrib-alist objects)
  (fold
   (lambda (object alist)
     (let* ((name (string->symbol (attrib-name object)))
            (same (assq-ref alist name)))
       (if same
           (assq-set! alist name (cons object same))
           (assq-set! alist name (list object)))))
   ;; Initial empty alist.
   '()
   objects))

(define (check-missing-attribs page alist)
  (define (blame-missing name)
    (unless (assq-ref alist name)
      (blame-object page
                    'warning
                    (format #f
                            (_ "Missing required attribute: ~A")
                            name))))

  (for-each blame-missing required-attribs))

(define (attribs->symbol-attribs page floating-attribs)
  "Forms symbol attribute list from objects in the list
FLOATING-ATTRIBS."
  (let ((attrib-alist (attribs->attrib-alist floating-attribs)))
    (check-missing-attribs page attrib-alist)
    (let ((output-alist (map check-attrib attrib-alist)))
      (check-special/device (assq-ref output-alist 'graphical)
                            (assq-ref attrib-alist 'device))
      output-alist)))
