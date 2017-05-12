(define-module (symbol check attrib)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (geda page)
  #:use-module (geda object)
  #:use-module (geda attrib)
  #:use-module (symbol gettext)
  #:use-module (symbol blame)
  #:use-module (symbol check duplicate)

  #:export (floating-attrib?
            filter-floating-attribs
            graphical-attrib?
            check-attribute
            check-required-attribs
            check-attrib-duplicates
            attribs->symbol-attribs))

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

(define (valid-graphical? object)
  (string=? (attrib-value object) "1"))

(define (graphical-attrib? object)
  "Checks if object is attribute 'graphical=1'."
  (and (floating-attrib? object)
       (string=? (attrib-name object) "graphical")
       (valid-graphical? object)))


(define (check-required-attribs page attr-name objects)
  "Checks required toplevel attributes ATTR-NAME on PAGE."
  (define (filter-attrib object)
    (and (attribute? object)
         (string=? (attrib-name object) attr-name)
         (blame-object object
                       'info
                       (format #f (_ "Found ~A=~A\n") attr-name (text-string object)))
         object))

  (let ((attrib-list (filter filter-attrib objects)))
    (if (null? attrib-list)
        (blame-object page
                      'warning
                      (format #f (_ "Missing ~A= attribute\n") attr-name)))))

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
     `(graphical ,(valid-graphical? (check-floating-attrib-duplicates objects))))
    (('slotdef . objects)
     `(slotdef . ,objects))
    (('net . objects)
     `(net . ,objects))
    ((name . objects)
     `(,name . ,(check-floating-attrib-duplicates objects)))
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

(define (attribs->symbol-attribs floating-attribs)
  "Forms symbol attribute list from objects in the list
FLOATING-ATTRIBS."
  (map check-attrib (attribs->attrib-alist floating-attribs)))
