(define-module (symbol check pin-attrib)
  #:use-module (srfi srfi-1)
  #:use-module (geda object)
  #:use-module (geda attrib)
  #:use-module (symbol gettext)
  #:use-module (symbol blame)
  #:use-module (symbol check attrib)

  #:export (check-pin-pintype
            check-pin-pinseq
            check-pin-pinnumber
            check-pin-required-attribs
            pin-attrib?
            pin-attribs
            net-numbers
            check-duplicate-net-pinnumbers
            check-duplicate-net-pinnumber-numbers
            check-old-pin))

(define (pin-attrib? object)
  "Returns #t if OBJECT is a pin attribute, otherwise
returns #f."
  (and (attribute? object)
       (and=> (attrib-attachment object) pin?)))

(define %valid-pintype-values
  '(in out io oc oe pas tp tri clk pwr))

(define (check-pin-pintype object)
  "Checks attributes 'pintype' of pin OBJECT."
  (define (found-attrib name value)
    (format #f (_ "Found ~A=~A attribute\n") name value))

  (define (invalid-attrib name value)
    (format #f (_ "Invalid ~A=~A attribute\n") name value))

  (and (pin? object)
       (for-each (lambda (attrib)
                   (when (string=? "pintype" (attrib-name attrib))
                     (let ((value (attrib-value attrib)))
                       (blame-object object
                                     'info
                                     (found-attrib 'pintype value))
                       (unless (memq (string->symbol value) %valid-pintype-values)
                         (blame-object object
                                       'error
                                       (invalid-attrib 'pintype value))))))
                 (object-attribs object))))

(define (pin-attribs pin name)
  "Returns all attributes of PIN with given NAME which must be a
symbol."
  (and (pin? pin)
       (filter (lambda (obj)
                 (eq? (string->symbol (attrib-name obj)) name))
               (object-attribs pin))))

(define (check-attrib-value attrib)
  (not (string=? (attrib-value attrib) "0")))


;;; Returns OBJECT's attribute pinseq= if it is valid, otherwise returns #f.
(define (check-pin-attrib object attr-name)
  (and (pin? object)
       (let ((attrib-list (pin-attribs object attr-name)))

         (if (null? attrib-list)
             (begin
               (blame-object object
                             'error
                             (format #f (_ "Missing ~A= attribute\n") attr-name))
               #f)
             (if (null? (cdr attrib-list))
                 (let ((attrib (car attrib-list)))
                   (if (check-attrib-value attrib)
                       attrib
                       (begin (blame-object attrib
                                            'error
                                            (format #f
                                                    (_ "Found ~A=~A attribute\n")
                                                    (attrib-name attrib)
                                                    (attrib-value attrib)))
                              #f)))
                 (begin
                   (blame-object object
                                 'error
                                 (format #f
                                         (_ "Found multiple ~A= attributes on one pin\n")
                                         attr-name))
                   #f))))))

(define (check-pin-pinseq pin)
  (check-pin-attrib pin 'pinseq))

(define (check-pin-pinnumber pin)
  (check-pin-attrib pin 'pinnumber))

(define (check-pin-required-attribs object attr-name)
  "Checks pin required attributes ATTR-NAME of pin OBJECT."
  (define (filter-attrib object)
    (and (string=? (attrib-name object) attr-name)
         object))

  (and (pin? object)
       (let ((attrib-list (filter filter-attrib (object-attribs object))))
         (if (null? attrib-list)
             (blame-object object
                              'warning
                              (format #f (_ "Missing ~A= attribute\n") attr-name))
             (unless (null? (cdr attrib-list))
               (blame-object object
                             'error (format #f
                                            (_"Found multiple ~A=~A attributes on one pin\n")
                                            attr-name
                                            (attrib-value (car
                                                           ;; reverse attributes for backward
                                                           ;; compatibility
                                                           (reverse attrib-list))))))))))

;;; Collect all net= pin numbers.
(define (net-numbers objects)
  (define (net-attrib? object)
    (and (floating-attrib? object)
         (string=? (attrib-name object) "net")))

  (define (get-net-attributes ls)
    (filter net-attrib? ls))

  (define (add-net-pin-attribute net-attrib)
    (let ((net (attrib-value net-attrib)))
      (blame-object net-attrib 'info (format #f (_ "Found [~A=~A]\n") 'net net))
      (let ((net-tokens (string-split net #\:)))
        ;; length of net tokens has to be 2
        (if (not (= 2 (length net-tokens)))
            (begin (blame-object net-attrib
                                 'error
                                 (format #f
                                         (_ "Bad ~A= attribute [~A]\n")
                                         'net
                                         net))
                   '())
            (let ((pin-tokens (string-split (cadr net-tokens) #\,)))
              (for-each
               (lambda (pin)
                 (blame-object net-attrib
                               'info
                               (format #f
                                       (_ "Found pin number ~A=~A in net attribute\n")
                                       'pinnumber
                                       pin)))
               pin-tokens)
              pin-tokens)))))

  (append-map add-net-pin-attribute
              (get-net-attributes objects)))


(define (check-duplicate-net-pinnumbers page ls)
  "Checks list of pin numbers LS (typically obtained from net=
attributes) for duplicates and reports errors for PAGE."
  (and (not (null? ls))
       (not (null? (cdr ls)))
       (if (string=? (car ls) (cadr ls))
           (blame-object page
                         'error
                         (format #f
                                 (_ "Found duplicate pin in net= attributes [~A]\n")
                                 (car ls))))
       (if (string=? (car ls) "0")
           (blame-object page
                         'error
                         (format #f
                                 (_ "Found pinnumber ~A=~A in net= attribute\n")
                                 'pinnumber
                                 0)))
       (check-duplicate-net-pinnumbers page (cdr ls))))

(define (check-duplicate-net-pinnumber-numbers page pins nets)
  "Compares sorted pin number lists PINS and NETS taken
accordingly from pinnumber= and net= attributes and reports errors
for PAGE if some numbers match."
  (and (not (null? pins))
       (not (null? nets))
       (let ((pin (car pins))
             (net (car nets)))
         (if (string=? pin net)
             (and (blame-object page
                                'warning
                                (format #f (_ "Found the same number in a pinnumber attribute and in a net attribute [~A]\n") pin))
                  (check-duplicate-net-pinnumber-numbers page (cdr pins) (cdr nets)))
             (if (string>? pin net)
                 (check-duplicate-net-pinnumber-numbers page pins (cdr nets))
                 (check-duplicate-net-pinnumber-numbers page (cdr pins) nets))))))
