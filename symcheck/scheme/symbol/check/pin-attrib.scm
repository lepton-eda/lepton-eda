(define-module (symbol check pin-attrib)
  #:use-module (srfi srfi-1)
  #:use-module (geda object)
  #:use-module (geda attrib)
  #:use-module (symbol gettext)
  #:use-module (symbol blame)
  #:use-module (symbol check attrib)
  #:use-module (symbol check duplicate)
  #:use-module (symbol check pin)

  #:export (check-pin-pinnumber
            pin-attrib?
            pin-attribs
            net-numbers
            check-duplicate-net-pinnumbers
            check-duplicate-net-pinnumber-numbers
            check-pin-attrib-duplicates
            check-duplicates/pinseq
            check-duplicates/pinnumber))

(define (pin-attrib? object)
  "Returns #t if OBJECT is a pin attribute, otherwise
returns #f."
  (and (attribute? object)
       (and=> (attrib-attachment object) pin?)))


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
             #f
             (let ((attrib (car attrib-list)))
               (and (check-attrib-value attrib)
                    attrib))))))

(define (check-pin-pinnumber pin)
  (check-pin-attrib pin 'pinnumber))

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


(define (pinseq<? a b)
  (string<? (symbol-pin-seq a) (symbol-pin-seq b)))

(define (pinseq=? a b)
  (string=? (symbol-pin-seq a) (symbol-pin-seq b)))

(define (pinnumber<? a b)
  (string<? (symbol-pin-number a) (symbol-pin-number b)))

(define (pinnumber=? a b)
  (string=? (symbol-pin-number a) (symbol-pin-number b)))

(define (check-pin-attrib-duplicates name getter less-func equal-func symbol-pins)
  "Checks for duplicated attributes in LS."
  (define (blame-duplicate symbol-pin)
    (blame-object (symbol-pin-object symbol-pin)
                  'error
                  (format #f
                          (_ "Duplicate pin attribute in the symbol: ~A=~A")
                          name
                          (getter symbol-pin))))
  (define (blame-if-list ls)
    (when (list? ls)
      (for-each blame-duplicate ls)))

  (for-each blame-if-list
            (list->duplicate-list (filter getter symbol-pins)
                                  less-func
                                  equal-func)))

(define (check-duplicates/pinseq symbol-pins)
  (check-pin-attrib-duplicates 'pinseq
                               symbol-pin-seq
                               pinseq<?
                               pinseq=?
                               symbol-pins))

(define (check-duplicates/pinnumber symbol-pins)
  (check-pin-attrib-duplicates 'pinnumber
                               symbol-pin-number
                               pinnumber<?
                               pinnumber=?
                               symbol-pins))
