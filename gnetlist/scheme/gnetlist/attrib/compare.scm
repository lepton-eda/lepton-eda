(define-module (gnetlist attrib compare)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (gnetlist attrib compare)
  #:export (refdes<?
            value<?))


;;; The table of suffices used to compare value= attribute values
(define %value-suffix-factor-table
  '((p . 10e-12)
    (n . 10e-9)
    (u . 10e-6)
    (m . 10e-3)
    (k . 10e+3)
    (M . 10e+6)
    (G . 10e+9)
    (T . 10e+12)))

;;; Outputs list of pairs (type . char) for every char of S which
;;; must be a string.  Type is #t for numeric chars and #f for
;;; others.  Zeros are treated differently, see comments for
;;; refdes->alpha-numeric-list below.
(define (string->char-pairs s)
  (with-input-from-string s
    (lambda ()
      (let loop ((result '())
                 (previous #f))
        (let ((c (read-char)))
          (if (eof-object? c)
              result
              (let ((type (if (eq? c #\0)
                              previous
                              (char-numeric? c))))
                (loop `((,type . ,c) . ,result) type))))))))

;;; Groups char pairs in LS to form a list of typed char-sets,
;;; where each char-set is of the form (type . list-of-chars).
;;; Type is #t for numeric values and #f for others.
(define (group-char-pairs ls)
  (fold
   (lambda (n p)
     (match n
       ((ntype . nvalue)
        (match p
          (((ptype . pvalue) . rest)
           (if (eq? ntype ptype)
               `((,ptype . (,nvalue . ,pvalue)) . ,rest)
               `((,ntype ,nvalue) . ,p)
               ))
          (_ `((,ntype ,nvalue)))))))
   '()
   ls))

;;; Given list of typed char sets LS, outputs a list of values,
;;; that is a list of alternating string and number, depending on
;;; the type of each charset.  Type is #t for numeric values and
;;; #f for others.
(define (char-set-list->value-list ls)
  (define (char-set->value x)
    (match x
      ((numeric? . value)
       ((if numeric? string->number identity) (list->string value)))))

  (map char-set->value ls))


;;; Breaks up string S and forms a list of strings and
;;; numbers. For example, if S is "R15a2", the result is '("R" 15
;;; "a" 2). Trailing zeros and zeros being a part of zero padded
;;; numbers become a part of a string to provide means for
;;; comparison of refdeses with zero padded numbers and ordinary
;;; refdeses. For example, "R99" is less than "R099".
(define (refdes->alpha-numeric-list s)
  (let ((ls (char-set-list->value-list
             (group-char-pairs
              (string->char-pairs s)))))
    `(,(number? (car ls)) . ,ls)))

;;; Compares two lists L1 and L2 which must be alphanumeric such
;;; as produced by refdes->alpha-numeric-list.
(define (rlist<? l1 l2)
    (match `(,l1 ,l2)
      (((same-numeric? . rest1) (same-numeric? . rest2))
       (let loop ((l1 rest1)
                  (l2 rest2)
                  (numeric? same-numeric?))
         (and (not (null? l2))
              (or (null? l1)
                  (let ((equal? (if numeric? = string=?))
                        (less? (if numeric? < string<?)))
                    (if (equal? (car l1) (car l2))
                        (loop (cdr l1) (cdr l2) (not numeric?))
                        (less? (car l1) (car l2))))))))
      (((numeric1? . rest1) (numeric2? . rest2))
       ;; It's enough that first list begins with a number and second
       ;; with a string
       numeric1?)))


(define (refdes<? a b)
  "Compares two strings A and B alphanumerically. Parts of the
strings consisting of digits are transformed into numbers and are
compared as numbers. Other parts are compared as strings. Starting
number is considered to be less than starting string. Zero padded
numbers are considered to be more than plain ones to avoid
ambiguity. Example usage is sorting of reference designators."
  (rlist<? (refdes->alpha-numeric-list a)
           (refdes->alpha-numeric-list b)))


(define (value<? a b)
  "Compares two string A and B, typically values of the attribute
\"value=\", trying to get the numbers they present accounting for
their suffix and numerical value. If it is not possible, compares
them by less as strings using case sensitive string comparison
function."
  (define (suffix->factor s)
    (assq-ref %value-suffix-factor-table (string->symbol s)))

  (define (value-string->number s)
    (let ((num (string->number s)))
      (or num
          (let* ((num-length (1- (string-length s)))
                 (suffix (suffix->factor (substring s num-length)))
                 (prefix (string->number (substring s 0 num-length))))
            (and suffix prefix (* prefix suffix))))))

  (let ((anum (value-string->number a))
        (bnum (value-string->number b)))
    (if (and anum bnum)
        (< anum bnum)
        (string<? a b))))
