(define-module (symbol check text)
  ;; See a note about UTF-8 below.
  ;; #:use-module (rnrs bytevectors)
  ;; #:use-module (rnrs io ports)
  #:use-module (geda object)
  #:use-module (geda attrib)
  #:use-module (symbol gettext)
  #:use-module (symbol blame)

  #:export (check-text-visibility
            check-text-string
            check-text))

(define (check-text-visibility object)
  "Checks if text OBJECT being non-attribute has inappropriate
visibility mode."
  (and (text? object)
       (not (attribute? object))
       (not (eq? (text-attribute-mode object) 'both))
       (blame-object object
                     'warning
                     (format #f
                             (_ "Found a simple text object with only SHOW_NAME or SHOW_VALUE set [~A]\n")
                             (text-string object)))))


(define (check-text-string-errors text)
  (define (add-error-if condition type ls)
    (if condition `(,type . ,ls) ls))

  (with-input-from-string text
    (lambda ()
      (let read-next ((escape #f)
                      (overbar #f)
                      (ls '()))
        (let ((c (read-char)))
          (if (eof-object? c)
              (reverse (add-error-if escape 'trailing-backslash
                                     (add-error-if overbar 'unbalanced-overbar ls)))

            (case c
              ((#\\) (read-next (not escape) overbar ls))
              ((#\_) (if escape
                       (read-next (not escape) (not overbar) ls)
                       (read-next escape overbar ls)))
              (else  (if escape
                       (read-next (not escape) overbar `(unbalanced-escape . ,ls))
                       (read-next escape overbar ls))))))))))


(define (check-text-string object)
  "Checks string of text OBJECT."
  (and (text? object)
       (let ((s (text-string object)))
         (for-each
          (lambda (error-type)
            (blame-object object
                          'warning
                          (case error-type
                            ((trailing-backslash)
                             (format #f (_ "Found text with a trailing '\\': consider to escape it with '\\\\' [~A]\n") s))
                            ((unbalanced-overbar)
                             (format #f (_ "Found text with unbalanced overbar markers '\\_' in it' [~A]\n") s))
                            ((unbalanced-escape)
                             (format #f (_ "Found text with a '\\' in it: consider to escape it with '\\\\' [~A]\n") s)))))
          (check-text-string-errors s)))))


(define (check-text object)
  "Checks text OBJECT."
  (check-text-string object)
  (unless (attribute? object)
    (check-text-visibility object)))


;;; Incorrect UTF-8 encoded strings are blocked in
;;; o_read_buffer(), so there is no need to check them here (at
;;; least yet).
#|

(define-syntax decoding-error?
  (syntax-rules ()
    ((_ exp)
     (catch 'decoding-error
       (lambda ()
         ;; Debug output
         ;; (pk 'exp exp)
         #f)
            (lambda (key subr message errno p) (not (= 0 errno)))))))

(define (valid-utf8-string? str)
  (not (decoding-error?
         (utf8->string
           (with-input-from-string str
             (lambda () (get-bytevector-all
               (current-input-port))))))))

|#
