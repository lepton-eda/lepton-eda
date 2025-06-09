;;; Test Scheme procedures related to the "whichend" field of
;;; pins.

(use-modules (ice-9 match)
             (lepton object)
             (lepton page))

;;; This is a very roundabout test.  We would add a setter for
;;; pin's "whichend" field and use it directly.  However, no
;;; Lepton code does use it.  The only place we encounter it is in
;;; the gEDA file format parser.

(test-begin "pin-whichend")

(define old-pin (make-net-pin '(1 . 2) '(3 . 4)))

(define old-page
  (page-append! (make-page "dummy") old-pin))

(define old-pin-string
  (match (string-split (page->string old-page) #\newline)
    ((version pin anything)
     pin)
    (_ #f)))

;;; Transform "P 1 2 3 4 1 0 0" => "P 1 2 3 4 1 0 1".
(define (toggle-whichend-field pin-str)
  (match (string-split pin-str #\space)
    ((object-type x0 y0 x1 y1 color pin-type whichend)
     (if (zero? (string->number whichend))
         (string-join (list object-type x0 y0 x1 y1 color pin-type "1"))
         #f))
    (_ #f)))

(define new-page
  (string->page
   "dummy2"
   (match (string-split (page->string old-page) #\newline)
     ((version pin anything)
      (string-join (list version
                         (toggle-whichend-field pin)
                         anything)
                   (char-set->string (char-set #\newline))))
     (_ #f))))

(define new-pin (car (page-contents new-page)))

(test-eq (object-color old-pin) (object-color new-pin))
(test-equal (line-start old-pin) (line-end new-pin))
(test-equal (line-end old-pin) (line-start new-pin))

(close-page! old-page)
(close-page! new-page)

(test-end "pin-whichend")
