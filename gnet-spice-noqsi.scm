(define (spice-noqsi filename)
    (set-current-output-port(open-output-file filename))
    (for-each reserve-refdes packages)
    (for-each process-part packages)
    (if subcircuit (format #t "~A\n" subcircuit))
    (for-each (lambda (s) (format #t "~A\n" s)) cards)
    (if subcircuit (format #t ".ENDS\n"))
    (if error-count (begin
        (format #f "~A errors.\n" error-count)
        (primitive-exit 1))))

;; This variable will hold the .subckt card if given.
(define subcircuit #f)

;; List of cards in the circuit or subcircuit.
(define cards '())

;; If this isn't zero, exit with nonzero status when done.
(define error-count 0)

;; Get a list of numbers 1..n
;; Why isn't this basic function in Guile?

(define (range n)
    (if (positive? n) (append (range (1- n)) (list n)) '()))

;; gnetlist associates net with pinnumber, but traditionally SPICE
;; backends for gnetlist have keyed on pinseq. This function implements that.

(define (get-net-by-pinseq refdes n)
    (let* (
        (pinseq (number->string n))
        (pinnumber (gnetlist:get-attribute-by-pinseq 
            refdes pinseq "pinnumber")))

        (if (equal? pinnumber "unknown") 
            (pinseq-error refdes pinseq)
            (get-net refdes pinnumber))))

;; If we can't get the pinnumber, return "" for the net, which should be 
;; adequately toxic in the resulting file in most cases.
            
(define (pinseq-error refdes pinseq)
    (format (current-error-port)
        "Error: pinseq=~A not found for refdes=~A\n" pinseq refdes)
    (set! error-count (1+ error-count))
    "")

;; Get the net attached to a particular pin.
;; This really should be a helper in gnetlist.scm, or even
;; replace the partially broken (gnetlist:get-nets).

(define (get-net refdes pin)
    (car (gnetlist:get-nets refdes pin)))

;; Expand a string in context of a particular refdes

(define (expand-string refdes s)
    (string-concatenate (map
        (lambda (f) (check-field refdes f))
        (parse-fields s))))

;; Split string into whitespace and ink.

(define (parse-fields s)
    (if (equal? s "")
        '()
        (let ((i (or 
            (string-skip s char-set:whitespace)
            (string-skip s 
                (char-set-complement char-set:whitespace)))))

            (append 
                (string-take s i) 
                (parse-fields (string-drop s i))))))    
        
;; Magic characters for field expansion

(define magic (string->char-set "?#=@"))

;; Check field for magic, expand if necessary

(define (check-field refdes field)
    (let ((i (string-index field magic)))
        (if i 
            (expand-field refdes
                (string-take field i)
                (substring field i i)
                (string-drop field (+ i 1)))
            field)))

;; Dispatch to the chosen expander

(define (expand-field refdes left key right)
    ((case key
        (("?") expand-refdes)
        (("#") expand-pin)
        (("=") expand-attr)
        (("@") expand-value)) refdes left right))

;; Expand refdes, munging if asked

(define (expand-refdes refdes left right)
    (string-append
        (if (string-prefix-ci? left refdes) 
            refdes 
            (get-munged left refdes))
        right))

(define (get-value-or-default refdes attr default)
    (if (equal? value "unknown") default value)

;; forward and reverse refdes maps

(define munges (make-hash-table))
(define refdes-reserved (make-hash-table))

;; prevent munging from accidentally duplicating an existing refdes

(define (reserve-refdes r) (hash-set! r r))

;; Get the munged version of refdes

(define (get-munged munged left refdes)
    (or 
        (hash-ref munges refdes)
        (make-munged refdes (string-append left refdes))))

;; Make unique munged version

(define (make-munged refdes candidate)
    (if (hash-ref refdes-reserved candidate)
        (make-munged refdes (string-append candidate "X"))
        (begin
            (hash-set! refdes-reserved candidate refdes)
            (hash-set! munges refdes candidate))))

;; Get name of net connected to pin, or invoke (all-by-pinseq) for ##

(define (expand-pin refdes left right)
    (if (equal? left "")
        (if (equal? right "#")
            (all-by-pinseq refdes)
            (get-net refdes right))
        (expand-pin left "" right)))

;; 

(define (all-by-pinseq refdes)
    (string-join
        (map
            (lambda (n) (get-net-by-pinseq refdes n))
            (range (length (gnetlist:get-pins refdes))))
        " "))


;; Expand attribute. Empty string if it doesn't exist, and no default given.

(define (expand-attr refdes name default)
    (let ((value (expand-value refdes name default)))
        (if (equal? value "")
            ""
            (string-append name "=" value))))

;; Expand value. Empty string if it doesn't exist, and no default given.

(define (expand-value refdes name default)
    (let ((value (gnetlist:get-package-attribute refdes name)))
        (if (equal? value "unknown")
            default
            value)))
