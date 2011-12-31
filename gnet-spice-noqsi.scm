
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

;;

(define (process-part refdes)
    (let (
        (proto (gnetlist:get-package-attribute refdes "spice-prototype"))
        (card '()))
        (if (equal? proto "unknown") 
            (set! proto (lookup-proto refdes)))
	(set! card (expand-string refdes proto))
        (if (string-prefix-ci? ".subckt" card) 
            (subckt card)
            (set! cards (cons card cards)))))

;; If no spice-prototype attribute, get prototype by other means.

(define (lookup-proto refdes)
    (or 
        (hash-ref prototypes 
            (gnetlist:get-package-attribute refdes "device"))
        (hash-ref prototypes "unknown")))


;; record a subcircuit card, error if more than one

(define (subckt card)
    (if subcircuit
        (begin
            (format #f "More than one .subckt card generated!\n")
            (set! error-count (1+ error-count)))
        (set! subcircuit card)))
        
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
    (let ((i (or 
        (field-skip s char-set:whitespace)
        (field-skip s 
            (char-set-complement char-set:whitespace)))))

        (if i    
            (append 
                (list (string-take s i))
                (parse-fields (string-drop s i)))
            (list s))))
 
;; string-skip is a bit difficult to use directly, yielding 0 for no match,
;; and #f when the whole string matches! Yielding only a positive number or
;; #f simplifies the logic above, so that's what I do here.

(define (field-skip s cs)
    (let ((i (string-skip s cs)))
    
    (if i 
        (if (zero? i) 
            #f 
            i)
        #f)))
   
;; Magic characters for field expansion

(define magic (string->char-set "?#=@"))

;; Check field for magic, expand if necessary

(define (check-field refdes field)
    (let ((i (string-index field magic)))
        (if i 
            (expand-field refdes
                (string-take field i)
                (substring field i (+ i 1))
                (string-drop field (+ i 1)))
            field)))

;; Dispatch to the chosen expander

(define (expand-field refdes left key right)
    ((cond
        ((equal? key "?") expand-refdes)
        ((equal? key "#") expand-pin)
        ((equal? key "=") expand-attr)
        ((equal? key "@") expand-value)) refdes left right))

;; Expand refdes, munging if asked

(define (expand-refdes refdes left right)
    (string-append
        (if (string-prefix-ci? left refdes) 
            refdes 
            (get-munged left refdes))
        right))

(define (get-value-or-default refdes attr default)
    (if (equal? value "unknown") default value))

;; forward and reverse refdes maps

(define munges (make-hash-table))
(define refdes-reserved (make-hash-table))

;; prevent munging from accidentally duplicating an existing refdes

(define (reserve-refdes r) (hash-set! refdes-reserved r r))

;; Get the munged version of refdes
;; "left" is the required prefix

(define (get-munged left refdes)
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

;; Prototypes

(define prototypes (make-hash-table))

(define (prototype-set! device proto) (hash-set! prototypes device proto))

;; Standard prototypes

(prototype-set! "unknown" "? ## value@ model-name@ spice-args@")
(prototype-set! "AOP-Standard" "X? ## model-name@")
(prototype-set! "BATTERY" "V? #1 #2 spice-args@")
(prototype-set! "SPICE-cccs" "F? #1 #2 V? value@\n?? #3 #4 DC 0")
(prototype-set! "SPICE-ccvs" "H? #1 #2 V? value@\n?? #3 #4 DC 0")
(prototype-set! "directive" "value@")
(prototype-set! "include" ".INCLUDE value@")
(prototype-set! "options" ".OPTIONS value@")
(prototype-set! "CURRENT_SOURCE" "I? ## value@")
(prototype-set! "K" "K? inductors@ value@")
(prototype-set! "SPICE-nullor" "N? ##")
(prototype-set! "SPICE-NPN" "Q? ## model-name@ spice-args@ ic= temp=")
(prototype-set! "PNP_TRANSISTOR" "Q? ## model-name@ spice-args@ ic= temp=")
(prototype-set! "NPN_TRANSISTOR" "Q? ## model-name@ spice-args@ ic= temp=")
(prototype-set! "spice-subcircuit-LL" ".SUBCKT ##P model-name@")
(prototype-set! "SPICE-VC-switch" "S? ## model-name@ value@")
(prototype-set! "T-line" "T? ## value@")
(prototype-set! "vac" "V? ## value@")
(prototype-set! "SPICE-vccs" "G? ## value@")
(prototype-set! "SPICE-vcvs" "E? ## value@")
(prototype-set! "VOLTAGE_SOURCE" "V? ## value@")
(prototype-set! "vexp" "V? ## value@")
(prototype-set! "vpulse" "V? ## value@")
(prototype-set! "vpwl" "V? ## value@")
(prototype-set! "vsin" "V? ## value@")
(prototype-set! "VOLTAGE_SOURCE" "V? ## value@")
(prototype-set! 
    "CAPACITOR" "C? ## value@ model-name@ spice-args@ l= w= area= ic=")
(prototype-set! "DIODE" "D? ## model-name@ spice-args@ area= ic= temp=")
(prototype-set! "NMOS_TRANSISTOR" 
    "M? ## model-name@ spice-args@ l= w= as= ad= pd= ps= nrd= nrs= temp= ic= m=")
(prototype-set! "PMOS_TRANSISTOR" 
    "M? ## model-name@ spice-args@ l= w= as= ad= pd= ps= nrd= nrs= temp= ic= m=")
(prototype-set! "RESISTOR" 
    "R? ## value@ model-name@ spice-args@ w= l= area= temp=")
(prototype-set! "DUAL_OPAMP" 
    "X? #3 #2 #8 #4 #1 model-name@\nX? #5 #6 #8 #4 #7 model-name@")
(prototype-set! "QUAD_OPAMP"
    "X? #3 #2 #11 #4 #1 model-name@
X? #5 #6 #11 #4 #7 model-name@
X? #10 #9 #11 #4 #8 model-name@
X? #12 #13 #11 #4 #14 model-name@")

