(use-modules (lepton core toplevel)
             (lepton object)
             (lepton attrib)
             (lepton page)
             (lepton toplevel))

(define (make-attribs)
  (list
   (make-text '(200 . 200) 'lower-left  0 "pinnumber=1" 4 #f 'value 5)
   (make-text '(200 . 400) 'lower-left  0 "pinseq=1" 4 #f 'both 5)
   (make-text '(200 . 100) 'lower-left  0 "pinlabel=1" 4 #f 'value 5)
   (make-text '(200 . 300) 'lower-left  0 "pintype=pwr" 4 #f 'value 5)))

(define (set-stroke! line)
  (set-object-stroke! line 0 'none 'solid))

(define (make-gnd-symbol)
  (let ((page (make-page "symbol"))
        (pin
         (make-net-pin '(100 . 300) '(100 . 100) 1))
        (attribs (make-attribs))
        (lines
         (list
          (set-stroke! (make-line '(0 . 100) '(200 . 100) 3))
          (set-stroke! (make-line '(55 . 50) '(145 . 50)  3))
          (set-stroke! (make-line '(80 . 10) '(120 . 10)  3))))
        (net-attrib
         (make-text
          '(300 . 100) 'lower-left  0 "net=GND:1" 10 #f 'value 8)))

    (apply page-append!
           page
           (append (list pin) attribs lines (list net-attrib)))

    (apply attach-attribs! pin attribs)

    page))


(define (make-vss-symbol)
  (let ((page (make-page "symbol"))
        (pin
         (make-net-pin '(100 . 100) '(100 . 300) 1))
        (attribs (make-attribs))
        (line
         (set-stroke! (make-line '(0 . 300) '(200 . 300) 3)))
        (net-attrib
         (make-text
          '(300 . 100) 'lower-left  0 "net=Vss:1" 10 #f 'value 8)))

    (apply page-append!
           page
           (append (list pin) attribs (list line) (list net-attrib)))

    (apply attach-attribs! pin attribs)

    page))

(define (thunk->string thunk)
  (%with-toplevel
   (%make-toplevel)
   (lambda ()
     (let ((page (thunk)))
       (page->string page)))))

(define dummy-sym-contents
  "v 20200604 2
B 0 0 500 500 3 10 1 0 -1 -1 0 -1 -1 -1 -1 -1
T 0 600 21 6 1 0 0 0 1
refdes=R?")

(define (list-function)
  '("my-gnd.sym"
    "my-vss.sym"
    "dummy.sym"))

(define (get-function symbol-name)
  (cond
   ((string=? symbol-name "my-gnd.sym") (thunk->string make-gnd-symbol))
   ((string=? symbol-name "my-vss.sym") (thunk->string make-vss-symbol))
   ((string=? symbol-name "dummy.sym") dummy-sym-contents)
   (else #f)))


(component-library-funcs list-function get-function "power-symbols")
