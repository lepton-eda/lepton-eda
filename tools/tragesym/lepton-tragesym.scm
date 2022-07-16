;;; Copyright (C) 2019-2022 Lepton EDA Contributors
;;;
;;; Based on Python script by Werner Hoch
;;; Copyright (C) 2001,2002,2003,2004,2006,2007,2008 Werner Hoch <werner.ho@gmx.de>
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; lepton-tragesym
;;; Create Lepton EDA symbols out of structured textfiles.

;;; FEATURES:
;;; - Create pins and their attributes/elements.
;;; - Sort pins alphabetically by attributes.
;;; - Rotate top and bottom pinlabel= attributes if requested.
;;; - Swap words of the pinlabel= attributes (only for attributes
;;;   of right pins and top pins, in the latter case only when
;;;   rotation is requested, too).
;;; - Negation lines if label is in "_","\" is for escape
;;; - Symbol width will be automatically increased based on values
;;;   defined by the greater number of pins at the top or at the
;;;   bottom of the symbol, and the width requested.

(use-modules (ice-9 getopt-long)
             (ice-9 match)
             (ice-9 rdelim)
             (ice-9 textual-ports)
             (srfi srfi-1)
             (srfi srfi-9)
             (sxml match)
             (lepton attrib)
             (lepton ffi)
             (lepton object)
             (lepton page)
             (lepton toplevel)
             (lepton version)
             (netlist attrib compare))

;;; Initialize liblepton library.
(liblepton_init)
(unless (getenv "LEPTON_INHIBIT_RC_FILES")
  (register-data-dirs))


(define %option-spec
  '((help    (single-char #\h) (value #f))
   (version (single-char #\V) (value #f))))

(define %options
  (getopt-long (command-line) %option-spec))

(define special-attributes
  '("name" "refdes"))

(define single-attribute-warning
  '("device"
    "footprint"
    "author"
    "documentation"
    "description"
    "numslots"
    "symversion"
    "dist-license"
    "use-license"))

(define stylelist
  '("line" "dot" "clk" "dotclk" "spacer" "none"))

(define poslist
  '("l" "r" "t" "b" ""))

(define typelist
  '("in" "out" "io" "oc" "oe" "pas" "tp" "tri" "clk" "pwr"))


(define %default-component-options
  '((wordswap . #t)
    (rotate_labels . #f)
    (sort_labels . #t)
    (generate_pinseq . #t)
    (sym_width . 1400)
    (pinwidthvertikal . 400)
    (pinwidthvertical . 400)
    (pinwidthhorizontal . 400)))

;;; These constants have no appropriate config settings in the
;;; original tragesym.
(define %pinlength 300)
(define %clock-triangle-height 100)
(define %clock-triangle-base 150)

;;; Transform immutable alist into a mutable one.
(define %component-options
  (map (lambda (x) (cons (car x) (cdr x)))
       %default-component-options))

(define (set-component-option! name value)
  (set! %component-options
        (assq-set! %component-options name value)))

(define (component-option name)
  (assq-ref %component-options name))


(define (string->pair s warning-thunk)
  (define cs (char-set #\= #\tab))

  (let ((index (string-index s cs)))
    (if (or (not index)
            (zero? index)
            (= index (1- (string-length s))))
        (warning-thunk)
        (let ((name (string-trim-both (string-take s index)
                                      char-set:whitespace))
              (value (string-trim-both (string-drop s (1+ index))
                                       char-set:whitespace)))
          (if (or (string-null? name)
                  (string-null? value))
              (warning-thunk)
              (cons name value))))))


(define (string->integer value)
  (let ((num (string->number value)))
    (and (integer? num)
         num)))


(define (string->option-value s)
  (define value-alist
    '(("yes" . #t)
      ("on" . #t)
      ("no" . #f)
      ("off" . #f)))
  (or (assoc-ref value-alist s)
      (string->integer s)))

(define (format-warning s . args)
  (display "Warning: " (current-error-port))
  (apply format (current-error-port) s args)
  (put-char (current-error-port) #\newline)
  ;; Return value for functions that need it.
  #f)

(define option-names (map car %default-component-options))
(define (option-name-exists? name)
  (or (memq name option-names)
      (format-warning "Option ~S is not allowed and will be dropped." name)))


(define (option-string->pair s)
  (define (drop-warning)
    (format-warning "Wrong option definition: ~S" s))

  (let ((name-value (string->pair s drop-warning)))
    (and name-value
         (let ((option-name (string->symbol (car name-value)))
               (option-value (string->option-value (cdr name-value))))
           (and (option-name-exists? option-name)
                (set-component-option! option-name option-value))))))

(define valid-section-names '(geda_attr options pins))

(define (section-string->symbol s)
  (let ((section-name
         (string->symbol (string-drop-right (string-drop s 1) 1))))
    ;; Don't exit here, show must go on.
    (unless (memq section-name valid-section-names)
      (format-warning "Found illegal section name: ~A.
The section won't be processed." section-name))
    section-name))

(define (file-contents filename)
  (let ((cs (char-set-delete char-set:whitespace #\tab)))
    (with-input-from-file filename
      (lambda ()
        (let loop ((s (read-line))
                   (ls '()))
          (if (eof-object? s)
              (reverse ls)
              (let ((trimmed-s (string-trim-right (string-trim-both s cs) #\tab)))
                (loop (read-line)
                      (if (or (string-null? trimmed-s)
                              (char=? #\# (string-ref trimmed-s 0)))
                          ls
                          (if (section-name? trimmed-s)
                              (cons `(section ,(section-string->symbol trimmed-s)) ls)
                              (cons `(element ,trimmed-s) ls)))))))))))


;;; FIXME: check with spaces before and after section name.
(define (section-name? s)
  (and (char=? #\[ (string-ref s 0))
       (char=? #\] (string-ref s (1- (string-length s))))))


(define (merge-options ls)
  (sxml-match
   ls
   [(list (section . ,section) (element ,element) . ,[anything])
    (merge-options `((section ,@section ,element) ,@anything))]
   [,other other]))


(define (file-contents->alist filename)
  (sxml-match
   (merge-options (file-contents filename))
   ;; The same as (map car list-of-sections).
   [(list (section . ,anything) ...)
    `((,@anything) ...)]))


;;; Encapsulates data related to a pin.
(define-record-type pin
  (make-pin number seq type style pos net label)
  pin?
  (number pin-number set-pin-number!)
  (seq pin-seq set-pin-seq!)
  (type pin-type set-pin-type!)
  (style pin-style set-pin-style!)
  (pos pin-pos set-pin-pos!)
  (net pin-net set-pin-net!)
  (label pin-label set-pin-label!))


(define (pin-info p)
  (match p
    (($ pin number seq type style pos net label)
     (format #f
             "Pin object (number:~A seq:~A type:~A style:~A pos:~A net:~A label:~A)"
             number seq type style pos net label))))


;;; Original script sorted by position side as well. We use fixed
;;; position instead.
(define (pin<? pin other-pin)
  (or (string<? (pin-net pin) (pin-net other-pin))
      (and (string= (pin-net pin) (pin-net other-pin))
           (refdes<? (parse-label (pin-label pin))
                     (parse-label (pin-label other-pin))))))


(define (format-error s . args)
  (display "Error: " (current-error-port))
  (apply format (current-error-port) s args)
  (put-char (current-error-port) #\newline)
  (primitive-exit 1))


(define (check-pin-info pin)
  (if (string= (pin-style pin) "spacer")
      (begin
        (when (component-option 'sort_labels)
          (format-warning "Spacers may be in unpredictable place when sorting labels.
Cheat fraudulent label names for them to sort them properly."))
        (when (string-null? (pin-pos pin))
          (format-error "There must be a position with a spacer."))
        (unless (member (pin-pos pin) poslist)
          (format-error "Position is not allowed: ~A." (pin-info pin))))
      (begin
        (when (and (component-option 'generate_pinseq)
                   (not (string= (pin-style pin) "none"))
                   ;; This will work if pin-seq is not auto-generated.
                   (not (integer? (pin-seq pin))))
          (let ((num (string->integer (pin-seq pin))))
            (if num
                (set-pin-seq! pin num)
                (format-error "Pinseq needs to be a number: ~A." (pin-info pin)))))
        (unless (member (pin-type pin) typelist)
          (format-error "Pintype not allowed: ~A." (pin-info pin)))
        (unless (member (pin-style pin) stylelist)
          (format-error "Style is not allowed: ~A." (pin-info pin)))
        (unless (member (pin-pos pin) poslist)
          (format-error "Position is not allowed: ~A." (pin-info pin)))
        (when (and (string-null? (pin-pos pin)) (string-null? (pin-net pin)))
          (format-error "There must be either position or a netlabel: ~A." (pin-info pin))))))


(define (usage)
    "Print a usage message."
    (format #t "~A INPUT-FILE OUTPUT-FILE
(C) 2019 Lepton EDA Contributors
Based on original \"tragesym\" version
(C) 2001,2002,2003,2004,2006,2007 by Werner Hoch <werner.ho@gmx.de>
"
        (basename (car (command-line)))))

(define (print-version)
  (display-lepton-version #:print-name #t #:copyright #t))


(define (parse-label s)
  (define overbar-*fix "\\_")
  (if (and (string-prefix? overbar-*fix s)
           (string-suffix? overbar-*fix s))
      (string-drop-right (string-drop s 2) 2)
      s))

;;; Round *unsigned* integer x (size) to closest r (grid step)
(define (round-closest x r)
  (* r (round (/ x r))))

;; returns the words in reverse order
(define (swapwords s)
  (string-join (reverse (string-split s #\space)) " "))


(define (warn-missing-attribute s)
  (format-warning "~S attribute missing." s))


(define (writesym options attributes pins)
  (define page (make-page "dummy-filename"))
  (define %horizontal-pin-distance (component-option 'pinwidthhorizontal))
  ;; 'pinwidthvertikal' is backward compatible option for older
  ;; versions. New option name is 'pinwidthvertical'.
  (define %vertical-pin-distance (or (component-option 'pinwidthvertical)
                                     (component-option 'pinwidthvertikal)))
  (define %swap-words (component-option 'wordswap))
  (define %rotate-labels (component-option 'rotate_labels))

  (define merge-by-net-name identity)

  (define (left-pin? pin) (string= (pin-pos pin) "l"))
  (define (right-pin? pin) (string= (pin-pos pin) "r"))
  (define (bottom-pin? pin) (string= (pin-pos pin) "b"))
  (define (top-pin? pin) (string= (pin-pos pin) "t"))

  (define (real-pin? pin)
    (not (string= (pin-style pin) "none")))

  (define real-pins
    (filter real-pin?
            (if (component-option 'sort_labels)
                (sort pins pin<?)
                pins)))

  (define pins/left (filter left-pin? real-pins))
  (define pins/right (filter right-pin? real-pins))
  (define pins/bottom (filter bottom-pin? real-pins))
  (define pins/top (filter top-pin? real-pins))

  ;; Count the number of pins in each side
  (define pin-count/left (length pins/left))
  (define pin-count/right (length pins/right))
  (define pin-count/bottom (length pins/bottom))
  (define pin-count/top (length pins/top))

  ;; Calculate the origin of the symbol (bottom left of the box).
  ;; It can be just (0 . 0). The values below were taken from the
  ;; original tragesym code.
  (define origin-x (+ %pinlength 100))
  (define origin-y (+ 100 (if (> pin-count/bottom 0) %pinlength 0)))

  ;; Calculate the minimum symwidth and increase it if necessary
  (define calculated-top-symwidth
    (* (1+ pin-count/top) %horizontal-pin-distance))

  (define calculated-bottom-symwidth
    (* (1+ pin-count/bottom) %horizontal-pin-distance))

  (define calculated-symwidth (max calculated-bottom-symwidth calculated-top-symwidth))

  (define symbol-width
    (max (or (component-option 'sym_width) 0)
         calculated-symwidth))

  (define symbol-height
    (let ((max-pin-count (if (< pin-count/left pin-count/right)
                             pin-count/right
                             pin-count/left)))
      (* (1+ max-pin-count) %vertical-pin-distance)))

  ;; Calculate the position of several items.
  (define pin-right-x (+ origin-x %pinlength symbol-width))
  (define pin-left-x (- origin-x %pinlength))
  (define pin-top-y (+ origin-y symbol-height %pinlength))
  (define pin-bottom-y (- origin-y %pinlength))

  (define (draw-refdes-attrib)
    (let ((x (+ origin-x symbol-width))
          (y (+ origin-y symbol-height 100))
          (angle 0)
          (alignment 'lower-right)
          (size 10)
          (color 8)
          (value (assoc-ref attributes "refdes")))
      (if value
          (page-append! page
                        (make-text `(,x . ,y)
                                   alignment
                                   angle
                                   (format #f "refdes=~A" value)
                                   size
                                   'visible
                                   'value
                                   color))
          (warn-missing-attribute "refdes"))))

  ;; Center name= at symbol's (x,y) only if we have top
  ;; pins. Otherwise it will be at the top.
  (define name-x
    (if (> pin-count/top 0)
        (/ (+ origin-x symbol-width) 2)
        origin-x))
  (define name-y
    (if (> pin-count/top 0)
        (+ (/ (+ origin-y symbol-height) 2) 100)
        (+ origin-y symbol-height 100)))

  (define textx origin-x)
  (define texty
    (if (> pin-count/top 0)
        (+ origin-y symbol-height %pinlength 100)
        (+ name-y 200)))


  (define (draw-name)
    (let ((name-value (assoc-ref attributes "name"))
          (color 9)
          (size 10)
          (alignment 'lower-left)
          (angle 0))
      (if name-value
          (page-append! page
                        (make-text `(,name-x . ,name-y)
                                   alignment
                                   angle
                                   (format #f "~A" name-value)
                                   size
                                   'visible
                                   'both
                                   color))
          (warn-missing-attribute "name"))))

  ;; Calculate the position of the pins at the left and right side.
  (define pin-left-y (+ origin-y
                        (* (max pin-count/left pin-count/right)
                           %vertical-pin-distance)))
  (define pin-right-y pin-left-y)

  ;; Calculate the position of several items.
  ;; Let's add some pad if sym_width was defined.
  (define pin-top-x
    (round-closest
     (+ origin-x %horizontal-pin-distance
        (/ (- symbol-width
              calculated-top-symwidth)
           2))
     100))

  (define pin-bottom-x
    (round-closest
     (+ origin-x %horizontal-pin-distance
        (/ (- symbol-width
              calculated-bottom-symwidth)
           2))
     100))

  (define pin-start-attribute-offset 200)
  (define pin-attribute-distance 50)

  (define bubble-radius 50)
  (define bubble-color 6)

  ;; Draw the pin.
  (define (draw-pin pin pin-start-x pin-start-y)
    (define (first* a . args) a)

    (let* ((has-bubble? (or (string= (pin-style pin) "dot")
                            (string= (pin-style pin) "dotclk")))
           (has-clock-sign? (or (equal? (pin-style pin) "clk")
                                (equal? (pin-style pin) "dotclk")))
           (pin-length (if has-bubble?
                           (- %pinlength (* 2 bubble-radius))
                           %pinlength))
           (which? (pin-pos pin))
           (pin-end-x (match which?
                        ("l" (+ pin-start-x pin-length))
                        ("r" (- pin-start-x pin-length))
                        (_ pin-start-x)))
           (pin-end-y (match which?
                        ("b" (+ pin-start-y pin-length))
                        ("t" (- pin-start-y pin-length))
                        (_ pin-start-y)))
           (bubble-x (match which?
                       ("l" (+ pin-end-x bubble-radius))
                       ("r" (- pin-end-x bubble-radius))
                       ;; Otherwise, X is the same for the start
                       ;; and end.
                       (_ pin-start-x)))
           (bubble-y (match which?
                       ("b" (+ pin-end-y bubble-radius))
                       ("t" (- pin-end-y bubble-radius))
                       ;; Otherwise, Y is the same for the start
                       ;; and end.
                       (_ pin-start-y)))
           ;; Here we do assume the pin end if no bubble would be
           ;; drawn. That is the pin end point if on the symbol
           ;; box.
           (pin-end-x* (match which?
                         ("l" (+ pin-start-x %pinlength))
                         ("r" (- pin-start-x %pinlength))
                         (_ pin-start-x)))
           (pin-end-y* (match which?
                         ("b" (+ pin-start-y %pinlength))
                         ("t" (- pin-start-y %pinlength))
                         (_ pin-start-y))))

      ;; Draw its negation bubble, if needed.
      ;; It goes first for now, since we are going to attach
      ;; attribs to the pin below, but they are not ready yet.
      (when has-bubble?
        (page-append! page
                      (make-circle (cons bubble-x bubble-y)
                                   bubble-radius
                                   bubble-color)))

      ;; Draw the clock sign.
      (when has-clock-sign?
        (let ((x1 ((match which?
                     ("l" +)
                     ("r" -)
                     (_ first*)) pin-end-x* %clock-triangle-height))
              (y1 ((match which?
                     ("b" +)
                     ("t" -)
                     (_ first*)) pin-end-y* %clock-triangle-height))
              (x2 ((match which?
                     ((or "b" "t") +)
                     (_ first*))
                   pin-end-x* (/ %clock-triangle-base 2)))
              (y2 ((match which?
                     ((or "l" "r") +)
                     (_ first*))
                   pin-end-y* (/ %clock-triangle-base 2)))
              (x3 ((match which?
                     ((or "b" "t") -)
                     (_ first*))
                   pin-end-x* (/ %clock-triangle-base 2)))
              (y3 ((match which?
                     ((or "l" "r") -)
                     (_ first*))
                   pin-end-y* (/ %clock-triangle-base 2))))
          (page-append! page
                        (set-object-stroke!
                         (make-line `(,x1 . ,y1) `(,x2 . ,y2))
                         10 'round 'solid)
                        (set-object-stroke!
                         (make-line `(,x1 . ,y1) `(,x3 . ,y3))
                         10 'round 'solid))))

      (let* ((pin-object (make-net-pin
                          `(,pin-start-x . ,pin-start-y)
                          `(,pin-end-x . ,pin-end-y)))
             (pinnumber-x
              (match which?
                ("l" (+ pin-start-x pin-start-attribute-offset))
                ("r" (- pin-start-x pin-start-attribute-offset))
                (_ (+ pin-start-x (if %rotate-labels
                                      (- pin-attribute-distance)
                                      pin-attribute-distance)))))
             (pinnumber-y
              (match which?
                ("t" (- pin-start-y pin-start-attribute-offset))
                ("b" (+ pin-start-y pin-start-attribute-offset))
                (_ (+ pin-start-y pin-attribute-distance))))

             (pinnumber-alignment
              (match which?
                ("l" 'lower-right)
                ("b" (if %rotate-labels 'lower-right 'upper-left))
                (_ 'lower-left)))

             (pinnumber-angle
              (match which?
                ((or "t" "b") (if %rotate-labels 90 0))
                (_ 0)))
             (pinnumber
              (make-text `(,pinnumber-x . ,pinnumber-y)
                         pinnumber-alignment
                         pinnumber-angle
                         (format #f "pinnumber=~A" (pin-number pin))
                         ;; Size.
                         8
                         'visible
                         'value))
             (pinseq-x
              (match which?
                ("l" (+ pin-start-x pin-start-attribute-offset))
                ("r" (- pin-start-x pin-start-attribute-offset))
                (_ (+ pin-start-x (if %rotate-labels pin-attribute-distance (- pin-attribute-distance))))))

             (pinseq-y
              (match which?
                ("t" (- pin-start-y pin-start-attribute-offset))
                ("b" (+ pin-start-y pin-start-attribute-offset))
                (_ (- pin-start-y pin-attribute-distance))))

             (pinseq-alignment
              (match which?
                ("r" 'upper-left)
                ("t" (if %rotate-labels 'upper-left 'lower-right))
                (_ 'upper-right)))

             (pinseq-angle
              (match which?
                ((or "t" "b") (if %rotate-labels 90 0))
                (_ 0)))
             (pinseq-counter (and (pin-seq pin)
                          (make-text `(,pinseq-x . ,pinseq-y)
                                     pinseq-alignment
                                     pinseq-angle
                                     (format #f "pinseq=~A" (pin-seq pin))
                                     ;; Size.
                                     8
                                     ;; Invisible.
                                     #f
                                     'value)))
             (clk? (or (string= (pin-style pin) "clk")
                       (string= (pin-style pin) "dotclk")))
             (pinlabel-distance 50)
             (pinlabel-x
              ((match which?
                 ("l" +)
                 ("r" -)
                 (_ first*))
               pin-end-x*
               (+ pinlabel-distance (if clk? %clock-triangle-height 0))))
             (pinlabel-y
              ((match which?
                 ("t" -)
                 ("b" +)
                 (_ first*))
               pin-end-y*
               (+ pinlabel-distance (if clk? %clock-triangle-height 0))))
             (pinlabel-alignment
              (match which?
                ("l" 'lower-left)
                ("r" 'lower-right)
                ("t" (if %rotate-labels 'lower-right 'upper-center))
                ("b" (if %rotate-labels 'lower-left 'lower-center))))
             (pinlabel-angle
              (match which?
                ((or "t" "b") (if %rotate-labels 90 0))
                (_ 0)))

             (pintype-x-distance 50)
             (pintype-x
              ((match which?
                 ("l" +)
                 ("r" -)
                 (_ first*))
               pin-end-x*
               (+ pintype-x-distance (if clk? %clock-triangle-height 0))))

             (pintype-y-distance (if %rotate-labels 50 200))
             (pintype-y
              ((match which?
                 ("t" -)
                 ("b" +)
                 (_ first*))
               pin-end-y*
               (+ pintype-y-distance (if clk? %clock-triangle-height 0))))

             (pintype-alignment
              (match which?
                ("l" 'upper-left)
                ("r" 'upper-right)
                ("t" (if %rotate-labels 'upper-right 'upper-center))
                ("b" (if %rotate-labels 'upper-left 'lower-center))))

             (pintype-angle
              (match which?
                ((or "t" "b") (if %rotate-labels 90 0))
                (_ 0)))

             (pinlabel-text
              (match which?
                ;; Swap words for right pins anyway, if specified.
                ("r" (if %swap-words
                         (swapwords (pin-label pin))
                         (pin-label pin)))
                ;; Swap words for top pins only if rotation is requested as well.
                ("t" (if (and %swap-words %rotate-labels)
                         (swapwords (pin-label pin))
                         (pin-label pin)))
                ;; Do not swap words for left and bottom pins.
                (_ (pin-label pin))))

             (pinlabel
              (make-text `(,pinlabel-x . ,pinlabel-y)
                         pinlabel-alignment
                         pinlabel-angle
                         (format #f "pinlabel=~A" pinlabel-text)
                         ;; Size.
                         8
                         'visible
                         'value
                         ;; Color.
                         9))
             (pintype
              (make-text `(,pintype-x . ,pintype-y)
                         pintype-alignment
                         pintype-angle
                         (format #f "pintype=~A" (pin-type pin))
                         ;; Size.
                         8
                         ;; Invisible.
                         #f
                         'value))
             (attribs (filter identity
                              (list pinnumber pinseq-counter pinlabel pintype))))

        (apply page-append! page pin-object attribs)
        (apply attach-attribs! pin-object attribs)
        (set-object-color! pinlabel 9))))

  (define (draw-with-increment draw-function items position increment)
    (unless (null? items)
      (draw-function (car items) position)
      (match-let (((x . y) position)
                  ((xi . yi) increment))
        (draw-with-increment draw-function
                             (cdr items)
                             ;; Increment position.
                             (cons (+ x xi) (+ y yi))
                             increment))))

  (define (draw-pin-list pins position position-increment)
    (draw-with-increment
     (lambda (pin position)
       (unless (string= "spacer" (pin-style pin))
         (draw-pin pin (car position) (cdr position))))
     pins position position-increment))

  (define formatted-net-attribs
    (match
        (list->duplicate-list*
         (filter
          (lambda (pin) (string= (pin-style pin) "none")) pins)
         (lambda (p1 p2)
           (or (string< (pin-net p1) (pin-net p2))
               (and (string= (pin-net p1) (pin-net p2))
                    (string< (pin-number p1) (pin-number p2)))))
         (lambda (p1 p2) (string= (pin-net p1) (pin-net p2))))
      (((($ pin number seq type style pos net label) ...) ...)
       (map string-append
            (map car net)
            (map
             (lambda (ls) (string-append ":" (string-join ls ",")))
             number)))))

  (define (filter-special-attribs attrib-alist)
    (filter
     (lambda (x)
       (not (member (car x) special-attributes)))
     attrib-alist))

  (define (make-attrib-list attrib-list net-attrib-list)
    (append
     (append-map
      (lambda (name-value)
        (if (list? name-value)
            ;; If it is a list, then there are several values.
            (map
             (lambda (value)
               ;; Make a pair (name . value) for each value.
               (cons (car name-value) value))
             (cdr name-value))
            ;; Just a pair, wrap it in a list to fed to append-map.
            (list name-value)))
      (filter-special-attribs attrib-list))
     (map (lambda (x) (cons "net" x)) net-attrib-list)))

  (define (draw-attrib attrib position)
    (let ((name (car attrib))
          (value (cdr attrib))
          (x (car position))
          (y (cdr position))
          (size 10)
          (angle 0)
          (visibility #f)
          (alignment 'lower-left)
          (color 5))
      (page-append! page
                    (make-text `(,x . ,y)
                               alignment
                               angle
                               (format #f "~A=~A" name value)
                               size
                               visibility
                               'both
                               color))))

  (define (draw-attrib-list attribs position increment)
    (draw-with-increment draw-attrib attribs position increment))


  (unless (zero? (+ pin-count/top pin-count/bottom))
    (format (current-error-port)
            "Note: use sym_width to adjust symbol width if texts overlap.\n"))


  ;; Left pins.
  (draw-pin-list pins/left
                 (cons pin-left-x pin-left-y)
                 `(0 . ,(- %vertical-pin-distance)))
  ;; Right pins.
  (draw-pin-list pins/right
                 (cons pin-right-x pin-right-y)
                 `(0 . ,(- %vertical-pin-distance)))
  ;; Top pins.
  (draw-pin-list pins/top
                 (cons pin-top-x pin-top-y)
                 `(,%horizontal-pin-distance . 0))
  ;; Bottom pins.
  (draw-pin-list pins/bottom
                 (cons pin-bottom-x pin-bottom-y)
                 `(,%horizontal-pin-distance . 0))

  (page-append! page
                (make-box `(,origin-x . ,origin-y)
                          `(,(+ origin-x symbol-width)
                            . ,(+ origin-y symbol-height))))

  (draw-refdes-attrib)
  (draw-name)

  (draw-attrib-list (make-attrib-list attributes
                                      formatted-net-attribs)
                    (cons textx texty)
                    '(0 . 200))
  (display
   (page->string page)))


(define (generate-pinseq pins)
  (define (generate-pinseq? pin)
    (and (not (string= (pin-style pin) "none"))
         (not (string= (pin-style pin) "spacer"))))

  (define (non-empty-pinseq? pin)
    (let ((pinseq (pin-seq pin)))
      (and (not (string-null? pinseq))
           pinseq)))

  (let* ((plain-pins (filter generate-pinseq? pins))
         (available-pinseq-list (filter-map non-empty-pinseq? plain-pins)))
    (let loop ((pins-to-process plain-pins)
               (pinseq-counter 1))
      (and (not (null? pins-to-process))
           (let ((pin (car pins-to-process)))
             (if (non-empty-pinseq? pin)
                 ;; Pin already has pinseq=, so don't increment
                 ;; our counter.  Continue with the rest of the
                 ;; list.
                 (loop (cdr pins-to-process) pinseq-counter)
                 ;; Pin has no pinseq.  First check, if our
                 ;; counter is valid, looking through the list of
                 ;; already set pinseqs.
                 (if (member (number->string pinseq-counter)
                             available-pinseq-list)
                     ;; Loop with the same pin list, just increment
                     ;; the counter.
                     (loop pins-to-process (1+ pinseq-counter))
                     ;; Set new pinseq=, and continue.
                     (begin
                       (set-pin-seq! (car pins-to-process) (number->string pinseq-counter))
                       (loop (cdr pins-to-process) (1+ pinseq-counter)))))))))
  ;; Return value.
  pins)


(define (remove-pinseq pins)
  (define (delete-pinseq pin)
    (when (string-null? (pin-seq pin))
      (set-pin-seq! pin #f)))
  (for-each delete-pinseq pins))


(define (attribute-string->pair s)
  (define (drop-warning)
    (format-warning "Empty attribute ~S in the geda_attr section.
The incomplete attribute will be dropped." s))

  (string->pair (string-trim-both s char-set:whitespace)
                drop-warning))

;;; Tests
;; (attribute-string->pair " = ") => #f
;; (attribute-string->pair " a = b ") => ("a" . "b")
;; (attribute-string->pair "= a") => #f
;; (attribute-string->pair "=b") => #f
;; (attribute-string->pair "a = ") => #f
;; (attribute-string->pair "a") => #f
;; (attribute-string->pair "a= ") => #f
;; (attribute-string->pair "a=") => #f
;; (attribute-string->pair "a=b") => ("a" . "b")

(define (pin-string->pin s)
  (define (drop-warning)
    (format-warning "Invalid pin specification in the pin section ~S.
It has less than 3 fields. The pin will be dropped." s))

  (define pintype-aliases
    '(("i/o" . "io")
      ("i" . "in")
      ("o" . "out")
      ("p" . "pas")))

  (define (translate-pintype pin)
    (let ((pintype (assoc-ref pintype-aliases (pin-type pin))))
      (when pintype (set-pin-type! pin pintype))
      pin))

  (define (lowercase-fields pin)
    (set-pin-type! pin (string-downcase (pin-type pin)))
    (set-pin-style! pin (string-downcase (pin-style pin)))
    (set-pin-pos! pin (string-downcase (pin-pos pin)))
    pin)

  (let* ((pin-list (string-split s #\tab))
         (l (length pin-list)))
    (if (not (<= 2 l))
        (drop-warning)
        (translate-pintype
         (lowercase-fields
          (apply make-pin
                 (append pin-list
                         (list-tabulate (- 7 l)
                                        (lambda (n) "")))))))))

(define (list->duplicate-list* ls f-less? f-equal?)
  "Sorts list LS using function F-LESS? for comparison and
transforms it into a list of lists containing duplicated members
which values are equal in terms of F-EQUAL?."
  (fold-right
   (lambda (elem ret)
     (match ret
       (((x . xrest) . rest)
        (if (f-equal? elem x)
            `((,elem . (,x . ,xrest)) . ,rest)
            `((,elem) . ,ret)))
       (_ `((,elem) . ,ret))))
   '()
   (sort ls f-less?)))


(define (attribute-list->alist attrib-ls)
  (define (fix-pair p)
    (match p
      ((name value)
       `(,name . ,value))
      (ls ls)))

  (define (pair-list->pair ls)
    (match ls (((name . value) ... )
               `(,(car name) . ,value))
           ;; Improper pair list. Don't check, simply return '().
           (_ '())))
  (define (attrib-name< s1 s2)
    (string< (car s1) (car s2)))

  (define (attrib-name= s1 s2)
    (string= (car s1) (car s2)))

  (map fix-pair
       (map pair-list->pair
            (list->duplicate-list* attrib-ls attrib-name< attrib-name=))))

(define (eval-options file-option-alist)
  "Updates %COMPONENT-OPTIONS with values read from input file."
  (for-each option-string->pair file-option-alist)
  %component-options)

;;; Handle command line options.
(define file-args (option-ref %options '() '()))

(when (option-ref %options 'help #f)
  (usage)
  (primitive-exit 0))

(when (option-ref %options 'version #f)
  (print-version)
  (primitive-exit 0))

(unless (= (length file-args) 2)
  (usage)
  (primitive-exit 1))

;;; Get file names.
(let ((input-file (car file-args))
      (output-file (cadr file-args)))

  (unless (file-exists? input-file)
    (format-error "Input file ~S not found." input-file))

  ;; FIXME: check that non-multi-attribs have only one value and vice versa
  (let* ((input-file-alist (file-contents->alist input-file))
         (options (eval-options (assq-ref input-file-alist
                                          'options)))
         (attributes
          (attribute-list->alist
           (filter-map attribute-string->pair
                       (assq-ref input-file-alist
                                 'geda_attr))))
         (pins
          (filter-map pin-string->pin
                      (assq-ref input-file-alist
                                'pins))))

    (if (component-option 'generate_pinseq)
        (generate-pinseq pins)
        (remove-pinseq pins))

    (for-each check-pin-info pins)

    (for-each
     (lambda (name)
       (unless (assoc-ref name attributes)
         (warn-missing-attribute name)))
     single-attribute-warning)

    (with-output-to-file output-file
      (lambda ()
        (with-toplevel
         (make-toplevel)
         (lambda ()
           (writesym options attributes pins)))))))
