(define-module (gnetlist verbose)
  #:use-module (gnetlist option)
  #:use-module (gnetlist package)
  #:use-module (gnetlist package-pin)
  #:use-module (gnetlist pin-net)

  #:export (verbose-print
            verbose-done
            verbose-print-netlist))

(define verbose-mode (gnetlist-option-ref 'verbose))

(define %char-counter 0)

(define (verbose-print s)
  (when verbose-mode
    (display s)
    (set! %char-counter
          (if (string-suffix? "\n" s)
              0
              (+ %char-counter (string-length s))))
    (if (>= %char-counter 78)
        (verbose-print "\n"))))

(define (verbose-done)
  (verbose-print "\nDONE\n"))


(define (verbose-print-netlist netlist)
  (define (print-net net)
    (let ((package (pin-net-connection-package net))
          (pinnumber (pin-net-connection-pinnumber net)))
      (if (and package pinnumber)
          (format #f "\t\t~A ~A [~A]
"
                  package
                  pinnumber
                  (pin-net-id net))
          "")))

  (define (print-nets net-list)
    (string-join (map print-net net-list) ""))

  (define (print-pin-info pin)
    (format #f "\tpin~A (~A) ~A\n~A\n"
            (or (package-pin-number pin) "?")
            (or (package-pin-label pin) "")
            (or (package-pin-name pin) "Null net name")
            (print-nets (package-pin-nets pin))))

  (define (print-pin-list pin-list)
    (map print-pin-info pin-list))

  (define (print-package-info package)
    (format #f "component ~S
Hierarchy tag: ~S
~A
"
            (or (package-refdes package) "SPECIAL")
            (or (package-tag package) "")
            (print-pin-list (package-pins package))))

  (when verbose-mode
   (format #t "
Internal netlist representation:

~A
"
           (string-join (map print-package-info netlist) ""))))
