(use-modules (ice-9 rdelim)
             (lepton library)
             (lepton toplevel)
             (netlist))

;;; Helper functions.

;;; Checks, if a line is a comment or blank line.
(define (comment-or-blank-line? l)
  (let ((index (string-index l #\*)))
    (string-every char-set:whitespace
                  (if index
                      (string-take l index)
                      l))))

;;; Gets input from (current-input-port) and transform it into a
;;; list of line strings.
(define (input->line-list)
  (let loop ((ls '())
             (line (read-line)))
    (if (eof-object? line)
        (filter (negate comment-or-blank-line?) (reverse ls))
        (loop (cons line ls) (read-line)))))


(test-begin "spice-noqsi helper functions")
;;; Comment line.
(test-assert (comment-or-blank-line? "  * asdf"))
;;; Empty line.
(test-assert (comment-or-blank-line? ""))
;;; Only whitespaces.
(test-assert (comment-or-blank-line? "   \t "))
;;; Normal line.
(test-assert (not (comment-or-blank-line? " asdf")))
(test-end "spice-noqsi helper functions")


(test-begin "spice-noqsi")

;;; Something like "../../../../liblepton/scheme".
(define srcdir (getenv "srcdir"))
(define top-srcdir (string-append srcdir "/../../"))

;;; Paths. Assuming we're in the liblepton/scheme/ directory.
(define backend (string-append top-srcdir "utils/netlist/scheme/backend/gnet-spice-noqsi.scm"))
(define symbol-lib (string-append top-srcdir "symbols/sym"))
(define schematic-base (string-append top-srcdir "utils/netlist/examples/spice-noqsi/HelloWorld/HelloWorld"))
(define input-schematic (string-append schematic-base ".sch"))
(define output-circuit (string-append schematic-base ".cir"))

;;; Load component libraries.
(reset-component-library)
(component-library-search symbol-lib)
;;; Load backend.
(primitive-load backend)

(test-assert (file-exists? input-schematic))
(test-assert (file-exists? output-circuit))

(define backend-output-string
  (with-output-to-string
    (λ ()
      (with-toplevel
       (make-toplevel)
       (λ ()
         (set-toplevel-schematic! (make-toplevel-schematic (list input-schematic)))
         (spice-noqsi "-"))))))

(test-equal
    (with-input-from-string backend-output-string input->line-list)
  (with-input-from-file output-circuit input->line-list))

(test-end "spice-noqsi")
