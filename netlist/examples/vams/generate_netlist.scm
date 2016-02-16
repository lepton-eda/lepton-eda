;;; ---------------------------------------------------------------------
;;;    Written by Martin Lehmann
;;; ---------------------------------------------------------------------

;;; This file contents two functions to the hot-keys
;;; g-n and g-e, which stand for (g)enerate (n)etlist and - (e)ntity.
;;;

;; The generate-mode will be gave to the gnetlist environment, with help
;; of the -c parameter of gEDA gnetlist.
;; The same is doing with all toplevel attributes of a component (or schematic).
;; they are defined in the variable top-attribs.
;;
;; generate-mode sets :
;;   1 - generate netlist of current schematic
;;   2 - generate entity of selected component, or of toplevel, when non selected
;;
;; get-selected-filename: - returns the whole filename of
;;                          the current-gschem-schematic
;;
;; get-selected-component-attributes: - returns all toplevel attributes
;;                                      of the current entity

;; This function only put together the gnetlist command for the
;; generating-netlist-call.

(use-modules (geda page) (geda object) (geda attrib))

;;; Generate vhdl file name for given schematic NAME.
(define (schematic-name->vhdl-name name)
  (string-join
   (reverse
    (cons "vhdl"
          (cdr
           (reverse
            (string-split (basename name) #\.)))))
   "."))

(define generate-netlist
  (lambda ()
    (let* ((command "")
	   (source-file (page-filename (active-page)))
	   (source-file-length (string-length source-file))
           (target-file (schematic-name->vhdl-name source-file)))

      ;;generating the complex gnetlist command
      (display (getcwd))
      (set! command "lepton-netlist -c '(chdir \"..\") (display (getcwd)) (newline)'")
      (set! command (string-append command " -o " vhdl-path "/" target-file))
      (set! command (string-append command " -g vams " source-file))
      (display "\ngenerating netlist from current schematic\n")
      (display command)
      (newline)
      (system command))))

;; Makes the same like generate-netlist, but its activate a
;; generating-entity-call.

(define generate-entity
 (lambda ()
   (let* ((command "")
	  (top-attribs (get-selected-component-attributes))

	  ;; search the right schematic-file for gnetlist call
	  ;; Is necessary, when the selected component contents a
	  ;; underlying schematic (hierachical schematic)
	  (source-file (which-source-file top-attribs))

	  ;; generates the target-file, like <source-filebasename>.vhdl
	  (target-file (schematic-name->vhdl-name source-file)))

     (system*
       "lepton-netlist"
       "-c"
       (format #f "(chdir \"..\")~
                   (define top-attribs '~a)~
                   (define generate-mode '2)"
                   (list2string top-attribs))
       "-o"
       (format #f "~a/~a"
                  vhdl-path
                  target-file)
       "-g"
       "vams"
       (get-selected-filename)
       ))))


;; HELP FUNCTIONS

;; generates a string from a list.
(define (list2string ls)
  (string-append "(" (string-join ls " ") ")"))

;; search the right source-file, when selected component contents a
;; underlying schematic. which is saved in the source-attribute of
;; this component
(define which-source-file
  (lambda (top-attribs)
    (if (not (null? top-attribs))
	(if (string-prefix? "source=" (car top-attribs))
	    (append (substring (car top-attribs) 7
                               (string-length (car top-attribs))))
	    (which-source-file (cdr top-attribs)))
	(append (get-selected-filename)))))


;; define the default vhdl-path, where netlist- and entity-files are
;; saved to.
(define vhdl-path ".")
(define schematic-path ".")

(define menu-items
  `((,(N_ "Generate _Netlist") generate-netlist "gtk-execute")
    (,(N_ "Generate _Entity") generate-entity "gtk-execute")))

(begin
  ;; (begin...) is necessary here for guile 2.0.
  ;; See 'info guile' "R6RS Incompatibilities" for information on bug related
  ;; to syntax-transformers in top-level programs (N_ is a syntax transformer)
  (add-menu (N_ "_Gnetlist") menu-items))

(global-set-key "G N" 'generate-netlist)
(global-set-key "G E" 'generate-entity)

(display "loaded generate-netlist.scm\n")
