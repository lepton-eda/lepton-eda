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

(define generate-netlist
  (lambda ()
    (let* ((command "")
	   (source-file (get-selected-filename))
	   (source-file-length (string-length source-file))
	   
	   ;;generate a sensible output-filename (<old-path>/<old-basefilename>.vhdl)
	   (target-file (string-append
			 (substring source-file 
				    (+ (string-rindex source-file #\/ 0 
						      (string-length source-file)) 1)
				    (- (string-length source-file) 4))
			 ".vhdl")))

      ;;generating the complex gnetlist command
      (display (getenv 'PWD))
      (set! command "gnetlist")
      (set! command (string-append command " -o " vhdl-path "/" target-file))
      (set! command (string-append command " -g vams " source-file))
      (display "\ngenerating netlist from current schematic\n")
      (display command)
      (newline)
      (system command)

      ;; this part is not really important
      ;;(set! command (string-append "dtpad " vhdl-path "/" 
      ;;			   (string-append (substring target-file 0 
      ;;						     (string-rindex 
      ;;					      target-file #\. 0)) 
      ;;			  "_arc.vhdl")
      ;;   " &"))
      ;;(system command)
)))

;; Makes the same like generate-netlist, but its activate a 
;; generating-entity-call.

(define generate-entity
 (lambda ()
   (let* ((command "")
	  (guile-comm  "")
	  (top-attribs (get-selected-component-attributes))
	  
	  ;; search the right schematic-file for gnetlist call
	  ;; Is necessary, when the selected component contents a
	  ;; underlying schematic (hierachical schematic)
	  (source-file (which-source-file top-attribs))

	  ;; generates the target-file, like <source-filebasename>.vhdl
	  (target-file (string-append
			(substring source-file
				   (if (string-rindex source-file #\/ 0 
						     (string-length source-file))
				       (+ (string-rindex source-file #\/ 0 
						     (string-length source-file)) 1)
				       0)
				   (- (string-length source-file) 4))
			".vhdl")))


     ;; put the whole gnetlist command together
     (set! guile-comm 
	   (string-append guile-comm "\"(define top-attribs " "'" 
			  (list2string top-attribs) ") (define generate-mode '2)\"")) 
     (set! command (string-append "gnetlist -c " guile-comm 
				  " -o " vhdl-path "/" target-file
				  " -g vams " (get-selected-filename)))
     (display command)
     (newline)
     (system command)
     
     ;; not important
     ;;(set! command (string-append "dtpad " vhdl-path "/" target-file " &"))
     ;;(if (null? top-attribs)
     ;;	 (system command))

)))


;; HELP FUNCTIONS

;; generates a string from a list. 
(define list2string 
  (lambda (list)
    (let ((string "("))
      (for-each (lambda (element)
		  (set! string (string-append string element " ")))
		list)
      (set! string (string-append string ")"))
      (append string))))


;; search the right source-file, when selected component contents a
;; underlying schematic. which is saved in the source-attribute of
;; this component
(define which-source-file
  (lambda (top-attribs)
    (if (not (null? top-attribs))
	(if (string-prefix=? "source=" (car top-attribs))
	    (begin
	      (append (substring (car top-attribs) 7 
				 (string-length (car top-attribs)))))
	    (which-source-file (cdr top-attribs)))
	(append (get-selected-filename)))))


;; define the default vhdl-path, where netlist- and entity-files are
;; saved to.
(define vhdl-path ".")

(display "loaded generate-netlist.scm\n")
