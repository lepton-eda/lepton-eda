;;; ---------------------------------------------------------------------
;;;    Written by Martin Lehmann
;;; ---------------------------------------------------------------------

;;; Copyright (C) 2000,2016 gEDA Contributors
;;; Copyright (C) 2017-2020 Lepton EDA Contributors

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
;; get-selected-component-attributes: - returns all toplevel attributes
;;                                      of the current entity

;; This function only put together the gnetlist command for the
;; generating-netlist-call.

(use-modules (ice-9 format)
             (lepton attrib)
             (lepton log)
             (lepton object)
             (lepton page))

;;; Replace extension in the basename of a file NAME with EXT
(define (replace-extension #;in name #;with ext)
  (string-join
   (reverse
    (cons ext
          (cdr (reverse (string-split name #\.)))))
   "."))

;;; Composes lepton-netlist command.
(define (make-netlist-command working-directory
                              input-file-name
                              output-file-name)
  (list "lepton-netlist"
        "-c"
        (format #f "(chdir \"~a\")" working-directory)
        "-g"
        "vams"
        "-o"
        output-file-name
        input-file-name))

;;; Generates full VAMS netlist.
(define (generate-netlist)
  (let* ((sch-name (page-filename (active-page)))
         (vhdl-name (replace-extension (basename sch-name) "vhdl"))
         (command (make-netlist-command
                   ".."
                   sch-name
                   (string-append vhdl-path
                                  file-name-separator-string
                                  vhdl-name))))
    (log! 'message
          (format #f "Generate netlist from current schematic\n~A\n"
                  command))
    (apply system* command)))


;; Makes the same like generate-netlist, but its activate a
;; generating-entity-call.

(define (generate-entity)
  ;; Helper procedure searching for a proper source file with
  ;; underlying schematic for selected component if it has the
  ;; "source=" attribute.
  (define (which-source-file top-attribs)
    (if (not (null? top-attribs))
        (if (string-prefix? "source=" (car top-attribs))
            (append (substring (car top-attribs) 7
                               (string-length (car top-attribs))))
            (which-source-file (cdr top-attribs)))
        (page-filename (active-page))))

  (let* ((sch-name (page-filename (active-page)))
         (top-attribs (get-selected-component-attributes))
         (source-name (which-source-file top-attribs))
         ;; generates the vhdl-name, like <source-filebasename>.vhdl
         (vhdl-name (replace-extension (basename source-name) "vhdl"))
         (command (list "lepton-netlist"
                        "-c"
                        (format #f "(chdir \"..\")~
                                    (define top-attribs '~a)~
                                    (define generate-mode '2)"
                                top-attribs)
                        "-o"
                        (string-append vhdl-path
                                       file-name-separator-string
                                       vhdl-name)
                        "-g"
                        "vams"
                        sch-name)))

    (log! 'message (format #f "Generate entity for ~A:\n~A\n"
                           sch-name
                           command))
    (apply system* command)))

;; define the default vhdl-path, where netlist- and entity-files are
;; saved to.
(define vhdl-path ".")

;;; Define menu for gnetlist invocation.
(define menu-items
  `((,(N_ "Generate _Netlist") generate-netlist "gtk-execute")
    (,(N_ "Generate _Entity") generate-entity "gtk-execute")))

;;; Actually add the menu defined above.
(begin
  ;; (begin...) is necessary here for guile 2.0.
  ;; See 'info guile' "R6RS Incompatibilities" for information on bug related
  ;; to syntax-transformers in top-level programs (N_ is a syntax transformer)
  (add-menu (N_ "_Gnetlist") menu-items))

;;; Shortcuts.
(global-set-key "G N" 'generate-netlist)
(global-set-key "G E" 'generate-entity)

(log! 'message "Loaded generate-netlist.scm\n")
