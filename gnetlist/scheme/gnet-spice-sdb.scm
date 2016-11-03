;;; gEDA - GPL Electronic Design Automation
;;; gnetlist - gEDA Netlist
;;; Copyright (C) 1998-2010 Ales Hvezda
;;; Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
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
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;;; MA 02111-1301 USA.
;;
;; --------------------------------------------------------------------------
;;
;; SPICE netlist backend written by S. Gieltjes starts here
;;
;; further modified by W. Kazubski to use scaling parameters for devices
;; other than MOSFETS
;;
;;----------------------------------------------------------------------
;;
;; Started with gnet-spice1.scm by W. Kazubski.  Radically
;; hacked by SDB to support advanced spice netlist generation.
;; Project started 3.5.2003 -- SDB.
;;
;; Details and documentation at http://www.brorson.com/gEDA/SPICE/
;;
;;  Change log:
;;  3.5.2003 -- Started hacking.  SDB.
;;  3.17.2003 -- 2nd version.  Hacked to allow for .SUBCKT files to model ics.
;;               Changed write-ic.  Added get-file-type.  Added
;;               write-subcircuit.  SDB.
;;  3.31.2003 -- 3rd version.  Hacked to enable creating .SUBCKT schematics for
;;               hierarchical circuit modeling.
;;  8.29.2003 -- 4th version.  Include patches from Ken Healy to sort netlist,
;;               code by SDB to use gnetlist command line args in Scheme fcns,
;;               as well as from Theo Deckers to fix strange problem with '.SUBCKT
;;               quoting.
;;  9.9.2003  -- 5th version.  Rearranged code for more organization (I was beginning
;;               to get lost. . . .).  Incorporated changes to handle external SPICE
;;               files more intelligently.  Changed spew to be configurable by setting
;;               -v from the command line.  Placed new fcn debug-spew into gnetlist.scm.
;;               Added -I command line flag.
;;  10.14.2003 -- Bugfixes: Added empty-string? and hacked get-file-type to handle
;;                case where a model file has an empty line before .SUBCKT or .MODEL.
;;                Also modified write-net-names-on-component to gracefully handle
;;                case where not every pin has a pinseq attribute.  Now only outputs
;;                pins with valid pinseq attribute.
;;  12.25.2003 -- Bugfix:  Unswizzled emission of pins from user-defined .subckts.
;;                (Now correctly uses pinseq to define emission order of pins.)  Also
;;                added ability to emit attributes for semiconductors (e.g. area, off,
;;                ic, etc.)  Added in response to user requests.
;;  12.29.2003 -- Two small enhancements requested by Peter Kaiser.
;;  12.29.2003.a -- Minor bugfix.
;;  12.29.2003.b -- Second minor bugfix.
;;  12.29.2003.c -- Change res & cap to incorporate modelname & "area=" attrib.
;;  3.24.2004 -- Bugfixes made to JFET stuff during Feb.  Change released now.
;;  8.22.2004 -- Added command line as first line of file.
;;  8.29.2004 -- Changed sense source naming in controlled sources because the old convention
;;               was confusing ngspice.
;;  10.9.2004 -- Added patches for voltage controlled switches from Peter Kaiser.
;;  3.16.2005 -- Fixed CCCS bug (typo in Vsense) noticed by David Logan
;;  5.16.2005 -- Modified behavior of .INCLUDE directive.  Now by default it just
;;               spits out the string instead of putting the contents of the file
;;               into the SPICE netlist.  You can force insertion of the file using
;;               the -e flag.
;;  6.12.2005 -- Changed order of writing out netlist and .model/.subckt cards to
;;               facilitate use of numparam with ngspice.  Change supplied by
;;               Dominique Michel.
;;  9.11.2005 -- Incorporated patch from Paul Bunyk to enable netlisting of
;;               Josephson junctions and "K" mutual inductances.  Also enabled
;;               netlisting of "COIL" devices as inductors.
;;  12.27.2005 -- Fix bug discovered by John Doty: spice-IO pins with refdes greater
;;                than P9 were sorted incorrectly (as strings).  Now they are sorted
;;                as numbers.
;;  3.10.2006 -- Added "m" attribute to PMOS and NMOS per request of Peter Kaiser.
;;  4.11.2006 --  Changed the .END and .ENDS cards to lowercase.
;;                This fixes bug 1442912. Carlos Nieves Onega.
;;  2.10.2007 -- Various bugfixes.  Also incorporated slotted part
;;               netlist patch from Jeff Mallatt.  SDB.
;;  4.28.2007 -- Fixed slotted part stuff so that it uses pinseq to emit pins.  SDB
;;  1.9.2008 -- Fix slotted part handling to work without a modified pinseq.  pcjc2
;;  1.3.2011 -- Combine write-ic and write-subcircuit with a fix to the unbound
;;              type variable.  Fully document a check for the special "?" value
;;              explaining why it fails silently.  Clean up
;;              write-net-names-on-component to make it a bit more flexible.
;;              Combine write-probe-item and write-net-names-on-component.  Add
;;              a range utility function.  CC
;;  1.13.2011 -- Add four lines of code (and some comments) that allow formaitting strings
;;               to be used for netlisting NGspice device models. CC
;;  6.12.2011 -- Updated the Problematci name=? symbols to name=unknown and removed the
;;               FIXME check for them. This should be a step closer to place holder consistancy. CC
;;
;;**********************************************************************************
;;
;;  Organization of gnet-spice-sdb.scm file:
;;  --  Functions for program housekeeping, handling of calling flags, file manipulation.
;;  --  Functions for handling nets & devices and creating SPICE cards.
;;  --  High-level functions which control program flow.  Note that the program entry
;;      point lives at the very bottom of this file.
;;
;;  Unfortunately, no organization is present beneath this top level. . . .
;;
;;**********************************************************************************


;;**********************************************************************************
;;************  Program housekeeping, handling calling flags, etc.  ****************
;;**********************************************************************************

;; The following is needed to make guile 1.8.x happy.
(use-modules (ice-9 rdelim) (srfi srfi-1))

;; Common functions for the `spice' and `spice-sdb' backends
(load-from-path "spice-common.scm")

;;--------------------------------------------------------------------------------
;; spice-sdb:get-file-info-list-item  -- loops through the model-file list looking
;;  for triplet corresponding to model-name.  If found, it returns the corresponding
;;  list.  If not found, returns #f
;;--------------------------------------------------------------------------------
(define spice-sdb:get-file-info-list-item
  (lambda (model-name file-info-list)
    (if (null? file-info-list)
        '()                                           ;; return #f upon empty list.
                                                      ;; #f replaced with '() by peter
        (let*  ((list-element (car file-info-list))   ;; else process list-item
                (list-elt-model-name (car list-element))
                (list-elt-file-name (cadr list-element))
                (list-elt-file-type (caddr list-element))
               )
          (if (string=? list-elt-model-name model-name)
              list-element                                                        ;; found model-name.  Return list-element.
              (spice-sdb:get-file-info-list-item model-name (cdr file-info-list)) ;; otherwise, recurse.
          )
        )  ;; end of let*
)))


;;--------------------------------------------------------------------------
;; handle-spice-file:  This wraps insert-text-file.
;; Calling form: (handle-spice-file file-name)
;; It looks to see if the -I flag was set at the command line.  If so,
;; it just writes a .INCLUDE card with the file name.  If not,  it calls
;; insert-text-file to stick the file's contents into the SPICE netlist.
;;--------------------------------------------------------------------------
(define spice-sdb:handle-spice-file
  (lambda (file-name)
    (debug-spew (string-append "Handling spice model file " file-name "\n"))
    (if (calling-flag? "include_mode" (gnetlist:get-calling-flags))
        (display (string-append ".INCLUDE " file-name "\n"))       ;; -I found: just print out .INCLUDE card
        (spice-sdb:insert-text-file file-name)                     ;; -I not found: invoke insert-text-file
    )  ;; end of if (calling-flag
))


;;--------------------------------------------------------------------------
;; Given a filename, open the file, get the contents, and dump them
;; into the spice file.
;; Calling form is "(insert-text-file input-file output-file)"
;; The function opens input-file, but assumes that output-file is
;; already open.
;;
;; This function is usually used to include spice models contained in
;; files into the netlist.  Note that it doesn't
;; check the correctness of the spice code in the file -- you're on your own!
;;---------------------------------------------------------------------------
(define spice-sdb:insert-text-file
  (lambda (model-filename)
    (if (file-exists? model-filename)
    (let ((model-file (open-input-file model-filename)) )
      (display (string-append "*vvvvvvvv  Included SPICE model from " model-filename " vvvvvvvv\n"))
      (let while ((model-line (read-line model-file)))
          (if (not (eof-object? model-line))
                   (begin
                     (display (string-append model-line "\n"))
                     (while (read-line model-file))
                   )  ;; end of inner begin
          ) ;; end of if
        )  ;; end of inner let
        (close-port model-file)
        (display (string-append "*^^^^^^^^  End of included SPICE model from " model-filename " ^^^^^^^^\n*\n"))
     ) ;; end of outer let
    (begin
      (message (string-append "ERROR: File '" model-filename "' not found.\n"))
      (primitive-exit 1))
    )
  )
)

;;; Determines the schematic type, ie. a normal schematic or a
;;; .SUBCKT lower level by searching for a "spice-subcircuit-LL"
;;; device amongst PACKAGE-LIST. If such package is found, returns
;;; ".SUBCKT model-name" else return "normal schematic".
(define (spice-sdb:get-schematic-type package-list)
  (define (subckt-name package)
    (and (string=? (gnetlist:get-package-attribute package "device")
                   "spice-subcircuit-LL")
         (string-append ".SUBCKT "
                        (gnetlist:get-package-attribute package
                                                        "model-name"))))
  (any subckt-name package-list))


;;----------------------------------------------------------
;; Extract the modelname from the .SUBCKT modelname line.
;; Just grab the chars from char 8 to the end of the string.
;;---------------------------------------------------------
(define spice-sdb:get-subcircuit-modelname
  (lambda (schematic-type)
    (substring schematic-type 8 (string-length schematic-type))
  )
)


;;-----------------------------------------------------------
;;  This iterates through the schematic and compiles a list of
;;  all spice-IO pins found.  This is used when writing out
;;  a .SUBCKT lower level netlist.
;;-----------------------------------------------------------
(define spice-sdb:get-spice-IO-pins
  (lambda (ls spice-io-package-list)
    (if (null? ls)

        spice-io-package-list        ;; end iteration & return list if ls is empty.

        (let* ((package (car ls))    ;; otherwise process package. . .
               (device (get-device package))
              )
           (if (string=? device "spice-IO")  ;; look for subcircuit label

               ;; we have found a spice-IO pin.
               (spice-sdb:get-spice-IO-pins (cdr ls) (cons package spice-io-package-list))

               ;; no spice-IO pin found.  Iterate . . . .
               (spice-sdb:get-spice-IO-pins (cdr ls) spice-io-package-list)

           ) ;; end of if string=?

        )  ;; end of let*
   ) ;; end if null
 )
)


;;----------------------------------------------------------------
;;  This takes the list of io-pin-packages and sorts it in order of
;;  refdes.
;;  Repaired on 12.27.2005 to correctly sort pin numbers > 9.
;;----------------------------------------------------------------
(define spice-sdb:sort-spice-IO-pins
  (lambda (package-list)
    ;;  Yes, this isn't good Scheme form.  Tough!  Writing this out
    ;;  in a functional programming form would be totally confusing!
    ;;  Note that this fcn requires that
    ;;  each spice-IO pin have the same, single character prefix (i.e. 'P')
    (let* ((char-prefixes              (map car (map string->list package-list)))  ;; Pull off first char (prefix)
           (prefixes                   (map string char-prefixes))                 ;; Make list of strings from prefixes
           (split-numbers-list         (map cdr (map string->list package-list)))  ;; Pull off refdes numbers as list elements
           (string-numbers-list        (map list->string split-numbers-list))      ;; Recombine split up (multidigit) number strings
           (numbers-list               (map string->number string-numbers-list))   ;; Convert strings to numbers for sorting
           (sorted-numbers-list        (sort numbers-list <))                      ;; Sort refdes numbers as numbers
           (sorted-string-numbers-list (map number->string sorted-numbers-list)) ) ;; Create sorted list of refdes strings.

      (map-in-order string-append  prefixes sorted-string-numbers-list)  ;; Laminate prefixes back onto refdes numbers & return.

    )
  )
)


;;----------------------------------------------------------------
;;  Given a list of spice-IO packages (refdeses), this function returns the list
;;  of nets attached to the IOs.
;;----------------------------------------------------------------
(define spice-sdb:get-IO-nets
  (lambda (package-list net-list)
    (if (null? package-list)

        net-list        ;; end iteration & return net-list if ls is empty.

        (let* ((package (car package-list))                  ;; otherwise process package. . .
               (net (car (gnetlist:get-nets package "1")))   ;; get the net attached to pin 1
              )
         ;; now iterate
          (spice-sdb:get-IO-nets (cdr package-list) (cons net net-list))
        )
    ) ;; end of if
  )
)

;;----------------------------------------------------------
;;  This returns a list of all the integers from start to
;;  stop, with the optional step size.
;;  It is similar to perl's range operator '..'
;;----------------------------------------------------------
(define (range start stop . step)
  (if (null? step)
    (iota (+ (- stop start) 1) start)
    (begin
      (set! step (car step))
      (iota (+ (ceiling (/ (- stop start) step)) 1) start step)
    )
  )
)


;;; Given a MODEL-FILENAME, open the file, get the first line,
;;; and see if it is a .MODEL or .SUBCKT file.
;;; Returns either ".MODEL" or ".SUBCKT" or #f if nothing has been found.
;;; The function opens input-file, and closes it when it is done.
(define (spice-sdb:get-file-type model-filename)
  (if (file-exists? model-filename)
      (let ((model-file (open-input-file model-filename)) )
        (let while ((file-line (read-line model-file)) )

          (cond
           ((eof-object? file-line) ;;  Arrived at end of line without finding .MODEL or .SUBCKT.
            #f)

           ((string-null? file-line)
            (while (read-line model-file))) ;; Found empty line.  Iterate before doing anything else.

           ((string-prefix? "*" file-line)
            (while (read-line model-file))) ;; Found *comment.  Iterate.

           ((string-prefix? "." file-line)
            (begin
              (debug-spew "In get-file-type, first-char = .\n") ;; DEBUG stuff
              (cond

               ((string-prefix-ci? ".subckt" file-line) ;; found .subckt as first line.
                ".SUBCKT" )

               ((string-prefix-ci? ".model" file-line) ;; found .model as first line.
                ".MODEL"  )

               (else #f)))) ;; first . spice card is neither .model nor .subckt

           (else
            (while (read-line model-file))))))
      (begin
        (message (string-append "ERROR: File '" model-filename "' not found.\n"))
        (primitive-exit 1))))


;;---------------------------------------------------------------
;;  write prefix if first char of refdes is improper,
;;  eg. if MOSFET is named T1 then becomes MT1 in SPICE
;;---------------------------------------------------------------
(define spice-sdb:write-prefix
    (lambda (package prefix)
      (let ((different-prefix (not (string=? (substring package 0 1) prefix)) )
            (nomunge (calling-flag? "nomunge_mode" (gnetlist:get-calling-flags)) )
           )
        (debug-spew (string-append "Checking prefix.  Package prefix =" (substring package 0 1) "\n"))
        (debug-spew (string-append "                  correct prefix =" prefix "\n"))
        (debug-spew "   nomunge mode = ")
        (debug-spew nomunge)
        (debug-spew (string-append "\n  different-prefix="))
        (debug-spew different-prefix)
        (debug-spew "\n")
        (if (and different-prefix (not nomunge))
            (display prefix) )
      )
    )
)


;;---------------------------------------------------------------
;; spice-sdb:packsort
;;   Sort procedure to order refdes's alphabetically but
;;   keep A? packages at the end of list so SPICE simulation
;;   directives operate correctly.
;;  This fcn written by Ken Healy to enable SPICE netlisting for
;;  Gnucap, which wants A refdes cards (i.e. SPICE directives)
;;  to appear last in the SPICE netlist.  Slightly modified
;;  and incorporated into main spice-sdb release by SDB on 9.1.2003.
;;  To output the netlist in sorted order, use the -s switch
;;  when invoking gnetlist from the command line.  Example:
;;  gnetlist -s -g spice-sdb -o output.spice Schematic.sch
;;  The default behavior (i.e. if -s is not specified) is to do
;;  no sorting.
;;---------------------------------------------------------------
(define spice-sdb:packsort
  (lambda (x y)
    (let ((xdes (string-ref x 0))
          (ydes (string-ref y 0))
          (xnum (string-tail x 1))
          (ynum (string-tail y 1))
         )

      (if (char-ci=? xdes ydes)
          (if (string-ci<? xnum ynum) #t #f)
          (if (char-ci=? xdes #\A) #f
              (if (char-ci=? ydes #\A) #t
                  (if (char-ci<? xdes ydes) #t #f)))))))

(define (string-tail string start)
  (substring string start (string-length string)))


;;---------------------------------------------------------------
;; spice-sdb:sort_refdes?
;;   Returns #t or #f depending upon if -s was discovered in
;;   the calling flags given to gnetlist.   Used in conjunction with
;;   spice-sdb:packsort.
;;   Calling form: (spice-sdb:sort-refdes? (gnetlist:get-calling-flags))
;;   9.1.2003 -- SDB.
;;---------------------------------------------------------------
;;  Note:  I should re-write this to use calling-flag? . . . .
(define spice-sdb:sort-refdes?
  (lambda (calling-flag-list)

    (if (null? calling-flag-list)
          '#f                                             ;; return #f if null list -- sort_mode not found.
          (let* ((calling-pair (car calling-flag-list))   ;; otherwise look for sort_mode in remainder of list.
                 (calling-flag (car calling-pair))
                 (flag-value (cadr calling-pair))  )

            (if (string=? calling-flag "sort_mode")
                flag-value                                               ;; return flag-value if sort_mode found
                (spice-sdb:sort-refdes? (cdr calling-flag-list))    ;; otherwise recurse until sort_mode is found
            )  ;; end if
          )  ;; end of let*
     )  ;; end of if
))


;;**********************************************************************************
;;***************  Dealing with nets, devices, & SPICE cards.    *******************
;;**********************************************************************************


;;----------------------------------------------------------------
;;
;; Write-transistor-diode: writes out component followed by
;; model or model file associated
;; with the component.
;;  This function does the following:
;;   1.  Writes out the correct refdes prefix (if specified and necessary).
;;   2.  Writes out the refdes and nets
;;   3.  Looks for "model-name" attribute. Writes it out if it exists.
;;   4.  If there is no "model-name" attribute, it writes out the "value"
;;       attribute.  If there is no "value" attribute, it writes out "unknown"
;;       and returns, causing the spice simulator to puke when the netlist
;;       is run.  This is important
;;       'cause the spice simulator needs to have some indication of what
;;       model to look for.
;;   5.  Outputs optional attributes attached to device, if any.  Feature
;;       added by SDB on 12.25.2003.
;;   6.  Outputs a new line
;;   7.  Looks for a the "model" attribute.  If it exists, it it writes out
;;       a .MODEL line like this:  .MODEL model-name type (model)
;;
;;----------------------------------------------------------------
(define spice-sdb:write-transistor-diode
  (lambda (package prefix type attrib-list)

    ;; First do local assignments
    (let ((model-name (gnetlist:get-package-attribute package "model-name"))
          (model (gnetlist:get-package-attribute package "model"))
          (value (gnetlist:get-package-attribute package "value"))
          (area (gnetlist:get-package-attribute package "area"))
          (off (gnetlist:get-package-attribute package "off"))
          (model-file (gnetlist:get-package-attribute package "file"))
         )   ;; end of local assignments

   ;; Write out the refdes prefix, if specified and necessary.
      (if prefix
        (spice-sdb:write-prefix package prefix)
      )

   ;; Next we write out the refdes and nets.
      (spice-sdb:write-component-no-value package)

   ;; next look for "model-name" attribute.  Write it out if it exists.
   ;; otherwise look for "value" attribute.
      (if (not (string=? model-name "unknown"))
          (display (string-append model-name " " ))  ;; display model-name if known
          (display (string-append value " ")))       ;; otherwise display value

  ;; Next write out attributes if they exist
  ;; First attribute is area.  It is written as a simple string
      (if (not (string=? area "unknown"))
          (display (string-append area " ")))

  ;; Next attribute is off.    It is written as a simple string
      (if (not (string=? off "unknown"))
          (display (string-append off " ")))

  ;; Write out remaining attributes
      (spice:write-list-of-attributes package attrib-list)

  ;; Now write out newline in preparation for writing out model.
      (newline)

     ;; Now write out any model which is pointed to by the part.
        (cond

     ;; one line model and model name exist
         ( (not (or (string=? model "unknown") (string=? model-name "unknown")))
           (debug-spew (string-append "found model and model-name for " package "\n"))
           (display (string-append ".MODEL " model-name " " type " (" model ")\n")) )

     ;; one line model and component value exist
         ( (not (or (string=? model "unknown") (string=? value "unknown")))
           (debug-spew (string-append "found model and value for " package "\n"))
           (display (string-append ".MODEL " model-name " " type " (" value ")\n")) )

     ;; model file and model name exist
         ( (not (or (string=? model-file "unknown") (string=? model-name "unknown")))
           (debug-spew (string-append "found file and model-name for " package "\n"))
           (debug-spew "I'll deal with the file later . . .\n")
         )

     ;; model file and component value exist
         ( (not (or (string=? model-file "unknown") (string=? value "unknown")))
           (debug-spew (string-append "found file and value for " package "\n"))
           (debug-spew "I'll deal with the file later . . .\n")
         )

         )  ;; close of cond
        )
    )
)


;;----------------------------------------------------------------
;;  write diode
;;  This writes out a valid diode refdes & then calls
;;  the function which writes the rest of the line.
;;----------------------------------------------------------------
(define spice-sdb:write-diode
  (lambda (package)
    (debug-spew (string-append "Found diode.  Refdes = " package "\n"))
    (let ((attrib-list (list "ic" "temp") ))
      (spice-sdb:write-transistor-diode package "D" "D" attrib-list))
  )
)


;;----------------------------------------------------------------
;;  spice-sdb:write-ic
;;  This writes out a valid ic or subcircuit line.
;;  The algorithm is as follows:
;;  1.  Figure out what type of model goes with this part from
;;      file-info-list.  If it isn't listed, look for a MODEL attribute.
;;      If MODEL attribute is attached, write out SPICE card, and then
;;      write out .MODEL on next line.
;;      If no MODEL attribute is attached, just write out what little
;;      we know.  Then return
;;  2.  If the model-name is in the file-info-list, get the associated
;;      file-type.  Compare it against the component's refdes.  If model-type
;;      is .MODEL or .SUBCKT and refdes doesn't begin with a U or X
;;      respectively, prepend the correct prefix to the refdes.
;; 3.   Print out the rest of the line.
;;
;;----------------------------------------------------------------
(define (spice-sdb:write-ic package file-info-list)

    ;; First do local assignments
    (let ((first-char (string (string-ref package 0)))  ;; extract first char of refdes
          (model-name (gnetlist:get-package-attribute package "model-name"))
          (model (gnetlist:get-package-attribute package "model"))
          (value (gnetlist:get-package-attribute package "value"))
          (type  (gnetlist:get-package-attribute package "type"))
          (model-file (gnetlist:get-package-attribute package "file"))
          (list-item (list))
         )   ;; end of local assignments

      (cond
        ((string=? first-char "U") (debug-spew (string-append "Found ic.  Refdes = " package "\n")))
        ((string=? first-char "X") (debug-spew (string-append "Found subcircuit.  Refdes = " package "\n")))
      )

    ;; First, if model-name is empty, we use value attribute instead.
    ;; We do this by sticking the contents of "value" into "model-name".
      (if (string=? model-name "unknown")
          (set! model-name value))

    ;; Now get item from file-info-list using model-name as key
      (set! list-item (spice-sdb:get-file-info-list-item model-name file-info-list))

    ;; check to see if list-item is null.
      (if (null? list-item)

    ;; list-item is null.  Evidently, we didn't discover any files holding this model.
    ;; Instead we look for model attribute
          (if (not (string=? model "unknown"))
            (begin                                     ;; model attribute exists -- write out card and model.
              (debug-spew "Model info not found in model file list, but model attribute exists.  Write out spice card and .model line..\n")
              (spice-sdb:write-component-no-value package)
              (display (string-append model-name "\n" ))
              (display (string-append ".MODEL " model-name " "))
              (if (not (string=? type "unknown")) (display (string-append type " ")))  ;; If no type then just skip it.
              (display (string-append "(" model ")\n"))
            )
            (begin                                     ;; no model attribute either.  Just write out card.
              (debug-spew "Model info not found in model file list.  No model attribute either.  Just write what we know.\n")
              (spice-sdb:write-component-no-value package)
              (display (string-append model-name "\n" ))
            )
          )   ;; end if (not (string=? . . . .

    ;; list-item is not null.  Therefore we process line depending upon contents of list-item
          (let ((file-type (caddr list-item)) )
           (cond
              ;; ---- file holds a model ----
              ((string=? file-type ".MODEL")
               (begin
                (debug-spew (string-append "Found .MODEL with model-file and model-name for " package "\n"))
                 (spice-sdb:write-prefix package "U")  ;; this prepends an "U" to the refdes if needed, since we have a .model
                 (spice-sdb:write-component-no-value package)
                 (display (string-append model-name "\n" ))
                (debug-spew "We'll handle the file contents later . . .\n")
               ))

              ;; ---- file holds a subcircuit ----
              ((string=? file-type ".SUBCKT")
               (begin
                 (debug-spew (string-append "Found .SUBCKT with model-file and model-name for " package "\n"))
                 (spice-sdb:write-prefix package "X")  ;; this prepends an "X" to the refdes if needed, since we have a .subckt
                 (spice-sdb:write-component-no-value package)
                 (display (string-append model-name "\n" ))
                 (debug-spew "We'll handle the file contents later . . .\n")
               ))
           )  ;; close of inner cond
         )   ;; end of inner let
       )  ;; end of if (null? list-item

  ) ;; end of outer let
)


;;-----------------------------------------------------------
;;  write npn bipolar transistor
;;  This writes out a valid transistor refdes & then calls
;;  the function which writes the rest of the line.
;;-----------------------------------------------------------
(define spice-sdb:write-npn-bipolar-transistor
  (lambda (package)
    (debug-spew (string-append "Found npn bipolar transistor.  Refdes = " package "\n"))
    (let ((attrib-list (list "ic" "temp") ))
      (spice-sdb:write-transistor-diode package "Q" "NPN" attrib-list))
  )
)


;;-----------------------------------------------------------
;;  write pnp bipolar transistor
;;-----------------------------------------------------------
(define spice-sdb:write-pnp-bipolar-transistor
  (lambda (package)
    (debug-spew (string-append "Found pnp bipolar transistor.  Refdes = " package "\n"))
    (let ((attrib-list (list "ic" "temp") ))
      (spice-sdb:write-transistor-diode package "Q" "PNP" attrib-list))
  )
)


;;-----------------------------------------------------------
;;  write n-channel jfet transistor
;;-----------------------------------------------------------
(define spice-sdb:write-nfet-transistor
  (lambda (package)
    (debug-spew (string-append "Found n-channel JFET.  Refdes = " package "\n"))
    (let ((attrib-list (list "ic" "temp") ))
      (spice-sdb:write-transistor-diode package "J" "NJF" attrib-list))
  )
)


;;-----------------------------------------------------------
;;  write p-channel jfet transistor
;;-----------------------------------------------------------
(define spice-sdb:write-pfet-transistor
  (lambda (package)
    (debug-spew (string-append "Found p-channel JFET.  Refdes = " package "\n"))
    (let ((attrib-list (list "ic" "temp") ))
      (spice-sdb:write-transistor-diode package "J" "PJF" attrib-list))
  )
)


;;------------------------------------------------------
;;  write pmos transistor
;;------------------------------------------------------
(define spice-sdb:write-pmos-transistor
  (lambda (package)
    (debug-spew (string-append "Found PMOS transistor.  Refdes = " package "\n"))
    (let ((attrib-list (list "l" "w" "as" "ad" "pd" "ps" "nrd" "nrs" "temp" "ic" "m")))
      (spice-sdb:write-transistor-diode package "M" "PMOS" attrib-list))
  )
)


;;------------------------------------------------------
;;  write nmos transistor
;;------------------------------------------------------
(define spice-sdb:write-nmos-transistor
  (lambda (package)
    (debug-spew (string-append "Found NMOS transistor.  Refdes = " package "\n"))
    (let ((attrib-list (list "l" "w" "as" "ad" "pd" "ps" "nrd" "nrs" "temp" "ic" "m")))
      (spice-sdb:write-transistor-diode package "M" "NMOS" attrib-list))
  )
)


;;------------------------------------------------------
;;  write subckt pmos transistor
;;------------------------------------------------------
(define spice-sdb:write-subckt-pmos-transistor
  (lambda (package)
    (debug-spew (string-append "Found PMOS subcircuit transistor.  Refdes = " package "\n"))
    (let ((attrib-list (list "l" "w" "as" "ad" "pd" "ps" "nrd" "nrs" "temp" "ic" "m")))
      (spice-sdb:write-transistor-diode package "X" "PMOS" attrib-list))
  )
)

;;------------------------------------------------------
;;  write subckt nmos transistor
;;------------------------------------------------------
(define spice-sdb:write-subckt-nmos-transistor
  (lambda (package)
    (debug-spew (string-append "Found NMOS subcircuit transistor.  Refdes = " package "\n"))
    (let ((attrib-list (list "l" "w" "as" "ad" "pd" "ps" "nrd" "nrs" "temp" "ic" "m")))
      (spice-sdb:write-transistor-diode package "X" "NMOS" attrib-list))
  )
)
;;------------------------------------------------------------
;;  write mesfet transistor
;;------------------------------------------------------------
;; ************  Fix this!!!!!!!!!!  **************
(define spice-sdb:write-mesfet-transistor
  (lambda (package)
    (spice-sdb:write-transistor-diode package "Z" "MESFET" (list))))  ;; XXXXXX Fix this!!!


;;-----------------------------------------------------------
;;  write voltage controled switch
;;-----------------------------------------------------------
(define spice-sdb:write-vc-switch
  (lambda (package)
    (debug-spew (string-append "Found voltage controlled switch.  Refdes = " package "\n"))
    (let ((attrib-list (list " " ) ))
      (spice-sdb:write-transistor-diode package "S" "SW" attrib-list))
  )
)


;;--------------------------------------------------------------------
;;  write resistor
;;--------------------------------------------------------------------
(define spice-sdb:write-resistor
  (lambda (package)

    (debug-spew (string-append "Found resistor.  Refdes = " package "\n"))

    ;; first write out refdes and attached nets
    (spice-sdb:write-component-no-value package)

    ;; next write out mandatory resistor value if it exists.
    (let ((value (gnetlist:get-package-attribute package "value")))
        (if (not (string=? value "unknown"))
                (display (string-append value " " )))
    )

    ;; next write our model name if it exists
    (let ((model-name (gnetlist:get-package-attribute package "model-name")))
        (if (not (string=? model-name "unknown"))
                (display (string-append model-name " " )))
    )

    ;; next create list of attributes which can be attached to a resistor.
    ;; I include non-standard "area" attrib here per popular demand.
    (let ((attrib-list (list "area" "l" "w" "temp")))
            ;; write the attributes (if any) separately
      (spice:write-list-of-attributes package attrib-list)
      (display " "))  ;; add additional space. . . .

    ;; finally output a new line
    (newline)

  )
)


;;----------------------------------------------------------------------------
;;  write capacitor
;;----------------------------------------------------------------------------
(define spice-sdb:write-capacitor
  (lambda (package)

    (debug-spew (string-append "Found capacitor.  Refdes = " package "\n"))

    ;; first write out refdes and attached nets
    (spice-sdb:write-component-no-value package)

    ;; next write capacitor value, if any.  Note that if the
    ;; component value is not assigned nothing will be written out.
    (let ((value (gnetlist:get-package-attribute package "value")))
        (if (not (string=? value "unknown"))
                (display (string-append value " " )))
    )

    ;; next write capacitor model name, if any.  This is applicable to
    ;; semiconductor caps used in chip design.
    (let ((model-name (gnetlist:get-package-attribute package "model-name")))
        (if (not (string=? model-name "unknown"))
                (display (string-append model-name " " )))
    )

    ;; Next write out attributes if they exist.  Use
    ;; a list of attributes which can be attached to a capacitor.
    ;; I include non-standard "area" attrib here per request of Peter Kaiser.
    (let ((attrib-list (list "area" "l" "w" "ic")))
      (spice:write-list-of-attributes package attrib-list)
            ;; write the off attribute separately
                (display " "))  ;; add additional space. . . .

    (newline)
  )
)


;;----------------------------------------------------------------------------
;;  write inductor
;;----------------------------------------------------------------------------
(define spice-sdb:write-inductor
  (lambda (package)

    (debug-spew (string-append "Found inductor.  Refdes = " package "\n"))

    ;; first write out refdes and attached nets
    (spice-sdb:write-component-no-value package)


    ;; next write inductor value, if any.  Note that if the
    ;; component value is not assigned, then it will write "unknown"
    (let ((value (gnetlist:get-package-attribute package "value")))
                (display value)
    )


    ;; create list of attributes which can be attached to a inductor
    (let ((attrib-list (list "l" "w" "ic")))
      (spice:write-list-of-attributes package attrib-list)

      ;; write the off attribute separately
      (display " "))  ;; add additional space. . . .

    (newline)
  )
)


;;-------------------------------------------------------------------------
;;  write independent voltage source
;;  The behavior of the voltage source is held in the "value" attribute
;;-------------------------------------------------------------------------
(define spice-sdb:write-independent-voltage-source
  (lambda (package)
    (debug-spew (string-append "Found independent voltage source.  Refdes = " package "\n"))

            ;; first write out refdes and attached nets
    (spice-sdb:write-component-no-value package)

            ;; next write voltage value, if any.  Note that if the
            ;; voltage value is not assigned, then it will write "unknown"
    (let ((value (gnetlist:get-package-attribute package "value")))
                (display value)
    )

    (newline)
  )
)


;;-------------------------------------------------------------------------
;;  write independent current source
;;  The behavior of the current source is held in the "value" attribute
;;-------------------------------------------------------------------------
(define spice-sdb:write-independent-current-source
  (lambda (package)

        (debug-spew (string-append "Found independent current source.  Refdes = " package "\n"))

            ;; first write out refdes and attached nets
    (spice-sdb:write-component-no-value package)

            ;; next write current value, if any.  Note that if the
            ;; current value is not assigned, then it will write "unknown"
    (let ((value (gnetlist:get-package-attribute package "value")))
                (display value)
    )

    (newline)
  )
)


;;----------------------------------------------------------------------------
;;  write Josephson junction in wrspice format. Paul Bunyk, Sep 2, 2005
;;----------------------------------------------------------------------------
(define spice-sdb:write-josephson-junction
  (lambda (package)

    (debug-spew (string-append "Found Josephson junction.  Refdes = " package "\n"))

    ;; first write out refdes and attached nets
    (spice-sdb:write-component-no-value package)

    ;; next, add a dummy node for JJ phase. Unlike in Xic netlister, give it
    ;; a reasonable name, not a number, e.g., refdes.
    (display (string-append package " "))

    ;; next write JJ model name, if any.
    (let ((model-name (gnetlist:get-package-attribute package "model-name")))
        (if (not (string=? model-name "unknown"))
                (display (string-append model-name " " )))
    )

    ;; Next write out attributes if they exist.  Use
    ;; a list of attributes which can be attached to a junction.
    (spice:write-list-of-attributes package (list "area"))
    (newline)
  )
)


;;----------------------------------------------------------------------------
;;  write mutual inductance(actually K). Paul Bunyk, Sep 2, 2005
;;----------------------------------------------------------------------------
(define spice-sdb:write-coupling-coefficient
  (lambda (package)

    (debug-spew (string-append "Found mutual inductance.  Refdes = " package "\n"))

    ;; first write out refdes and attached nets (none)
    (spice-sdb:write-component-no-value package)

    ;; next two inductor names and value
    (let ((inductors (gnetlist:get-package-attribute package "inductors"))
          (value (gnetlist:get-package-attribute package "value")) )
        (if (not (string=? inductors "unknown"))
                (display (string-append inductors " " )))
        (if (not (string=? value "unknown"))
                (display (string-append value " " )))

    )

    (newline)
  )
)


;;----------------------------------------------------------------------------
;; write a voltage probe
;;----------------------------------------------------------------------------
(define (spice-sdb:write-probe package)
    ;; fetch only one attr we care about, so far
    (let ((value (gnetlist:get-package-attribute package "value"))
         ) ;; end of local assignments

    (debug-spew (string-append "Found Probe item, refdes = " package "\n"))

    (if (string=? value "unknown")
      (set! value "TRAN"))

    (display (string-append "* Probe device " package " on nets "))
    (spice-sdb:write-net-names-on-component package)
    (newline)
    (display (string-append ".print " value " +"))
    (spice-sdb:write-net-names-on-component package
      (string-join (map (lambda (x) "V(~a)") (gnetlist:get-pins package)) " " 'infix) ) ;; make format string
    (newline)
  ) ;; end of let
) ;; close of define


;;--------------------------------------------------------------------
;; Given a refdes, and optionally a format string, this writes
;; out the nets attached to the component's pins. If it's not called
;; with a format string it looks for one in the net-format attribute,
;; otherwise it writes out the pins unformatted. This is used to write
;; out non-slotted parts.
;;--------------------------------------------------------------------
(define (spice-sdb:write-net-names-on-component refdes . format)

;; get-net-name -- helper function. Called with pinseq, returns net name,
;; unless net name is "ERROR_INVALID_PIN" then it returns false.
    (define (get-net-name pin)
        (set! pin (number->string pin))

;; -------  Super debug stuff  --------
          (if #f
            (begin
              (debug-spew "  In write-net-names-on-component. . . . \n")
              (debug-spew (string-append "     pin-name = " pin "\n"))
              (debug-spew (string-append "     pinnumber = " (gnetlist:get-attribute-by-pinseq refdes pin "pinnumber") "\n"))
              (debug-spew (string-append "     pinseq = " (gnetlist:get-attribute-by-pinseq refdes pin "pinseq")))
              (if (not (string=? pin (gnetlist:get-attribute-by-pinseq refdes pin "pinseq")))
                (debug-spew " <== INCONSISTENT!\n")
                (debug-spew "\n") )
              (debug-spew (string-append "     netname = " (car (spice:get-net refdes (gnetlist:get-attribute-by-pinseq refdes pin "pinnumber"))) "\n"))
          )) ;; if #T for super debugging
;; -------------------------------------

        (set! pin (car (spice:get-net refdes (gnetlist:get-attribute-by-pinseq refdes pin "pinnumber"))))
        (if (string=? pin "ERROR_INVALID_PIN")
          (begin
            (debug-spew (string-append "For " refdes ", found pin with no pinseq attribute.  Ignoring. . . .\n"))
            #f)  ;; begin
        pin)  ;; if
    )  ;; define get-net-name

    ;; First do local assignments
    (let ((netnames (filter-map get-net-name (range 1 (length (gnetlist:get-pins refdes)))))
         )  ;; let
      (if (null? format) ;; Format agument take priority, otherwise use attribute
        (set! format (gnetlist:get-package-attribute refdes "net-format"))
        (set! format (car format)) )
      (if (string=? format "unknown")
        (display (string-join netnames " " 'suffix))               ;; write out nets.
        (apply simple-format (cons #t (cons format netnames))) )   ;; write out nets with format string
    )  ;; let
)


;;-------------------------------------------------------------------
;; Write the refdes and the net names connected to pins on this component.
;; No return, and no component value is written, or extra attribs.
;; Those are handled later.
;;-------------------------------------------------------------------
(define spice-sdb:write-component-no-value
  (lambda (package)
    (display (string-append package " "))  ;; write component refdes
    (spice-sdb:write-net-names-on-component package)
  )
)


;;------------------------------------------------------------
;; Given a refdes, returns the device attribute "value" as string
;; Used when "value" is an optional attribute.
;; Returns "unknown" if not available.
;;------------------------------------------------------------
(define spice-sdb:component-optional-value
  (lambda (package)
    (let ((value (gnetlist:get-package-attribute package "value")))
      (if (not (string=? value "unknown"))
        (string-append value " ")
        ""))))


;;-----------------------------------------------------------
;; Given a refdes, returns the device attribute "model" as string
;;-----------------------------------------------------------
(define spice-sdb:component-model
  (lambda (package)
    (let ((model (gnetlist:get-package-attribute package "model")))
      (if (not (string=? model "unknown"))
        model spice:component-value))))


;;----------------------------------------------------------
;; Include SPICE statements from a SPICE directive block.
;;----------------------------------------------------------
(define spice-sdb:write-directive
  (lambda (package)
             ;; Collect variables used in creating spice code
        (let ((value (gnetlist:get-package-attribute package "value"))
              (file (gnetlist:get-package-attribute package "file"))
             )   ;; end of local assignments

          (debug-spew (string-append "Found SPICE directive box.  Refdes = " package "\n"))

          (cond

              ;; First look to see if there is a value.
           ((not (string=? value "unknown"))
            (begin
              (display (string-append value "\n"))
              (debug-spew (string-append "Appending value = \"" value "\" to output file.\n"))
            ))

              ;; since there is no value, look for file.
           ((not (string=? file "unknown"))
            (begin
              (spice-sdb:insert-text-file file)   ;; Note that we don't wait until the end here.  Is that OK?
              (debug-spew (string-append "Inserting contents of file = " file " into output file.\n"))
            ))

          ) ;; close of cond
        ) ;; close of let
    ) ;; close of lambda
) ;; close of define


;;----------------------------------------------------------
;; Include a file using an .INCLUDE directive
;; Changed on 6.12.2005: to embed the contents of the file,
;; you must call gnetlist with the -e flag set.
;;----------------------------------------------------------
(define spice-sdb:write-include
  (lambda (package)
    (let ((file (gnetlist:get-package-attribute package "file")))

      (debug-spew (string-append "Found SPICE include box.  Refdes = " package "\n"))

      (if (not (string=? file "unknown"))
        (if  (calling-flag? "embedd_mode" (gnetlist:get-calling-flags))
              (begin
                (spice-sdb:insert-text-file file)                 ;; -e found: invoke insert-text-file
                (debug-spew (string-append "embedding contents of file " file " into netlist.\n")))
              (begin
                (display (string-append ".INCLUDE " file "\n"))   ;; -e not found: just print out .INCLUDE card
                (debug-spew "placing .include directive string into netlist.\n"))
          )
        (debug-spew "silently skip \"unknown\" file.\n")
       )
)))


;;----------------------------------------------------------
;; Include an option using an .OPTIONS directive
;;----------------------------------------------------------
(define spice-sdb:write-options
  (lambda (package)
    (debug-spew (string-append "Found .OPTIONS box.  Refdes = " package "\n"))
    (display (string-append ".OPTIONS " (spice:component-value package) "\n"))))


;;----------------------------------------------------------
;; Include a spice model (instantiated as a model box on the schematic)
;;  Two types of model can be included:
;;  1.  An embedded model, which is a one- or multi-line string held in the attribute "model".
;;      In this case, the following attributes are mandatory:
;;      --  model (i.e. list of parameter=value strings)
;;      --  model-name
;;      --  type
;;      In this case, the function creates and formats the correct spice model line(s).
;;  2.  A model held in a file whose name is held in the attribute "file"
;;      In this case, the following attribute are mandatory:
;;      --  file (i.e. list of parameter=value strings)
;;      In this case, the function just opens the file and dumps the contents
;;      into the netlist.
;;----------------------------------------------------------
(define spice-sdb:write-model
  (lambda (package)
             ;; Collect variables used in creating spice code
        (let ((model-name (gnetlist:get-package-attribute package "model-name"))
              (model-file (gnetlist:get-package-attribute package "file"))
              (model (gnetlist:get-package-attribute package "model"))
              (type (gnetlist:get-package-attribute package "type"))
             )   ;; end of local assignments

          (debug-spew (string-append "Found .MODEL box.  Refdes = " package "\n"))

          ;; Now, depending upon what combination of model, model-file, and model-name
          ;; exist (as described above) write out lines into spice netlist.
          (cond
             ;; one model and model name exist
           ( (not (or (string=? model "unknown") (string=? model-name "unknown")))
             (debug-spew (string-append "found model and model-name for " package "\n"))
             (display (string-append ".MODEL " model-name " " type " (" model ")\n")) )

             ;; model file exists
           ( (not (or (string=? model-file "unknown") ))
             (debug-spew (string-append "found model-file for " package "\n"))
             ;; (spice-sdb:insert-text-file model-file)   ;; don't write it out -- it's handled after the second pass.
           )

          )  ;; close of cond
        ) ;; close of let
    ) ;; close of lambda
) ;; close of define


;;-------------------------------------------------------------------
;;  This writes out the default component (i.e. the "device" attribute
;;  was not recognized).  This function does the following:
;;
;;  1.  Gets the refdes (package).
;;  2.  Checks the refdes against a short list of possible values.
;;      Depending upon the refdes, it does the following thing:
;;      A? -- Invokes write-ic. This provides the opportunity for a code model
;;            which may include a .model line.
;;      D? -- Invokes write-diode
;;      Q? -- Invokes write-transistor-diode. (The "type" attribute is <unknown>
;;            in this case so that the spice simulator will barf if the user
;;            has been careless.)
;;      M? -- Same as Q
;;      U? -- Invokes write-ic. This provides the opportunity for a component
;;            model to be instantiated.
;;      X? -- Invokes write-ic.  This provides the opportunity for a component
;;            subcircuit to be instantiated.
;;      V? -- Invokes write-independent-voltage-source
;;      I? -- Invokes write-independent-current-source
;;      Otherwise, it just outputs the refdes, the attached nets, and the
;;      value of the "value" attribute.
;;
;;-------------------------------------------------------------------
(define spice-sdb:write-default-component
  (lambda (package file-info-list)

    (let ((first-char (string (string-ref package 0)) ))  ;; extract first char of refdes.
      (cond
       ((string=? first-char "A") (spice-sdb:write-ic package file-info-list))
       ((string=? first-char "D") (spice-sdb:write-diode package))
       ((string=? first-char "Q") (spice-sdb:write-transistor-diode package #f "<unknown>" (list)))
       ((string=? first-char "M") (spice-sdb:write-transistor-diode package #f "<unknown>" (list)))
       ((string=? first-char "U") (spice-sdb:write-ic package file-info-list))
       ((string=? first-char "V") (spice-sdb:write-independent-voltage-source package))
       ((string=? first-char "I") (spice-sdb:write-independent-current-source package))
       ((string=? first-char "X") (spice-sdb:write-ic package file-info-list))
       (else
        (message (string-append "Found unknown component.  Refdes = " package "\n"))
        (spice-sdb:write-component-no-value package)
        ;; write component value, if components have a label "value=#"
        ;; what if a component has no value label, currently unknown is written
        (display (spice:component-value package))
        (newline)
       )
      ) ;; end cond
     )  ;; end let
  )
)


;;**********************************************************************************
;;***************  High-level functions for program control  ***********************
;;**********************************************************************************

;;----------------------------------------------------------------------
;; write-netlist is passed a list of refdesses (ls).  It uses
;; each refdes to get the corresponding
;; "device" attribute.  Depending upon the device, it then invokes one or another of the
;; spice line output fcns to output a line of the spice netlist.
;; I have enlarged the number of devices it recognizes -- SDB.
;; write the refdes, to the pin# connected net and component
;; value and optional extra attributes
;; check if the component is a special spice component.
;;----------------------------------------------------------------------
(define spice-sdb:write-netlist
  (lambda (file-info-list ls)
     (if (not (null? ls))
      (let* ((package (car ls))             ;; assign package
             (device (get-device package))  ;; assign device.
            )                               ;; end of let* assignments

;; Super debug stuff -- outputs line describing device being processed.
        (debug-spew (string-append "--- checking package = " package "\n"))
        (debug-spew (string-append "    device = " device "\n"))
;; done with debug stuff

        (cond
          ( (string=? device "none"))                 ;; do nothing for graphical symbols.
          ( (string=? device "spice-subcircuit-LL"))  ;; do nothing for subcircuit declaration.
          ( (string=? device "spice-IO"))             ;; do nothing for SPICE IO pins.
          ( (string=? device "SPICE-ccvs")
              (spice:write-ccvs package))
          ( (string=? device "SPICE-cccs")
              (spice:write-cccs package))
          ( (string=? device "SPICE-vcvs")
              (spice:write-vcvs package))
          ( (string=? device "SPICE-vccs")
              (spice:write-vccs package))
          ( (string=? device "SPICE-nullor")
              (spice:write-nullor package))
          ( (string=? device "DIODE")
              (spice-sdb:write-diode package))
          ( (string=? device "PMOS_TRANSISTOR")
              (spice-sdb:write-pmos-transistor package))
          ( (string=? device "NMOS_TRANSISTOR")
              (spice-sdb:write-nmos-transistor package))
          ( (string=? device "PNP_TRANSISTOR")
              (spice-sdb:write-pnp-bipolar-transistor package))
          ( (string=? device "SPICE-PNP")
              (spice-sdb:write-pnp-bipolar-transistor package))
          ( (string=? device "NPN_TRANSISTOR")
              (spice-sdb:write-npn-bipolar-transistor package))
          ( (string=? device "SPICE-NPN")
              (spice-sdb:write-npn-bipolar-transistor package))
          ( (string=? device "PFET_TRANSISTOR")
              (spice-sdb:write-pfet-transistor package))
          ( (string=? device "NFET_TRANSISTOR")
              (spice-sdb:write-nfet-transistor package))
          ( (string=? device "MESFET_TRANSISTOR")
              (spice-sdb:write-mesfet-transistor package))
          ( (string=? device "SPICE-VC-switch")
              (spice-sdb:write-vc-switch package))
          ( (string=? device "RESISTOR")
              (spice-sdb:write-resistor package))
          ( (string=? device "CAPACITOR")
              (spice-sdb:write-capacitor package))
          ( (string=? device "POLARIZED_CAPACITOR")
              (spice-sdb:write-capacitor package))                       ;; change someday
          ( (string=? device "INDUCTOR")
              (spice-sdb:write-inductor package))
          ( (string=? device "COIL")           ;; Added to enable netlisting of coil-*.sym
              (spice-sdb:write-inductor package))
          ( (string=? device "VOLTAGE_SOURCE")
              (spice-sdb:write-independent-voltage-source package)) ;; change someday
          ( (string=? device "CURRENT_SOURCE")
              (spice-sdb:write-independent-current-source package)) ;; change someday
          ( (string=? device "JOSEPHSON_JUNCTION")
              (spice-sdb:write-josephson-junction package))
          ( (string=? device "K")
              (spice-sdb:write-coupling-coefficient package))
          ( (string=? device "model")
              (spice-sdb:write-model package))
          ( (string=? device "options")
              (spice-sdb:write-options package))
          ( (string=? device "directive")
              (spice-sdb:write-directive package))
          ( (string=? device "include")
              (spice-sdb:write-include package))
          ( (string=? device "TESTPOINT")
              (spice-sdb:write-probe package))
          ( (string=? device "SUBCKT_PMOS")
              (spice-sdb:write-subckt-pmos-transistor package))
          ( (string=? device "SUBCKT_NMOS")
              (spice-sdb:write-subckt-nmos-transistor package))
          ( else
              (spice-sdb:write-default-component package file-info-list))
        ) ;; end of cond
        (spice-sdb:write-netlist file-info-list (cdr ls))
         ))))


;;----------------------------------------------------------------------
;; create-file-info-list: This takes as argument the list of packages (refdesses).
;;   It runs through the package list, and for each gets the attributes.  If there is a
;;   "FILE" attribute, it gets the file info & uses it to build the
;;   file-info-list.  When done, it returns the file-info-list.
;;----------------------------------------------------------------------
(define (spice-sdb:create-file-info-list package-list file-info-list)
  (if (null? package-list)
      file-info-list ;; end of packages processed.  Return file-info-list
      (let* ((package (car package-list)) ;; otherwise get next package (i.e. refdes)
             (model (gnetlist:get-package-attribute package "model-name"))
             (model-file (gnetlist:get-package-attribute package "file"))
             ;; Now run a series of checks to see if we should stick
             ;; this file into the file-info-list.
             ;; First check to see if "file" attribute is non-empty,
             ;; then check to see if file is in file-info-list.  If
             ;; file is new, open it, find out what type it is and, if
             ;; file-type is known, push info into file-info-list.
             (file-type (and (not (string-ci=? model-file "unknown"))
                             (not (spice-sdb:in-file-info-list? model-file
                                                                file-info-list))
                             (spice-sdb:get-file-type model-file))))

        (spice-sdb:create-file-info-list
         (cdr package-list)
         (if file-type
             (cons (list model model-file file-type) file-info-list)
             file-info-list)))))


;;; Helper function.  Returns #t if file is already in
;;; file-info-list, otherwise #f.
;;; Assumes file-info-list of form:
;;; ((model1 file1 file-type1) (model2 file2 file-type2) . . . .)
(define (spice-sdb:in-file-info-list? model-file file-info-list)
  (define (is-model-file? elem)
    (string=? (second elem) model-file))
  (any is-model-file? file-info-list))


;;--------------------------------------------------------------
;; Write out spice netlist header
;;--------------------------------------------------------------
(define (spice-sdb:write-top-header)
  (display "*********************************************************\n")
  (display "* Spice file generated by gnetlist                      *\n")
  (display "* spice-sdb version 4.28.2007 by SDB --                 *\n")
  (display "* provides advanced spice netlisting capability.        *\n")
  (display "* Documentation at http://www.brorson.com/gEDA/SPICE/   *\n")
  (display "*********************************************************\n")
)


;;--------------------------------------------------------------
;; Write out .SUBCKT netlist header
;;--------------------------------------------------------------
(define (spice-sdb:write-subcircuit-header)
  (display "*******************************\n")
  (display "* Begin .SUBCKT model         *\n")
  (display "* spice-sdb ver 4.28.2007     *\n")
  (display "*******************************\n")
)


;;---------------------------------------------------------------
;; Write the .END line
;;---------------------------------------------------------------
(define (spice-sdb:write-bottom-footer salutation)
  (display salutation)
  (newline))

(define (print-command-line)
  "Prints gnetlist command line.
Only basename of the command is output.
If gnetlist is called from libtool wrapper,
the name is changed to canonical."
  (define libtool-prefix "lt-")
  (define lt-prefix-length (string-length libtool-prefix))
  (define (remove-lt-prefix name)
    (if (string-prefix? libtool-prefix name)
        (string-drop name lt-prefix-length)
        name))
  (let ((name (remove-lt-prefix (basename (car (program-arguments))))))
    (format #t "* ~A ~A\n"
            name
            (string-join (cdr (program-arguments)) " "))))

;;---------------------------------------------------------------
;; Spice netlist generation
;;   This is the entry point.
;;   Hacked on 3.31.2003 to enable writing out .SUBCKT models -- SDB.
;;   Hacked again in Sept 2003 to enable more intelligent embedding of external
;;       SPICE files into netlist -- SDB.
;;   The algorithm is as follows:
;;   1.  Figure out if there is a .SUBCKT block on the schematic,
;;       or if it is just a normal schematic.
;;       If a .SUBCKT:
;;       -- Write out subcircuit header (a comment identifying the netlister).
;;       -- find all spice-IO pins.  Get a list of the packages.
;;       -- put them in order (ordered by package refdes)
;;       -- get the list of nets attached to the spice-IO pins.
;;       -- write out .SUBCKT line
;;       If a normal schematic:
;;       -- Write out top header (a comment identifying the netlister).
;;   2.  Loop through all components, looking for components with a "file"
;;       attribute.  Every time a "file" attribute is found do this:
;;       --  Open the file and find out what kind of file it is (.SUBCKT or .MODEL).
;;       --  Determine if the file has previously been processed.  If not: stick the
;;           following info into the file-info list: (model-name file-name file-type).
;;           Otherwise just continue.
;;   3.  Loop through all components again, and write out a SPICE card for each.
;;   4.  Afterwards, for each item in the file-info list, open the file, and
;        write its contents into the netlist.
;;   5.  If the schematic-type is .SUBCKT:  write out .ENDS,  Otherwise: write out .END
;;   6.  Close up the SPICE netlist file and return.
;;---------------------------------------------------------------
(define spice-sdb
  (lambda (output-filename)
    ;; Redefine write-net-names-on-component
    (set! spice:write-net-names-on-component spice-sdb:write-net-names-on-component)

;;
;; First find out if this is a .SUBCKT lower level,
;; or if it is a regular schematic.
;;
    (set-current-output-port (gnetlist:output-port output-filename))
    (let* ((subckt? (spice-sdb:get-schematic-type packages))
           (schematic-type (or subckt? "normal schematic"))
           (model-name (spice-sdb:get-subcircuit-modelname schematic-type))
           (file-info-list (list))
          )
      (message "Using SPICE backend by SDB -- Version of 4.28.2007\n")
      (message (string-append "schematic-type = " schematic-type "\n"))

      (if subckt?
      ;; we have found a .SUBCKT type schematic.
          (let* ((io-pin-packages (spice-sdb:get-spice-IO-pins packages (list) ))
                 (io-pin-packages-ordered (spice-sdb:sort-spice-IO-pins io-pin-packages))
                 (io-nets-list (spice-sdb:get-IO-nets io-pin-packages-ordered (list) ))
                )
            (debug-spew "found .SUBCKT type schematic")
      ;; now write out .SUBCKT header and .SUBCKT line
            (spice-sdb:write-subcircuit-header)
            (format #t "~A\n" (string-join (cons schematic-type io-nets-list) " ")))

      ;; Otherwise it's a regular schematic.  Write out command line followed by comments in file header.
          (begin
            (debug-spew "found normal type schematic")
            (print-command-line)
            (spice-sdb:write-top-header)
          )

      ) ;; end of if (not (string=? . . . .


;;
;; Now loop through all devices and process all "FILE" attributes.  Create
;; file-info-list.
;; Thanks to Carlos Nieves Onega for his e-mail to
;; geda-dev which is the genesis of this section.
;;
      (debug-spew "\nMake first pass through design and create list of all model files referenced.\n")
      (set! file-info-list (spice-sdb:create-file-info-list packages file-info-list))
      (debug-spew "Done creating file-info-list.\n\n")


;;
;;  Moved this loop before the next one to get numparam to work with ngspice,
;;  because numparam will at the subckt definition come before the main netlist.
;;  Change suggested by Dominique Michel; implemented in code on 6.12.2005.
;;
;;  Next loop through all items in file-info-list in the SPICE netlist.
;;  For each model-name, open up the corresponding file, and call handle-spice-file
;;  to stick the corresponding stuff into the output SPICE file.
;;
      (debug-spew "Now process the items in model file list -- stick appropriate references to models in output SPICE file.\n")
      (for-each spice-sdb:handle-spice-file (map second file-info-list))
      (debug-spew "Done processing items in model file list.\n")


;;
;; Now write out netlist as before.  But don't write file contents out.
;; **** Modified by kh to sort list of packages so Spice directives, etc. (A?) are output last,
;; **** and in increasing order.
;;
      (debug-spew "Make second pass through design and write out a SPICE card for each component found.\n")
      (display "*==============  Begin SPICE netlist of main design ============\n")
      (if (spice-sdb:sort-refdes? (gnetlist:get-calling-flags))
          (spice-sdb:write-netlist file-info-list (sort packages spice-sdb:packsort))  ;; sort on refdes
          (spice-sdb:write-netlist file-info-list packages)                            ;; don't sort.
      )
      (debug-spew "Done writing SPICE cards . . .\n\n")


;;
;;  Now write out .END(S) of netlist, depending upon whether this schematic is a
;;  "normal schematic" or a .SUBCKT.
;;
      (if subckt?
          (begin
            (spice-sdb:write-bottom-footer (string-append ".ends " model-name))
            (display "*******************************\n")
          )
          (if (not (calling-flag? "no_end_card" (gnetlist:get-calling-flags)))
              (spice-sdb:write-bottom-footer ".end"))
      )


      (debug-spew "\nOutput file is written.  We are done.\n")
   )
;;
;;  Finally, close up and go home.
;;
    (close-output-port (current-output-port))
 )
)


;; Custom get-uref function to append ".${SLOT}" where a component
;; has a "slot=${SLOT}" attribute attached.
;;
;; NOTE: Original test for appending the ".<SLOT>" was this:
;;   (let ((numslots (gnetlist:get-package-attribute package "numslots"))
;;        (slot-count (length (gnetlist:get-unique-slots package)))
;;     (if (or (string=? numslots "unknown") (string=? numslots "0"))
;;
(define get-uref
  (lambda (object)
    (let ((real_uref (gnetlist:get-uref object)))
      (if (null? (get-attrib-value-by-attrib-name object "slot"))
        real_uref
        (string-append real_uref "."
          (car (get-attrib-value-by-attrib-name object "slot")))
      )
    )
  )
)
