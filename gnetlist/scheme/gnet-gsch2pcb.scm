
;;; gEDA - GNU Electronic Design Automation
;;; gnetlist - GNU Netlist
;;; Copyright (C) 1998-2000 Ales V. Hvezda
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
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;  gsch2pcb format  (based on PCBboard format by JM Routoure & Stefan Petersen)
;;  Bill Wilson    billw@wt.net
;;  6/17/2003


;;
;;
(define gsch2pcb:write-top-header
  (lambda (port)
    (display "# release: pcb 1.6.3\n" port)
    (display "PCB(\"\" 6000 5000)\n" port)
    (display "Grid(10 0 0)\n" port)
    (display "Cursor(10 270 3)\n" port)
    (display "Flags(0x000000d0)\n" port)
    (display "Groups(\"1,2,3,s:4,5,6,c:7:8:\")\n" port)
    (display "Styles(\"Signal,10,40,20:Power,25,60,35:Fat,40,60,35:Skinny,8,36,20\")\n" port)))


;;
;;
(define gsch2pcb:write-bottom-footer
  (lambda (port)
    (display "Layer(1 \"solder\")\n(\n)\n" port)
    (display "Layer(2 \"GND-sldr\")\n(\n)\n" port)
    (display "Layer(3 \"Vcc-sldr\")\n(\n)\n" port)
    (display "Layer(4 \"component\")\n(\n)\n" port)
    (display "Layer(5 \"GND-comp\")\n(\n)\n" port)
    (display "Layer(6 \"Vcc-comp\")\n(\n)\n" port)
    (display "Layer(7 \"unused\")\n(\n)\n" port)
    (display "Layer(8 \"unused\")\n(\n)" port)
    (newline port)))

;;
;;

;; Split string at current split-char and returns
;; a pair with substrings. If string is not splitable
;; it returns #f.
(define (string-split the-string split-char)
;;string-index is Guile specific
  (let ((split-index (string-index the-string split-char))
        (last-index (- (string-length the-string) 1)))
;;Check if split-char happens to be in the beginning or end of the string
    (cond ((not split-index)
           #f)
          ((= split-index 0)
           (string-split 
            (substring the-string 1 (string-length the-string)) 
            split-char))
          ((= split-index last-index)
           #f)
          (split-index
           (cons (substring the-string 0 split-index)
                 (substring the-string (+ split-index 1) 
                            (string-length the-string))))
          (else
           #f))))

;; Splits a string with space separated words and returns a list
;; of the words (still as strings).
(define (split-to-list the-string)
  (let ((the-list (string-split the-string #\space)))
    (if the-list
        (cons (car the-list) (split-to-list (cdr the-list)))
        (list the-string))))


(define gsch2pcb:write-value-footprint
  (lambda (pipe ls)
    (if (not (null? ls))
;;       refdes contains the first element of ls        
        (let* ((refdes (car ls))
               (value (gnetlist:get-package-attribute refdes "value"))
               (footprint (split-to-list 
                          (gnetlist:get-package-attribute refdes  "footprint") ) ) )

               (display (string-append "PKG_" (car footprint)) pipe)
               (display (string-append "(" (car footprint)) pipe)
               (case (length footprint)
                 ((1) #f)
                 ((2) (display (string-append "-" (cadr footprint)) pipe))
                 ((3) (display (string-append "-" (cadr footprint) 
                                         "-" (caddr footprint)) pipe))
;; Cases 4 & 5 are a kludge to help with spaces in file element names
;; This makes up to 4 spaces possible and should be generalized
                 ((4) (display (string-append "-" (cadr footprint) 
                           "-" (caddr footprint) "-"
                           (caddr (cdr footprint))) pipe))
                 ((5) (display (string-append "-" (cadr footprint)
                           "-" (caddr footprint)
                           "-" (caddr (cdr footprint)) 
                           "-" (caddr (cdr (cdr footprint)) )) pipe)))
               (display (string-append "," refdes ",") pipe)

               (display value pipe)
               (case (length footprint)                   
                 ((1) #f)  
                 ((2) (display (string-append "," (cadr footprint)) pipe))
                 ((3) (display (string-append "," (cadr footprint) 
                                       "," (caddr footprint)) pipe))
                 ((4) (display (string-append "," (cadr footprint) 
                           "," (caddr footprint)
                           "," (caddr (cdr footprint))) pipe))
                 ((5) (display (string-append "," (cadr footprint)
                           "," (caddr footprint)
                           "," (caddr (cdr footprint)) 
                           "," (caddr (cdr (cdr footprint)) )) pipe))
                 (else (display
                    (string-append "ERROR!, no footprint in device " refdes))))
               (display ")" pipe)
               (newline pipe)
               (gsch2pcb:write-value-footprint pipe (cdr ls))) )))

;;
;;

(define *m4-pcbdir* "/usr/X11R6/lib/X11/pcb/m4")

;; To emulate popen. Guileish again.
; Needed after guile ver. 1.3.2. To save 1.3a users, wrap it in.
(false-if-exception (use-modules (ice-9 popen)))

(define (gsch2pcb output-filename)
  (let ((port (open-output-file output-filename)))
    (gsch2pcb:write-top-header port)
       (close-port port))
       ;; pipe with the macro define in pcb program
;;  (let ((pipe (open-output-pipe (string-append "m4 " *m4-pcbdir* "/common.m4 - | sed '/^PKG/d' - >> " output-filename))))
;;  leave the packages that have not been found in the file.pcb
;;  will be process in the script gschem2pcb
;; Original pipe command commented out by AVH (bugfix by Rich Walker)
;;  (let ((pipe (open-output-pipe (string-append "m4 " *m4-pcbdir* "/common.m4 - >> " output-filename))))
;; Fixed pipe command (AVH 1/27/02)
   (let ((pipe (open-output-pipe (string-append "m4 -d -I" *m4-pcbdir* " " *m4-pcbdir* "/common.m4 - >> " output-filename))))


       ;; packages is a list with the different refdes value
    (gsch2pcb:write-value-footprint pipe packages)
       (close-pipe pipe))
  (let ((port (open output-filename (logior O_WRONLY O_APPEND))))
    (gsch2pcb:write-bottom-footer port)
       close-port port))
