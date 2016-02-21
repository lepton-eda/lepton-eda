; Copyright (C) 2016 Roland Lutz
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software Foundation,
; Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

(define (apitest:run cmd port)
  (simple-format port "~S\n" cmd)
  (let ((result (primitive-eval cmd)))
    (if (list? result)
	(begin
	  (display "<<<<<\n" port)
	  (for-each (lambda (x) (simple-format port "~S\n" x)) result)
	  (display ">>>>>\n" port))
	(simple-format port "~S\n" result)))
  (display "\n" port))

(define (apitest output-filename)
  (let ((port (open-output-file output-filename)))
    (apitest:run '(gnetlist:get-packages "placeholder") port)
    (apitest:run '(gnetlist:get-non-unique-packages "placeholder") port)
    (apitest:run '(gnetlist:get-all-nets "placeholder") port)
    (apitest:run '(gnetlist:get-all-unique-nets "placeholder") port)

    (apitest:run '(gnetlist:get-toplevel-attribute "tnMERqW0") port)

    (for-each
     (lambda (refdes)
       (apitest:run `(gnetlist:get-pins ,refdes) port)
       (apitest:run `(gnetlist:get-pins-nets ,refdes) port)
       (apitest:run `(gnetlist:vams-get-package-attributes ,refdes) port)
       (apitest:run `(gnetlist:get-all-package-attributes
		      ,refdes "device") port)

       (for-each
	(lambda (pin)
	  (apitest:run `(gnetlist:get-nets ,refdes ,pin) port)

	  (apitest:run `(gnetlist:get-attribute-by-pinnumber
			 ,refdes ,pin "pinnumber") port)
	  (apitest:run `(gnetlist:get-attribute-by-pinnumber
			 ,refdes ,pin "pinseq") port)
	  (apitest:run `(gnetlist:get-attribute-by-pinnumber
			 ,refdes ,pin "pinlabel") port)
	  (apitest:run `(gnetlist:get-attribute-by-pinnumber
			 ,refdes ,pin "pintype") port)

	  (let ((pinseq (gnetlist:get-attribute-by-pinnumber
			 refdes pin "pinseq")))
	    (apitest:run `(gnetlist:get-attribute-by-pinseq
			   ,refdes ,pinseq "pinnumber") port)
	    (apitest:run `(gnetlist:get-attribute-by-pinseq
			   ,refdes ,pinseq "pinseq") port)
	    (apitest:run `(gnetlist:get-attribute-by-pinseq
			   ,refdes ,pinseq "pinlabel") port)
	    (apitest:run `(gnetlist:get-attribute-by-pinseq
			   ,refdes ,pinseq "pintype") port)))
	(gnetlist:get-pins refdes)))
     (gnetlist:get-packages "placeholder"))

    (apitest:run '(gnetlist:get-pins "tnMERqW0") port)
    (apitest:run '(gnetlist:get-pins-nets "tnMERqW0") port)
    (apitest:run '(gnetlist:vams-get-package-attributes "tnMERqW0") port)

    (for-each
     (lambda (netname)
       (apitest:run `(gnetlist:get-all-connections ,netname) port)
       (apitest:run `(gnetlist:graphical-objs-in-net-with-attrib-get-attrib
		      ,netname "device=DRC_Directive" "value") port))
     (gnetlist:get-all-unique-nets "placeholder"))

    (apitest:run '(gnetlist:get-all-connections "tnMERqW0") port)
    (apitest:run '(gnetlist:graphical-objs-in-net-with-attrib-get-attrib
		   "tnMERqW0" "device=DRC_Directive" "value") port)

    (apitest:run '(gnetlist:get-backend-arguments) port)
    ;(apitest:run '(gnetlist:get-input-files) port)
    (apitest:run '(gnetlist:get-verbosity) port)

    (close-output-port port)))
