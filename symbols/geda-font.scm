;                                                         -*-Scheme-*-
;;;
;;; Define the default gaf font
;;;

(define geda-font-path (build-path geda-data-path "sym" "font"))
(font-directory geda-font-path)

(for-each
 (lambda (fontmap)
   (map-font-character-to-file 
    (car fontmap) 
    (build-path geda-font-path (cdr fontmap))))
 '(
   (" " . "space.sym")
   ("\n" . "newline.sym")
   ("!" . "excl.sym")
   ("," . "comma.sym")
   ("(" . "lparen.sym")
   (")" . "rparen.sym")
   ("-" . "minus.sym")
   ("+" . "plus.sym")
   ("#" . "pound.sym")
   ("?" . "quest.sym")
   ("\"" . "quote.sym")
   (":" . "colon.sym")
   ("@" . "at.sym")
   ("=" . "equal.sym")
   (">" . "more.sym")
   ("<" . "less.sym")
   ("/" . "slash.sym")
   ("$" . "dollar.sym")
   (";" . "semi.sym")
   ("&" . "amper.sym")
   ("\\" . "backslash.sym")
   ("{" . "lbrace.sym")
   ("}" . "rbrace.sym")
   ("'" . "apost.sym")
   ("`" . "backtick.sym")
   ("^" . "caret.sym")
   ("%" . "percent.sym")
   ("[" . "lbrack.sym")
   ("]" . "rbrack.sym")
   ("*" . "astericks.sym")
   (". " . "period.sym")
   ("_" . "under.sym")
   ("~" . "tilde.sym")
   ("|" . "vbar.sym")

   ;; A-umlaut finnish/swedish/german 
   ("Ä" . "A-diaeresis.sym")
   ;; A-ring finnish/swedish/danish/norwegian
   ("Å" . "A-ring.sym")
   ;; AE-diphtong danish/norwegian 
   ("Æ" . "AE-lig.sym")
   ;; O-umlaut finnish/swedish/german
   ("Ö" . "O-diaeresis.sym")
   ;; O-double_acute_accent hungarian 
   ("Ő" . "O-double-acute-accent.sym")
   ;; O-slash danish/norwegian
   ("Ø" . "O-slash.sym")
   ;; U-umlaut german
   ("Ü" . "U-diaeresis.sym")
   ;; U-double_acute_accent hungarian 
   ("Ű" . "U-double-acute-accent.sym")
   ;; a-umlaut finnish/swedish/german 
   ("ä" . "a_-diaeresis.sym")
   ;; a-ring finnish/swedish/danish/norwegian
   ("å" . "a_-ring.sym")
   ;; ae-diphtong danish/norwegian 
   ("æ" . "ae_-lig.sym")
   ;; o-umlaut finnish/swedish/german 
   ("ö" . "o_-diaeresis.sym")
   ;; o-double_acute_accent hungarian 
   ("ő" . "o_-double-acute-accent.sym")
   ;; o-slash danish/norwegian 
   ("ø" . "o_-slash.sym")
   ;; u-umlaut german 
   ("ü" . "u_-diaeresis.sym")
   ;; u-double_acute_accent hungarian 
   ("ű" . "u_-double-acute-accent.sym")
   ;; a-acute_accent spanish 
   ("á" . "a_-acute-accent.sym")
   ;; e-acute_accent spanish
   ("é" . "e_-acute-accent.sym")
   ;; i-acute_accent spanish
   ("í" . "i_-acute-accent.sym")
   ;; o-acute_accent spanish
   ("ó" . "o_-acute-accent.sym")
   ;; u-acute_accent spanish
   ("ú" . "u_-acute-accent.sym")
   ;; A-acute_accent spanish
   ("Á" . "A-acute-accent.sym")
   ;; E-acute_accent spanish
   ("É" . "E-acute-accent.sym")
   ;; I-acute_accent spanish
   ("Í" . "I-acute-accent.sym")
   ;; O-acute_accent spanish
   ("Ó" . "O-acute-accent.sym")
   ;; U-acute_accent spanish
   ("Ú" . "U-acute-accent.sym")
   ;; n-tilde spanish
   ("ñ" . "n_-tilde.sym")
   ;; N-tilde spanish
   ("Ñ" . "N-tilde.sym")
   ;; open exclamation spanish 
   ("¡" . "excl-open.sym")
   ;; open question spanish
   ("¿" . "quest-open.sym")
   ;; 
   ("ą" . "a_-ogonek.sym")
   ("Ą" . "A-ogonek.sym")
   ("ć" . "c_-acute-accent.sym")
   ("Ć" . "C-acute-accent.sym")
   ("ę" . "e_-ogonek.sym")
   ("Ę" . "E-ogonek.sym")
   ("ł" . "l_-slash.sym")
   ("Ł" . "L-slash.sym")
   ("ń" . "n_-acute-accent.sym")
   ("Ń" . "N-acute-accent.sym")
   ("ś" . "s_-acute-accent.sym")
   ("Ś" . "S-acute-accent.sym")
   ("ź" . "z_-acute-accent.sym")
   ("Ź" . "Z-acute-accent.sym")
   ("ż" . "z_-dot.sym")
   ("Ż" . "Z-dot.sym")
   ;; small mu character greek
   ("µ" . "mu_.sym")
   ("Ω" . "Omega.sym")
   ))
