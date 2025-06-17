;;; Deprecated module.

(define-module (geda attrib)
  #:use-module (geda deprecated)
  #:use-module (lepton attrib)

  #:re-export (parse-attrib
               attrib-name
               object-attribs
               attrib-attachment
               promotable-attribs
               attribute?
               attrib-value
               set-attrib-value!
               inherited-attribs
               promote-attribs!
               attrib-inherited?
               attach-attribs!
               detach-attribs!))

(deprecated-module-log-warning! "(lepton attrib)")
