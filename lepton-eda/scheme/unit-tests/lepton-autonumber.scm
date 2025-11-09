(use-modules (system foreign))
(use-modules (lepton autonumber))

(test-begin "autonumber-string->template")

(test-assert
    (null-pointer?
     (autonumber-string->template (string->pointer "ABC")
                                  (string->pointer "ok"))))
(test-equal "base"
  (pointer->string
   (autonumber-string->template (string->pointer "base?")
                                (string->pointer "base"))))
(test-equal "base"
  (pointer->string
   (autonumber-string->template (string->pointer "base123?")
                                (string->pointer "base"))))
(test-equal "base"
  (pointer->string
   (autonumber-string->template (string->pointer "base123")
                                (string->pointer "base"))))
(test-equal "base"
  (pointer->string
   (autonumber-string->template (string->pointer "base?123")
                                (string->pointer "base"))))

(test-assert
    (string-null?
     (pointer->string
      (autonumber-string->template (string->pointer "?")
                                   (string->pointer "")))))

(test-end)
