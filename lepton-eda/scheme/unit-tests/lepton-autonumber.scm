(use-modules (lepton autonumber))

(test-begin "autonumber-string->template")

(test-assert (not (autonumber-string->template "ABC" "ok")))
(test-equal "base" (autonumber-string->template "base?" "base"))
(test-equal "base" (autonumber-string->template "base123?" "base"))
(test-equal "base" (autonumber-string->template "base123" "base"))
(test-equal "base" (autonumber-string->template "base?123" "base"))
(test-equal "" (autonumber-string->template "?" ""))

(test-end)
