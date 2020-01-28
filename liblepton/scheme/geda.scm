; -*-Scheme-*-
(use-modules (lepton library))

; Clean up logfiles:
;
(use-modules (lepton log-rotate))
(cleanup-old-logs!)
