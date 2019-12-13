; -*-Scheme-*-
(use-modules (lepton library))

; Clean up logfiles:
;
(use-modules (geda log-rotate))
(cleanup-old-logs!)
