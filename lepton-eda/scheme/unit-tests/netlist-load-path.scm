(test-begin "load-path")
(test-equal
    %load-path
  (with-input-from-string (getenv "INITIAL_GUILE_LOAD_PATH") read))
(test-end "load-path")
