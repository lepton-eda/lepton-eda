(use-modules (lepton rc))

(test-begin "build-path" 2)

(test-equal "prefix/suffix"
  (build-path "prefix" "suffix"))

(test-equal "/path/to/a/directory"
  (build-path "/path" "to" "a" "directory"))

(test-end "build-path")
