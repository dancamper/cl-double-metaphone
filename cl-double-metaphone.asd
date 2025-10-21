;;;; cl-double-metaphone.asd
;;  ASDF system definition for the Common Lisp port of the Double Metaphone algorithm.
;;
;;  Provides a fully tested implementation of Lawrence Philips’
;;  Double Metaphone phonetic encoding algorithm, plus a comprehensive
;;  FiveAM regression suite included.
;;
;;  Tested on: SBCL

(asdf:defsystem "cl-double-metaphone"
  :description "Common Lisp implementation of the Double Metaphone phonetic algorithm."
  :long-description "Implements Lawrence Philips’ Double Metaphone algorithm in pure Common Lisp.
   Returns primary and secondary phonetic keys for English words.  Useful for
   fuzzy matching, search indexing, and linguistic experiments.  Includes
   a complete FiveAM test suite."
  :author "Dan S. Camper <dan@bti.net>"
  :license "Apache 2.0"
  :version "0.1.0"
  :homepage "https://github.com/dancamper/cl-double-metaphone"
  :serial t
  :components ((:file "package")
               (:file "double-metaphone")))


;; ---------------------------------------------------------------------
;; Test system definition
;; ---------------------------------------------------------------------

(asdf:defsystem "cl-double-metaphone/tests"
  :description "FiveAM test suite for cl-double-metaphone."
  :author "Dan S. Camper <dan@bti.net>"
  :license "Apache 2.0"
  :depends-on ("cl-double-metaphone" "fiveam")
  :components ((:file "double-metaphone-tests"))
  :perform (test-op (op c)
                    (uiop:symbol-call :fiveam :run!
                                      (find-symbol "DOUBLE-METAPHONE-SUITE"
                                                   :double-metaphone-tests))))

