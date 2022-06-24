(defpackage #:amphi-tests
  (:use #:cl #:fiveam #:amphi))

(in-package :amphi-tests)

(def-suite diff :description "snode/range diff")

(in-suite diff)

(test diff-identical-ranges
  (is (equal (diff-range '((:text . "ABCD") (:S . 10) (:E . 14))
			 '((:text . "ABCD") (:S . 10) (:E . 14)))
	     '())))

(run! 'diff)
