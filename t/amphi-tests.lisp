(defpackage #:amphi-tests
  (:use #:cl #:fiveam #:amphi))

(in-package :amphi-tests)

(def-suite range-suite :description "range operations")
(in-suite range-suite)

(test non-overlap-non-overlap-basic
  (is (amphi::non-overlap-ranges? '((:text . "ABCD") (:S . 10) (:E . 14))
				  '((:text . "ABCD") (:S . 30) (:E . 34)))))

(test non-overlap-completely-overlap
  (is (not (amphi::non-overlap-ranges? '((:text . "ABCD") (:S . 30) (:E . 34))
				       '((:text . "ABCD") (:S . 30) (:E . 34))))))

(test non-overlap-just-connected
  (is (amphi::non-overlap-ranges? '((:text . "ABCD") (:S . 30) (:E . 34))
				  '((:text . "ABCD") (:S . 34) (:E . 38)))))

(test non-overlap-just-connected-swap
  (is (amphi::non-overlap-ranges? '((:text . "ABCD") (:S . 34) (:E . 38))
				  '((:text . "ABCD") (:S . 38) (:E . 42)))))


(test non-overlap-overlap
  (is (not (amphi::non-overlap-ranges? '((:text . "ABCD") (:S . 31) (:E . 35))
				       '((:text . "ABCD") (:S . 34) (:E . 38))))))

(test non-overlap-overlap-swap
  (is (not (amphi::non-overlap-ranges? '((:text . "ABCD") (:S . 34) (:E . 38))
				       '((:text . "ABCD") (:S . 31) (:E . 35))))))

(def-suite diff-suite :description "snode/range diff")
(in-suite diff-suite)

(test diff-identical-ranges
  (is (equal (diff-range '((:text . "ABCD") (:S . 10) (:E . 14))
			 '((:text . "ABCD") (:S . 10) (:E . 14)))
	     '())))

(test diff-disjoint-ranges
  (is (equal (diff-range '((:text . "ABCD") (:S . 10) (:E . 14))
			 '((:text . "ABCD") (:S . 30) (:E . 34)))
	     '((:text . "ABCD") (:S . 10) (:E . 14)))))

(test diff-large-r2
  (is (equal (diff-range '((:text . "ABCD") (:S . 30) (:E . 34))
			 '((:text . "XABCDY") (:S . 29) (:E . 35)))
	     nil)))

(test diff-crop-head
  (is (equal (diff-range '((:text . "ABCD") (:S . 30) (:E . 34))
			 '((:text . "XA") (:S . 29) (:E . 31)))
	     '((:text . "BCD") (:S . 31) (:E . 34)))))

(test diff-crop-tail
  (is (equal (diff-range '((:text . "ABCD") (:S . 30) (:E . 34))
			 '((:text . "DY") (:S . 33) (:E . 35)))
	     '((:text . "ABC") (:S . 30) (:E . 33)))))

(test diff-snode-with-empty-snode
  (is (equal (diff-snode '(((:text . "ABCD") (:S . 30) (:E . 34)))
			 '())
	     '(((:text . "ABCD") (:S . 30) (:E . 34))))))

(test diff-snode-with-identical-one
  (is (equal (diff-snode '(((:text . "ABCD") (:S . 30) (:E . 34)))
			 '(((:text . "ABCD") (:S . 30) (:E . 34))))
	     '())))

(run! 'range-suite)
(run! 'diff-suite)

