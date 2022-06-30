(defpackage #:amphi-tests
  (:use #:cl #:fiveam #:amphi))

(in-package :amphi-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; RANGE SUITE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DIFF RANGE SUITE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-suite diff-range-suite :description "range diff")
(in-suite diff-range-suite)

(test diff-identical-ranges
  (is (equal (diff-range '((:text . "ABCD") (:S . 10) (:E . 14))
			 '((:text . "ABCD") (:S . 10) (:E . 14)))
	     '())))

(test diff-disjoint-ranges
  (is (equal (diff-range '((:text . "ABCD") (:S . 10) (:E . 14))
			 '((:text . "ABCD") (:S . 30) (:E . 34)))
	     '(((:text . "ABCD") (:S . 10) (:E . 14))))))

(test diff-large-r2
  (is (equal (diff-range '((:text . "ABCD") (:S . 30) (:E . 34))
			 '((:text . "XABCDY") (:S . 29) (:E . 35)))
	     '())))

(test diff-crop-head
  (is (equal (diff-range '((:text . "ABCD") (:S . 30) (:E . 34))
			 '((:text . "XA") (:S . 29) (:E . 31)))
	     '(((:text . "BCD") (:S . 31) (:E . 34))))))

(test diff-crop-head-by-one-match
  (is (equal (diff-range '((:text . "ABCD") (:S . 30) (:E . 34))
			 '((:text . "A") (:S . 30) (:E . 31)))
	     '(((:text . "BCD") (:S . 31) (:E . 34))))))

(test diff-crop-tail
  (is (equal (diff-range '((:text . "ABCD") (:S . 30) (:E . 34))
			 '((:text . "DY") (:S . 33) (:E . 35)))
	     '(((:text . "ABC") (:S . 30) (:E . 33))))))

(test diff-split-in-the-middle
  (is (equal (diff-range '((:text . "ABCD") (:S . 30) (:E . 34))
			 '((:text . "BC") (:S . 31) (:E . 33)))
	     '(((:text . "A") (:S . 30) (:E . 31))
	       ((:text . "D") (:S . 33) (:E . 34))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DIFF SNODE SUITE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-suite diff-snode-suite :description "snode diff")
(in-suite diff-snode-suite)

(test diff-snode-with-empty-snode
  (is (equal (diff-snode '(((:text . "ABCD") (:S . 30) (:E . 34)))
			 '())
	     '(((:text . "ABCD") (:S . 30) (:E . 34))))))

(test diff-snode-with-identical-one
  (is (equal (diff-snode '(((:text . "ABCD") (:S . 30) (:E . 34)))
			 '(((:text . "ABCD") (:S . 30) (:E . 34))))
	     '())))

(test diff-snode-with-partial-match
  (is (equal (diff-snode '(((:text . "ABCD") (:S . 30) (:E . 34)))
			 '(((:text . "D") (:S . 33) (:E . 34))))
	     '(((:text . "ABC") (:S . 30) (:E . 33))))))

(test diff-snode-with-two-partial-matches
  (is (equal (diff-snode '(((:text . "ABCD") (:S . 30) (:E . 34)))
			 '(((:text . "A") (:S . 30) (:E . 31))
			   ((:text . "D") (:S . 33) (:E . 34))))
	     '(((:text . "BC") (:S . 31) (:E . 33))))))

(test diff-snode-with-two-partial-matches-and-two-ranges-output
  (is (equal (diff-snode '(((:text . "ABCD") (:S . 30) (:E . 34)))
			 '(((:text . "A") (:S . 30) (:E . 31))
			   ((:text . "C") (:S . 32) (:E . 33))))
	     '(((:text . "B") (:S . 31) (:E . 32))
	       ((:text . "D") (:S . 33) (:E . 34))))))

(test diff-snode-triple-splits
  (is (equal (diff-snode '(((:text . "ABCDEFG") (:S . 30) (:E . 37)))
			 '(((:text . "D") (:S . 33) (:E . 34))
			   ((:text . "B") (:S . 31) (:E . 32))
			   ((:text . "F") (:S . 35) (:E . 36))))
	     '(((:text . "A") (:S . 30) (:E . 31))
	       ((:text . "C") (:S . 32) (:E . 33))
	       ((:text . "E") (:S . 34) (:E . 35))
	       ((:text . "G") (:S . 36) (:E . 37))))))


(run! 'range-suite)
(run! 'diff-range-suite)
(run! 'diff-snode-suite)



