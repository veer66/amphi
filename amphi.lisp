;;;; amphi.lisp

(in-package #:amphi)

(defvar *lang-sides* '(:SOURCE :TARGET))

(defun utf16-substr (str s e)
  (-<> str
       (string-to-octets <> :encoding :UTF-16BE)
       (subseq <> (* s 2) (* e 2))
       (octets-to-string <> :encoding :UTF-16BE)))

(defun utf16-len (str)
  (/ (length (string-to-octets str :encoding :UTF-16BE))
     2))

;; (utf16-len "กา𨭎")

(defun get-bi-text (tu)
  (assoc-value tu :BI-TEXT))

(defun get-text-from-tu (tu lang-side)
  (-<> tu
    (get-bi-text <>)
    (assoc-value <> lang-side)))

(defun get-word-id (rtok)
  (assoc-value rtok :WORD-ID))

(defun get-rtoks (bi-rtoks lang-side)
  (assoc-value bi-rtoks lang-side))

(defun update-word-id* (bi-rtoks lang-side)
  (-<> bi-rtoks
    (get-rtoks <> lang-side)
    (update-word-id <>)))

(defun get-bi-rtoks (tu)
  (assoc-value tu :BI-RTOKS))

(defun get-tree (tu)
  (assoc-value tu :TREE))

(defun get-bi-snode (node)
  (assoc-value node :BI-SNODE))

(defun get-snode (bi-snode lang-side)
  (assoc-value bi-snode lang-side))

(defun get-s (r)
  (assoc-value r :S))

(defun get-e (r)
  (assoc-value r :E))

(defun get-text (r)
  (assoc-value r :TEXT))

(defun get-children (node)
  (assoc-value node :CHILDREN))

(defun match-rtok (rtok snode)
  (loop for r in snode
	  thereis (and (<= (get-s r) (get-s rtok))
		       (>= (get-e r) (get-e rtok)))))

;; (match-rtok '(("s" . 10) ("e" . 20)) '((("s" . 1) ("e" . 100))))

(defun select-rtoks (rtoks snode)
  (loop for rtok in rtoks
	if (match-rtok rtok snode)
	  collect rtok))

(defun to-keyword (w)
  (-<> w
    (string-upcase <>)
    (regex-replace-all "_" <> "-")
    (intern <> 'keyword)))

(defun parse-tu (line)
  (jonathan:parse line :as :alist
		       :keyword-normalizer #'to-keyword
		       :normalize-all t))

(defun another-lang-dir (lang-dir)
  (ecase lang-dir
    (:SOURCE :TARGET)
    (:TARGET :SOURCE)))

(defun non-overlap-ranges? (r1 r2)
  (let ((s1 (cdr (assoc :s r1)))
	(e1 (cdr (assoc :e r1)))
	(s2 (cdr (assoc :s r2)))
	(e2 (cdr (assoc :e r2))))
    (or (<= e2 s1)
	(<= e1 s2))))

(defun crop-text (text s e s* e*)
  (let ((s-offset (- s* s))
	(e-offset (- (- e s) (- e e*))))
    (utf16-substr text s-offset e-offset)))

;; (crop-text "ABCD" 10 14 11 13)

(defun crop-range (r s* e*)
  (let ((s (cdr (assoc :s r)))
	(e (cdr (assoc :e r))))
    (loop for (key . val) in r
	  collect
	  (cond
	    ((eq key :s) (cons :s s*))
	    ((eq key :e) (cons :e e*))
	    ((eq key :text) (cons :text (crop-text val s e s* e*)))
	    (t (cons key val))))))

(defun diff-range (r1 r2)
  (let ((s1 (cdr (assoc :s r1)))
	(e1 (cdr (assoc :e r1)))
	(s2 (cdr (assoc :s r2)))
	(e2 (cdr (assoc :e r2))))
    (cond
      ((non-overlap-ranges? r1 r2) r1)
      ((and (>= s1 s2) (<= e1 e2)) nil)
      ((<= s1 s2) (crop-range r1 s1 s2))
      ((> s1 s2) (crop-range r1 e2 e1))
      (t nil))))

(defun diff-snode (snode1 snode2)
  (let ((snode1* nil)
	(snode1** snode1))
    (loop do (loop for r1 in snode1
		   do (let ((r1* r1))
			(loop for r2 in snode2
			      do (setq r1* (diff-range r1 r2)))
			(setq snode1* (cons r1* snode1*))))
	  do (return snode1*))))


;; (diff-snode '(((:text . "ABCD") (:S . 30) (:E . 34)))
;; 	    '())
