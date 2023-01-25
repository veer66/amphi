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

(defun text-range-eq? (r0 r1)
  (and (eq (get-s r0) (get-s r1))
       (eq (get-e r0) (get-e r1))))

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

;; 57 58
;; (crop-range '((:S . 57) (:E . 69) (:TEXT . "👎the reason")) 57 58)

(defun crop-range (r s* e*)
  (let ((s (cdr (assoc :s r)))
	(e (cdr (assoc :e r))))
    (loop for (key . val) in r
	  collect
	  (case key
		(:s (cons :s s*))
		(:e (cons :e e*))
		(:text (cons :text (crop-text val s e s* e*)))
		(otherwise (cons key val))))))

(defun diff-range (r1 r2)
  (let ((s1 (cdr (assoc :s r1)))
	(e1 (cdr (assoc :e r1)))
	(s2 (cdr (assoc :s r2)))
	(e2 (cdr (assoc :e r2))))
    (cond
      ((non-overlap-ranges? r1 r2) (list r1))
      ((and (<= s2 s1) (>= e2 e1)) '())
      ((and (> s2 s1) (< e2 e1)) (list (crop-range r1 s1 s2)
				       (crop-range r1 e2 e1)))
      ((< s1 s2) (list (crop-range r1 s1 s2)))
      ((eq s1 s2) (list (crop-range r1 e2 e1)))
      ((> s1 s2) (list (crop-range r1 e2 e1)))
      (t '()))))

(defun diff-snode-by-range (snode r)
  (loop for r1* in snode
	nconc (diff-range r1* r)))

(defun diff-range-by-snode (r snode)
  (reduce #'diff-snode-by-range
	  snode
	  :initial-value (list r)))

(defun diff-snode-when-snode2-is-not-null (snode1 snode2)
  (sort (loop for r1 in snode1
	      nconc (diff-range-by-snode r1 snode2))
	#'< :key #'(lambda (r) (cdr (assoc :s r)))))

(defun diff-snode (snode1 snode2)
  (if snode2
      (diff-snode-when-snode2-is-not-null snode1 snode2)
      snode1))

(defun fully-cover? (snode1 snode2)
  (null (diff-snode snode2 snode1)))

(defun sort-snode (snode attr)
  (flet ((key-fn (r)
	   (cdr (assoc attr r))))
    (sort (copy-list snode) #'< :key #'key-fn)))

(defmacro update-alist-values ((val alist) &rest cases)
  (let ((key (gensym "UPDATE-ALIST-"))
	(extended-cases (append cases `((otherwise ,val)))))
    `(mapcar (f_ (let ((,key (car _))
		       (,val (cdr _)))
		   (cons ,key (case ,key
				,@extended-cases))))
	     ,alist)))

(defun update-every-sub-tree (node update-fn &rest rest)
  (let ((updated-children (loop for child in (cdr (assoc :children node))
				collect (apply #'update-every-sub-tree
					       (append (list child update-fn)
						       rest))))
	(updated-node (apply update-fn (cons node rest))))
    (update-alist-values (v updated-node)
       (:CHILDREN updated-children))))

(defun sort-snodes-in-bi-rtoks (bi-rtoks attr)
  (update-alist-values (v bi-rtoks)
     (:SOURCE #1=(sort-snode v attr))
     (:TARGET #1#)))

(defun sort-snodes-in-bi-snode (bi-snode attr)
  (update-alist-values (v bi-snode)
     (:SOURCE #1=(sort-snode v attr))
     (:TARGET #1#)))

(defun sort-snodes-in-node (node attr)
  (update-alist-values (v node)
     (:BI-SNODE (sort-snodes-in-bi-snode v attr))))

(defun sort-snodes-in-tu (tu attr)
  (update-alist-values (v tu)
     (:TREE (update-every-sub-tree v #'sort-snodes-in-node attr))
     (:BI-RTOKS (sort-snodes-in-bi-rtoks v attr))))


(defun snode-in-snode? (snode1 snode2)
    (null (diff-snode snode1 snode2)))
