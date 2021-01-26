;;;; amphi.lisp

(in-package #:amphi)

(defvar *lang-sides* '(:SOURCE :TARGET))

(defun get-word-id (rtok)
  (-<> rtok
    (assoc :WORD-ID <>)
    (cdr <>)))

(defun get-rtoks (bi-rtoks lang-side)
  (-<> bi-rtoks
    (assoc lang-side <>)
    (cdr <>)))

(defun update-word-id* (bi-rtoks lang-side)
  (-<> bi-rtoks
    (get-rtoks <> lang-side)
    (update-word-id <>)))

(defun get-bi-rtoks (tu)
  (-<> tu
    (assoc :BI-RTOKS <>)
    (cdr <>)))

(defun get-tree (tu)
  (-<> tu
    (assoc :TREE <>)
    (cdr <>)))

(defun get-bi-snode (node)
  (-<> node
    (assoc :BI-SNODE <>)
    (cdr <>)))

(defun get-snode (bi-snode lang-side)
  (-<> bi-snode
    (assoc lang-side <>)
    (cdr <>)))

(defun get-s (r)
  (-<> r
    (assoc :S <>)
    (cdr <>)))

(defun get-e (r)
  (-<> r
    (assoc :E <>)
    (cdr <>)))

(defun get-text (r)
  (-<> r
    (assoc :TEXT <>)
    (cdr <>)))

(defun get-children (node)
  (-<> node
    (assoc :CHILDREN <>)
    (cdr <>)))

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
