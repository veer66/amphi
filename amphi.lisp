;;;; amphi.lisp

(in-package #:amphi)

(defvar *lang-sides* '(:SOURCE :TARGET))

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
