;;;; amphi.asd

(asdf:defsystem #:amphi
  :description "A library for manipulating Amphigram"
  :author "Vee Satayamas <5ssgdxltv@relay.firefox.com>"
  :license "GPL-3.0"
  :version "0.0.1"
  :serial t
  :depends-on (#:arrow-macros #:cl-ppcre #:jonathan #:alexandria)
  :components ((:file "package")
               (:file "amphi")))
