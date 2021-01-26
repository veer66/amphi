;;;; amphi.asd

(asdf:defsystem #:amphi
  :description "Describe amphi here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:arrow-macros #:cl-ppcre #:jonathan #:alexandria)
  :components ((:file "package")
               (:file "amphi")))
