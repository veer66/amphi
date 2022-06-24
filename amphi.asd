;;;; amphi.asd

(asdf:defsystem #:amphi
  :description "A library for manipulating Amphigram"
  :author "Vee Satayamas <5ssgdxltv@relay.firefox.com>"
  :license "GPL-3.0"
  :version "0.0.1"
  :serial t
  :depends-on (#:arrow-macros #:cl-ppcre #:jonathan #:alexandria)
  :components ((:file "package")
	       (:file "amphi"))
  :in-order-to ((test-op (test-op :amphi/test))))

(asdf:defsystem #:amphi/test
		:description "amphi library tester"
		:author "Vee Satayamas"
		:license "GPL-3.0"
		:serial t
		:depends-on (#:amphi #:fiveam)
		:pathname "t"
		:components ((:file "amphi-tests"))
		:perform (test-op (o s) (symbol-call :fiveam :run!)))
