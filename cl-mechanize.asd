(in-package :cl-user)

(defpackage :cl-mechanize-asd
  (:use :cl :asdf))

(in-package :cl-mechanize-asd)

(defsystem :cl-mechanize
  :description "A WWW::Mechanize work-alike"
  :author "Joachim Fasting <joachim.fasting@gmail.com>"
  :licence "BSD"
  :version "0.0"
  :serial t
  :components ((:file "packages")
               (:file "cl-mechanize"))
  :depends-on (:puri
               :drakma
               :closure-html
               :cxml-stp
               :cl-ppcre))
