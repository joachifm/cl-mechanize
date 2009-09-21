(in-package :cl-user)

(defpackage :cl-mechanize
  (:use :cl)
  (:nicknames :mechanize :net.browser)
  (:export #:*user-agent*
           #:*cookie-jar*
           #:*history
           #:*page*
           #:*status*
           #:fetch
           #:submit))

(defpackage :cl-mechanize-user
  (:use :cl :cl-mechanize)
  (:nicknames :mechanize-user :net.browser-user))
