(in-package :cl-user)

(defpackage :cl-mechanize
  (:use :cl)
  (:nicknames :browser :mechanize :net.browser)
  (:export #:*user-agent*
           #:form
           #:form-name
           #:form-action
           #:form-method
           #:form-inputs
           #:link
           #:link-text
           #:link-uri
           #:link-tag
           #:link-attrs
           #:page
           #:page-uri
           #:page-links
           #:page-forms
           #:page-dom
           #:page-content
           #:get-history
           #:get-cookies
           #:get-page
           #:get-status
           #:fetch
           #:submit
           #:follow
           #:back
           #:reload))

(defpackage :cl-mechanize-user
  (:use :cl :cl-mechanize)
  (:nicknames :browser-user :mechanize-user :net.browser-user))
