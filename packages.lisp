(in-package :cl-user)

(defpackage :cl-mechanize
  (:use :cl)
  (:nicknames :mechanize :net.browser)
  (:export #:*user-agent*
           #:*cookie-jar*
           #:*history
           #:*page*
           #:*status*
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
           #:fetch
           #:submit
           #:follow
           #:back
           #:reload))

(defpackage :cl-mechanize-user
  (:use :cl :cl-mechanize)
  (:nicknames :mechanize-user :net.browser-user))
