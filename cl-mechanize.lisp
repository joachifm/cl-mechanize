(in-package :cl-mechanize)

(defvar *user-agent* "SBCL/1.0.31"
  "User agent string used by GET.")
(defvar *cookie-jar* nil
  "The current cookie jar object. Updated automatically by GET.")
(defvar *history* nil
  "A list of pages, in chronological order.")
(defvar *page* nil
  "The current page object. Updated automatically by GET.")
(defvar *status* nil
  "The HTTP status code of the last request.")

(defclass form ()
  ((name
    :initarg :name
    :accessor form-name)
   (action
    :initarg :action
    :accessor form-action)
   (method
    :initarg :method
    :accessor form-method
    :initform :get)
   (inputs
    :initarg :inputs
    :accessor form-inputs
    :initform nil))
  (:documentation "Represents a form element."))

(defclass link ()
  ((text
    :initarg :text
    :accessor link-text)
   (uri
    :initarg :uri
    :accessor link-uri)
   (url
    :initarg :url
    :accessor link-url)
   (tag
    :initarg :tag
    :accessor link-tag
    :initform :a)
   (attrs
    :initarg :attrs
    :accessor link-attrs
    :initform nil))
  (:documentation "Represents a link element."))

(defclass page ()
  ((uri
    :initarg :uri
    :accessor page-uri)
   (links
    :initarg :links
    :accessor page-links
    :initform nil)
   (forms
    :initarg :forms
    :accessor page-forms
    :initform nil)
   (dom
    :initarg :dom
    :accessor page-dom)
   (content
    :initarg :content
    :accessor page-content))
  (:documentation "Contains the result of fetching a page."))

(defun fetch (uri &key (method :get) parameters)
  "Send a request and fetch the response.
Handles cookies and history automatically."
  (when (null *cookie-jar*)
    (setf *cookie-jar*
          (make-instance 'drakma:cookie-jar)))
  (multiple-value-bind (body status)
      (drakma:http-request uri
                           :user-agent *user-agent*
                           :method method
                           :parameters parameters)
    (let ((dom (chtml:parse body (stp:make-builder)))
          (links nil)
          (forms nil))
      (stp:do-recursively (n dom)
        (when (typep n 'stp:element)
          (cond ((equal (stp:local-name n) "a")
                 (push (make-instance 'link
                                      :text (stp:string-value n)
                                      :uri (puri:parse-uri (stp:attribute-value n "href"))
                                      :url (stp:attribute-value n "href")
                                      :tag :a)
                        links))
                ((equal (stp:local-name n) "form")
                 (push (make-instance 'form
                                      :name (stp:attribute-value n "name")
                                      :action (stp:attribute-value n "action"))
                       forms)))))
      (setf *page*
            (make-instance 'page
                           :uri uri
                           :links (nreverse links)
                           :forms (nreverse forms)
                           :dom dom
                           :content body))
      (setf *status* status)
      (push *page* *history*)
      *page*)))

(defun submit (form)
  "Submit a form."
  (declare (type form form))
  (fetch (puri:merge-uris (form-action form)
                          (page-uri *page*))
         :method (form-method form)
         :parameters (form-inputs form)))

(defun follow (link)
  "Follow a link."
  (declare (type link link))
  (fetch (puri:merge-uris (link-uri link)
                          (page-uri *page*))))

(defun back ()
  "Go back in history."
  (pop *history*)
  (let ((prev (car *history*)))
    (let ((*history* nil))
      (follow prev))))
