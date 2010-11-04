(declaim (optimize (debug 3) (safety 3)))

(in-package :cl-mechanize)

(defparameter *user-agent*
  (format nil "cl-mechanize/0.0 (~A ~A)"
          (lisp-implementation-type)
          (lisp-implementation-version))
  "Default User-Agent string.")

(defparameter *cookie-jar-dir* (merge-pathnames "cookies-drakma"
                                                (user-homedir-pathname))
  "Folder where cookies are stored.
Set to NIL to disable storing/loading cookies to/from disk.")

(defparameter *accept-cookies-p* t
  "Accept cookies from visited sites?")

(defparameter *cookie-lifetime-policy* :session
  "How long to keep cookies.

Setting to :EXPIRY keeps cookies until they expire

Setting to :SESSION does not store any cookies.")

(defclass browser ()
  ((user-agent
    :initarg :user-agent
    :initform *user-agent*
    :accessor browser-user-agent
    :documentation "User-Agent string used by GET.")
   (cookie-jar
    :initform (make-instance 'drakma:cookie-jar)
    :accessor browser-cookie-jar
    :documentation "The current cookie jar object. Updated by FETCH.")
   (history
    :initform nil
    :accessor browser-history
    :documentation "A list of visited pages, in chronological order.")
   (page
    :initform nil
    :accessor browser-page
    :documentation "The current page object. Updated by FETCH.")
   (status
    :initform nil
    :accessor browser-status
    :documentation "The HTTP status code of the last request."))
  (:documentation "Encapsulates the browser state."))

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

(defun fetch (uri browser &key (method :get) parameters)
  "Send a request and fetch the response."
  (declare (type (or string puri:uri) uri)
           (type browser browser)
           (type keyword method)
           (type list parameters))
  (multiple-value-bind (body status)
      (drakma:http-request uri
                           :user-agent (browser-user-agent browser)
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
      (let ((page (make-instance 'page
                                 :uri uri
                                 :links (nreverse links)
                                 :forms (nreverse forms)
                                 :dom dom
                                 :content body)))
        (setf (browser-page browser) page
              (browser-status browser) status)
        (push page (browser-history browser))
        page))))

(defun submit (form browser)
  "Submit a form on the current page."
  (declare (type form form)
           (type browser browser))
  (fetch (puri:merge-uris (form-action form)
                          (page-uri (browser-page browser)))
         browser
         :method (form-method form)
         :parameters (form-inputs form)))

(defun follow (link browser)
  "Follow a link on the current page."
  (declare (type link link)
           (type browser browser))
  (fetch (puri:merge-uris (link-uri link)
                          (page-uri (browser-page browser)))
         browser))

(defun back (browser)
  "Go back in history."
  (declare (ignore browser))
   (error "BACK not implemented."))

(defun reload (browser)
  "Repeat the current request."
  (declare (ignore browser))
  (error "RELOAD is not implemented."))
