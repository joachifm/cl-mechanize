# About
CL-MECHANIZE is a [WWW::Mechanize] work-alike for Common Lisp
implemented as a thin wrapper around [Drakma].

[WWW::Mechanize]: http://search.cpan.org/dist/WWW-Mechanize/lib/WWW/Mechanize.pm
[Drakma]: http://weitz.de/drakma

# Getting
`git clone git://github.com/joachifm/cl-mechanize.git cl-mechanize`

# Usage

    $ cd cl-mechanize
    $ sbcl

    ;; Load system
    (require :asdf)
    (asdf:operate 'asdf:load-op :cl-mechanize)
    (in-package :cl-mechanize-user)

    ;; Do a google search
    (fetch "http://www.google.com")
    (let* ((page (get-page))
           (search-form (car (page-forms page))))
      (setf (form-inputs search-form)
            '(("q" . "google")))
      (submit search-form)
      (format t "~A~%" (ppcre:all-matches-as-strings "<title>[a-z].*</title>"
                                                     (page-content page)))
      (dolist (link (page-links page))
        (format t "~A~%" (link-text link))))

    ;; Traverse the DOM
    (stp:do-recursively (n (page-dom (get-page)))
      ..)
