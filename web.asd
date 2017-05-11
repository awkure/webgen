;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Lowercase: Yes;  -*-

(defvar *loadswitch* :compile-if-needed)

(unless (find-class 'asdf::cl-file nil)
    (defclass asdf::cl-file (asdf:cl-source-file) ())
    (defmethod asdf::source-file-type ((c asdf::cl-file) (s asdf:module)) "cl"))

(asdf:defsystem "web"
    :name "web"
    :author "awkure, Peter Seibel"
    :maintainer "awkure"
    :license "MIT"
    :description "My collection of css, js and html processors"
    :serial t
    :components ((:cl-file "./lib/package")
                 (:cl-file "./lib/css/package")
                 (:cl-file "./lib/js/package")
                 (:cl-file "./test/package")
                 
                 (:cl-file "./lib/macros"   :depends-on (  "./lib/package"     ))
                 (:cl-file "./lib/core"     :depends-on (  "./lib/package"     ))
                 
                 (:cl-file "./lib/css/css"  :depends-on (  "./lib/css/package" ))
                 (:cl-file "./lib/css/sass" :depends-on (  "./lib/css/package" ))
                 
                 (:cl-file "./lib/js/js"    :depends-on (  "./lib/js/package"  ))
                 
                 (:cl-file "./test/suite"   :depends-on (  "./test/package"    ))
                 (:cl-file "./test/tests"   :depends-on (  "./test/package"  ))
                 
                 (:static-file "./README")
                 (:static-file "./LICENSE")))
