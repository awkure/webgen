;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Lowercase: Yes;  -*- ;;;

(in-package :cl-user)

(defpackage #:document-cludg 
  (:documentation "The suite needed to generate documentation via cldoc")
  (:use #:cl))

(in-package #:document-cludg)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Special cldoc handler
 
(cldoc::define-descriptor-handler DEFINE-STRING-PURGER (form)
  "string purger"
  (setf (car form) 'cldoc::define-string-purger)
  (values nil :restart (list (let ((*print-case* :upcase))
                               (macroexpand-1 form)))))
 
(cldoc::define-descriptor-handler DEFINE-LAMBDA-LIST-PURGER (form)
  "lambda purger"
  (setf (car form) 'cldoc::define-lambda-list-purger)
  (values nil :restart (list (let ((*print-case* :upcase))
                               (macroexpand-1 form)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extract documentation

(cldoc:extract-documentation 'cldoc:html "doc"
  '("./gendoc.cl"
    "../lib/package.cl"
    "../lib/macros.cl"
    "../lib/core.cl"
    "../lib/css/package.cl"
    "../lib/css/css.cl"
    "../lib/css/sass.cl"
    "../lib/js/package.cl"
    "../lib/js/js.cl"
    "../test/package.cl"
    "../test/suite.cl"
    "../test/tests.cl")
  :table-of-contents-title
  "WEBGEN | Documentation")
