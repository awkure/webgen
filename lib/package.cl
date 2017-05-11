;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Lowercase: Yes;  -*- ;;;

(in-package :cl-user)

(defpackage #:macros
  (:documentation "Helpers which I use almost everywhere, 
                   not anything will be used in this project")
  (:use #:cl)
  (:export #:ppme
           #:with-gensyms
           #:aif
           #:deferror
           #:with-timing
           #:show-timing-data
           #:clear-timing-data))

(defpackage #:core
  (:documentation "The core of this project")
  (:use #:cl)
  (:import-from #:macros #:with-gensyms)
  (:export #:with-html-out
           #:with-html-to-file
           #:in-html-style
           #:define-html-macro
           #:html
           #:emit-html
           #:define-html
           #:&attributes))
