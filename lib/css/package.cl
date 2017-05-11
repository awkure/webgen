;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Lowercase: Yes;  -*- ;;;

(in-package :cl-user)

(defpackage #:css 
  (:documentation "The wrapper over the implemented css 
                   and it's preprocessors")
  (:use #:cl #:core #:macros)
  (:export #:css-style
           #:process-css))
