;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Lowercase: Yes;  -*- ;;;

(in-package :cl-user)

(defpackage #:js 
  (:documentation "The wrapper over the implemented 
                   JavaScript implementations")
  (:use #:cl #:core #:macros #:css))
