;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Lowercase: Yes;  -*- ;;;

(in-package :cl-user)

(defpackage #:test-suite 
  (:documentation "Test suite for this library")
  (:import-from #:macros #:with-gensyms)
  (:use #:cl #:core)
  (:export #:deftest #:check))
