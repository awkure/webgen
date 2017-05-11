;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Lowercase: Yes;  -*- ;;;

(in-package #:test-suite)

(deftest html-test ()
  (combine
    (paragraph-html-test)
    (block-html-test)
    (inline-html-test)
    (embedded-html-test)))

(deftest xhtml-test ()
  (combine 
    (paragraph-xhtml-test)
    (block-xhtml-test)
    (inline-xhtml-test)
    (embedded-xhtml-test)))

(deftest paragraph-html-test  () ())
(deftest block-html-test      () ())
(deftest inline-html-test     () ())
(deftest embedded-html-test   () ())

(deftest paragraph-xhtml-test () ())
(deftest block-xhtml-test     () ())
(deftest inline-xhtml-test    () ())    
(deftest embedded-xhtml-test  () ())    

;; I got bored, later
