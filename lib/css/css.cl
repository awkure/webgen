;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Lowercase: Yes;  -*- ;;;

(in-package #:css)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic css implementation, public interfaces

(define-html-special-op css-style (processor &rest body)
  (dolist (sexp body)
    (if (eql (first sexp) :import)
      (emit-css-import processor sexp)
      (process-css processor sexp))))

(defun process-css (processor sexp)
  "The main wrapper for css parser"
  (destructuring-bind (selector &rest attributes) sexp
    (freshline processor)
    (emit-css-selector processor selector)
    (freshline processor)
    (raw-string processor "{")
    (indent processor)
    (freshline processor)
    (loop for (k v) on attributes by #'cddr do
          (process-css-key-or-val processor k)
          (raw-string processor ": ")
          (process-css-key-or-val processor v)
          (raw-string processor ";")
          (freshline processor))
    (unindent processor)
    (freshline processor)
    (raw-string processor "}")
    (freshline processor)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic css implementation, public api

(defun emit-css-import (processor sexp)
  "Generates the proper css import from lisp expression"
  (let ((url (second sexp)))
    (freshline processor)
    (raw-string processor "@import ")
    (cond 
      ((consp url)
       (raw-string processor "url(")
       (raw-string processor (second url))
       (raw-string processor ")"))
      (t (raw-string processor (format nil "\"~a\"" url))))
    (raw-string processor ";")))


(defun process-css-key-or-val (processor form)
  (if (keywordp form)
    (embed-value processor (string-downcase form))
    (process processor form)))

(defun emit-css-selector (processor selector)
  "Generate proper css selector from lisp expression"
  (cond 
    ((atom selector)
     (raw-string processor (string selector)))
    ((and (consp selector) (member (first selector) '(or and adjacent)))
     (loop with separator = (case (first selector) (or ", ") (and " ") (adjacemt " + "))
           for (x . rest) on (rest selector)
           do (emit-css-selector processor x)
           when rest do (raw-string processor separator)))
    (t 
      (multiple-value-bind (tag class pseudo-class id) (parse-selector selector)
        (when tag
          (embed-value processor (string tag)))
        (when class
          (embed-value processor (format nil ".~a" pseudo-class)))
        (when id 
          (embed-value processor (format nil "#~a" id)))))))

(defun parse-selector (selector)
  (if (member (first selector) '(:class :pseudo-class :id))
    (destructuring-bind (&key class pseudo-class id) selector
      (values nil class pseudo-class id))
    (destructuring-bind (tag &key class pseudo-class id) selector
      (values tag class psudo-class id))))
