;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Lowercase: Yes;  -*- ;;;

(in-package #:test-suite)

(defvar *test-name* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public interfaces 

;(defmacro check (&body forms)
;  `(combine 
;     ,@(loop for f in forms collect `(report ,f ',f))))

(defmacro check (&body forms)
  (pre-report forms)
  `(combine 
     ,@(loop for f in forms collect `(report ,f ',f))))

(defmacro deftest (name params &body body)
  `(defun ,name ,params 
     (let ((*test-name* (append *test-name* ,name)))
       ,@body)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Private interfaces 

(defmacro combine (&body forms)
  (with-gensyms (res)
    `(let ((,res t))
       ,@(loop for f in forms collect `(unless ,f (setf ,res nil)))
       ,res)))

(defun pre-report (forms)
  (loop for form in forms 
        do (progn
             (format t "~:[f~;.~]" form)
             (force-output)
             (sleep 1e-2))))

(defun report (res form)
  (format t "[~:[!~;+~]] ~a: ~a~%" res *test-name* form) res)
