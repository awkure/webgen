;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Lowercase: Yes;  -*- ;;;

(in-package #:macros)

(defmacro with-gensyms ((&rest names) &body body)
  "Takes the given list and binds the unique 
   names to the elements of the given list"
  `(let ,(loop for n in names collect `(,n (make-symbol ,(string n))))
     ,@body))

(defun spliceable (value)
  "Returns the wrapper over the values if it's not nil."
  (if value (list value)))

(defmacro aif (test-form then-form &optional else-form)
  "Tests the given test-form and makes `it` variable to it"
   `(let ((it ,test-form))
      (if it ,then-form ,else-form)))

(defmacro once-only ((&rest names) &body body)
  "This macro is used to generate code that evaluates certain 
   macro arguments once only and in particular order"
  (let ((gensyms (loop for n in names collect (gensym (string n)))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
           ,@body)))))

(defmacro hash ((&rest arguments) &body k-v-plist)
  "Make the hash table. Use no delimiter between keys/values."
  (let ((hash-t (make-symbol "HASH-TABLE")))
    (labels ((gethash-expand (plist table)
               (when plist
                 (cons `(setf (gethash ,(car plist) ,table) ,(cadr plist))
                   (gethash-expand (cddr plist) table)))))
      `(let ((,hash-t (make-hash-table ,@arguments)))
        ,@(gethash-expand k-v-plist hash-t)
        ,hash-t))))

(defmacro deferror (error-name report-format &rest slot-names)
  "Defines error conditions. Condition slot-names are passed as format arguments 
   to the report-format string when printing the condition, in order."
  (with-gensyms (condition stream)
    (flet ((catsyms (&rest symbols) (intern (format nil "~{~S~^-~}" symbols)))
           (keywordify (sym) (intern (string sym) :keyword)))
      `(define-condition ,error-name (error)
         ,(loop for slot-name in slot-names
                collect (list slot-name
                              :initarg (keywordify slot-name)
                              :initform nil ; default value if there's no initarg
                              :accessor (catsyms error-name slot-name)))
         (:report (lambda (,condition ,stream)
                    (format ,stream
                            ,report-format
                            ,@(loop for slot-name in slot-names
                                    collect (list (catsyms 
                                                   error-name
                                                   slot-name)
                                                  condition)))))))))

(defmacro ppme (form &environment env)
  "Prettify the output of the given form inside the given environment."
  (progn
    (write (macroexpand-1 form env)
           :length nil
           :level nil
           :gensym nil
           :pretty t
           :right-margin 83
           :circle nil
           :case :downcase)
    nil))


(defparameter *timing-data* ())

(defun clear-timing-data () (setf *timing-data* ()))

(defun show-timing-data ()
  "Shows stored timing data inside *timing-data* variable in a pretty way"
  (loop for (label time count per %-total) in (compile-timing-data) do
        (format t "[~3d% ~a: ~d ticks over ~d calls for ~d per]~%"
                %-total label time count per)))

(defmacro with-timing (label &body body)
  "The public wrapper for custom timer"
  (with-gensyms (start)
    `(let ((,start (get-internal-run-time)))
       (unwind-protect (progn ,@body)
         (push (list ',label ,start (get-internal-run-time)) *timing-data*)))))

(defun compile-timing-data ()
  "Defines the custom timer's behavior"
  (loop with timing-table = (make-hash-table)
        with  count-table = (make-hash-table)
        for (label start end) in *timing-data*
        for time = (- end start)
        summing time into total 
        do (incf (gethash label timing-table 0) time)
           (incf (gethash label  count-table 0))
        finally 
        (return
          (sort 
            (loop for label being the hash-keys in timing-table collect
                  (let ((time  (gethash label timing-table))
                        (count (gethash label count-table)))
                    (list label time count (round (/ time count)) 
                                           (round (* 100 (/ time total))))))
            #'> :key #'fifth))))
