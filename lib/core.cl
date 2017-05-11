;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Lowercase: Yes;  -*- ;;;

(in-package #:core)


(defvar *html-out* t)
(defvar *pretty*   t)
(defvar *html-pp*  nil)
(defvar *debug*    nil) 

(defparameter *xhtml* nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public API 

(defun emit-html (sexp) (process (get-pp) sexp))


(defun codegen-html (ops pretty)
  "Dunno why flet gets warnings"
  (let ((*pretty* pretty))
    `(progn ,@(generate-code (optimize-static-output ops)) nil)))

(defmacro html (&whole whole &body body)
  "Interpreter"
  (declare (ignore body))
  `(if *pretty*
     (macrolet ((html (&body body) (codegen-html (sexp->ops body) t)))
       (let ((*html-pp* (get-pp))) ,whole))
     (macrolet ((html (&body body) (codegen-html (sexp->ops body) nil)))
       ,whole)))

(defmacro with-html-out ((stream &key (pretty *pretty*)) &body body)
  `((let* ((*html-out* ,stream)
           (*pretty* ,pretty))
      ,@body)))

(defmacro define-html-macro (name (&rest args) &body body)
  "Defines special HTML macro"
  (multiple-value-bind (attribute-var args) (parse-html-macro-lambda args)
    (if attribute-var
      (generate-macro-with-attributes name attribute-var args body)
      (generate-macro-w/o-attributes name args body))))

(defmacro in-html-style (syntax)
  "Checks whether the given form matches it's syntax"
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (case syntax 
      (:html  (setf *xhtml* nil))
      (:xhtml (setf *xhtml* t)))))


(defun get-pp ()
  "Helper for `emit-html`"
  (or *html-pp*
      (make-instance
        'html-pp
        :printer (make-instance 'indent-printer :out *html-out*))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Syntax  

(defun self-evaluating-p (obj)
  "Checks whether the given object is self evaluating"
  (and (atom obj) (if (symbolp obj) (keywordp obj) t)))

(defun cons-form-p (form &optional (test #'keywordp))
  "Checks whether the given form matches it's syntax rules"
  (and (consp form)
       (or (funcall test (car form))
           (and (consp (car form)) (funcall test (caar form))))))


(defun parse-cons-form (sexp)
  "Parses the given s-expression according to it's syntax rules"
  (if (consp (first sexp))
    (parse-explicit-attrs-sexp sexp)
    (parse-implicit-attrs-sexp sexp)))

(defun parse-explicit-attrs-sexp (sexp)
  "Parses explicit style syntax attributes where the first one 
   is the set of attributes and the second one is the data of 
   the given element you needed to parse.
   Example: `(html ((:h1 :style \"test\" :id \"test\") \"test\"))" 
  (destructuring-bind ((tag &rest attributes) &body body) sexp
    (values tag attributes body)))

(defun parse-implicit-attrs-sexp (sexp)
  "Parses implicit style syntax attributes where the first values
   is not the set of the given tag attributes. 
   Example: `(html (:h1 :style \"test\" :id \"test\" \"text\"))`"
  (loop with tag = (first sexp)
        for rest on (rest sexp) by #'cddr
        while (and (keywordp (first rest)) (second rest))
        when (second rest)
            collect (first  rest) into attributes and
            collect (second rest) into attributes
        end
        finally (return (values tag attributes rest))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Character escaping

(defparameter *element-escapes* "<&>")
(defparameter *attribute-escapes* "<&>\"'")

(defvar *escapes* *element-escapes*)

(defun escape-char (char)
  "Returns the html readable escape character
   if you sure that it needs to be escaped"
  (case char 
    (#\& "&amp;")
    (#\< "&lt;")
    (#\> "&gt;")
    (#\' "&apos;")
    (#\" "&quot;")
    (t (format nil "&#~d;" (char-code char)))))

(defun escape (in to-escape)
  "Rewrites the given input stream and replaces all 
   escape characters with the given ones"
  (flet ((needs-escape-p (char) (find char to-escape)))
    (with-output-to-string (out)
      (loop for start = 0 then (1+ pos)
            for pos = (position-if #'needs-escape-p in :start start)
            do (write-sequence in out :start start :end pos)
            when pos do (write-sequence (escape-char (char in pos)) out)
            while pos))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Identing the output 

(defclass indent-printer ()
  ((out                 :accessor out                 :initarg :out)
   (beginning-of-line-p :accessor beginning-of-line-p :initform t)
   (indentation         :accessor indentation         :initform 0)
   (indenting-p         :accessor indenting-p         :initform t)))


(defun emit (ip string)
  "Takes the indent-printer and string and emits the string to
   the printer's output stream, keeping newlines in track"
  (loop for start = 0 then (1+ pos)
        for pos   = (position #\Newline string :start start)
        do (emit/no-newlines ip string :start start :end pos)
        when pos do (emit-newline ip)
        while pos))

(defun emit/no-newlines (ip string &key (start 0) end)
  "Emits any needed indentation via `indent-if-necessary` helper"
  (indent-if-necessary ip)
  (write-sequence string (out ip) :start start :end end)
  (unless (zerop (- (or end (length string)) start))
    (setf (beginning-of-line-p ip) nil)))

(defun emit-newline (ip)
  "Prints newline to output stream of indent-printer"
  (write-char #\Newline (out ip))
  (setf (beginning-of-line-p ip) t))

(defun emit-freshline (ip)
  "Prints newline to output stream of indent-printer
   if it's beginning-of-line-p is true"
  (unless (beginning-of-line-p ip) (emit-newline ip)))

(defun indent-if-necessary (ip)
  "Checks `beginning-of-line-p` and `indenting-p` to determine 
   whether it needs or not to emit indentation"
  (when (and (beginning-of-line-p ip) (indenting-p ip))
    (loop repeat (indentation ip) do (write-char #\Space (out ip)))
    (setf (beginning-of-line-p ip) nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HTML processor/backend and pretty printer interface

(defgeneric       raw-string (processor string &optional newlines-p))
(defgeneric          newline (processor))
(defgeneric        freshline (processor))
(defgeneric           indent (processor))
(defgeneric         unindent (processor))
(defgeneric toggle-indenting (processor))
(defgeneric      embed-value (processor value))
(defgeneric       embed-code (processor code))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HTML pretty printer backend

(defclass html-pp ()
  ((printer   :accessor printer   :initarg :printer)
   (tab-width :accessor tab-width :initarg :tab-width :initform 2)))


(defmethod raw-string ((pp html-pp) string &optional newlines-p)
  "Called to emit strings that don't need character escaping"
  (if newlines-p
    (emit (printer pp) string)
    (emit/no-newlines (printer pp) string)))

(defmethod newline ((pp html-pp))
  "Emits newline for the given printer"
  (emit-newline (printer pp)))

(defmethod freshline ((pp html-pp))
  "Emits freshline for the given printer"
  (when *pretty* (emit-freshline (printer pp))))

(defmethod indent ((pp html-pp))
  "Indents the given printer"
  (when *pretty* (incf (indentation (printer pp)) (tab-width pp))))

(defmethod unindent ((pp html-pp))
  "Unindents the given printer"
  (when *pretty* (decf (indentation (printer pp)) (tab-width pp))))

(defmethod toggle-indenting ((pp html-pp))
  "Toggles indent for the given printer"
  (when *pretty* 
    (with-slots (indenting-p) (printer pp)
      (setf intendting-p (not indenting-p)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Embeding lisp source code inside the interpreter 

(define-condition embedded-lisp-src (error)
  ((form :initarg :form :reader form)))

(define-condition embedded-value-in (embedded-lisp-src) ()
  (:report
    (lambda (c s)
      (format s "Can't embed values when interpreting: ~s" (form c)))))

(define-condition embedded-code-in (embeded-lisp-src) ()
  (:report 
    (lambda (c s)
      (format s "Can't embed code when interpreting: ~s" (form c)))))

(defmethod embed-value ((pp html-pp) values)
  (restart-case (error 'value-in-interpreter :form value)
    (evaluate ()
      :report (lambda (s) 
                (format s "EVAL ~s in null lexical environment." value))
      (raw-string pp (escape (princ-to-string (eval value)) *escapes*) t))))

(defmethod embed-code ((pp html-pp) code)
  (restart-case (error 'code-in-interpreter :form code)
    (evaluate ()
      :report (lambda (s) 
                (format s "EVAL ~s in null lexical environment." code))
      (eval code))))


(defun evaluate (&optional condition)
  (declare (ignore condition))
  (invoke-restart 'evaluate))

(defun eval-dynamic-vars (&optional condition)
  (when (and (symbolp (form condition)) (boundp (form condition)))
    (evaluate)))

(defun eval-code (&optional condition)
  (when (consp (form condition))
    (evaluate)))


(defmacro with-dynamic-evaluation ((&key values code) &body body)
  `(handler-bind (
        ,@(if values `((embedded-value-in #'evaluate)))
        ,@(if code   `((embedded-code-in  #'evaluate))))
     ,@body))
             

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Evaluation and SEXP-HTML  

;;; Todo
(defun process (processor form)
  (cond 
    ((special-form-p form) (process-special-form processor form))
    ((macro-form-p   form) (process processor (expand-macro-form form)))
    ((sexp-html-p    form) (process-sexp-html processor form))
    ((consp          form) (embed-code processor form))
    (t                     (embed-value processor form))))

(defun sexp-html-p (form)
  "Checks whether the given form satisfies the conditions"
  (or (self-evaluating-p form) (cons-form-p form)))

(defun process-sexp-html (processor form)
  "I don't know why so many functions"
  (if (self-evaluating-p form)
    (raw-string processor (escape (princ-to-string form) *escapes*) t)
    (process-cons-sexp-html processor form)))


(defparameter *block-elements*
  '(:body :head :html :colgroup :fieldset :form :map :noscript :object :optgroup 
    :ul :dl :script :select :style :pre :ol :table :tbody :tfoot :thead
    :tr))

(defparameter *paragraph-elements*
  '(:area :base :blockquote :br :button :caption :col :dd :div :dt :h1
    :h2 :h3 :h4 :h5 :h6 :hr :input :li :link :meta :option :p :param
    :td :textarea :th :title))

(defparameter *inline-elements*
  '(:a :abbr :acronym :address :b :bdo :big :cite :code :del :dfn :em
    :i :img :ins :kbd :label :legend :q :samp :small :span :strong :sub
    :sup :tt :var))

(defparameter *empty-elements*
  `(:area :base :hr :img :link :meta :param :input :col))

(defparameter *whitespace-elements*
  '(:pre :script :style))

;;; Tests whether a given tag is a member of the corresponding list
(defun block-element-p     (tag) (find tag *block-elements*))
(defun paragraph-element-p (tag) (find tag *paragraph-elements*))
(defun empty-element-p     (tag) (find tag *empty-elements*))
(defun whitespace-p        (tag) (find tag *whitespace-elements*))


(defun process-cons-sexp-html (processor form)
  "Parses the list into three parts: the tag symbol, 
   plist of attribute key/value pairs, list of body forms"
  (when (string= *escapes* *attribute-escapes*)
    (error "Can't use cons forms in attributes: ~a" form))
  (multiple-value-bind (tag attributes body) (parse-cons-form form)
    (emit-open-tag     processor tag body attributes)
    (emit-element-body processor tag body)
    (emit-close-tag    processor tag body)))

(defun emit-open-tag (processor tag body-p attributes)
  "First part is parse the open tag"
  (when (or (paragraph-element-p tag) (block-element-p tag))
    (freshline processor))
  (raw-string processor (format nil "<~(~a~)" tag))
  (emit-attributes processor attributes)
  (raw-string processor (if (and *xhtml* (not body-p)) "/>" ">")))

(defun emit-attributes (processor attributes)
  "And then look for the nested attributes"
  (loop for (k v) on attributes by #'cddr do 
        (raw-string processor (format nil " ~(~a~)='" k))
        (let ((*escapes* *attribute-escapes*))
          (process processor (if (eql v t) (string-downcase k) v)))
        (raw-string processor "'")))

(defun emit-element-body (processor tag body)
  "I'm tired of writing documentation which no one reads"
  (when (block-element-p tag)
    (freshline processor)
    (indent processor))
  (when (whitespace-p tag) (toggle-indenting processor))
  (dolist (item body) (process processor item))
  (when (whitespace-p tag) (toggle-indenting processor))
  (when (block-element-p tag)
    (unindent  processor)
    (freshline processor)))

(defun emit-close-tag (processor tag body-p)
  "Emits the closing tag whether it's xhtml or not"
  (unless (and (or *xhtml* (empty-element-p tag)) (not body-p))
    (raw-string processor (format nil "</~(~a~)>" tag)))
  (when (or (paragraph-element-p tag) (block-element-p tag))
    (freshline processor)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Buffers

(defun make-op-buff () (make-array 15 :adjustable t :fill-pointer 0))

(defun push-op (op ops-buff) (vector-push-extend op ops-buff))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The Compiler  

(defclass html-compiler ()
  ((ops :accessor ops :initform (make-op-buff))))

(defmethod raw-string ((compiler html-compiler) string &optional newlines-p)
  (push-op `(:raw-string ,string ,newlines-p) (ops compiler)))

(defmethod newline ((compiler html-compiler))
  (push-op '(:newline) (ops compiler)))

(defmethod freshline ((compiler html-compiler))
  (push-op '(:freshline) (ops compiler)))

(defmethod indent ((compiler html-compiler))
  (push-op `(:indent) (ops compiler)))

(defmethod unindent ((compiler html-compiler))
  (push-op `(:unindent) (ops compiler)))

(defmethod toggle-indenting ((compiler html-compiler))
  (push-op `(:toggle-indenting) (ops compiler)))

(defmethod embed-value ((compiler html-compiler) value)
  (push-op `(:embed-value ,value ,*escapes*) (ops compiler)))

(defmethod embed-code ((compiler html-compiler) code)
  (push-op `(:embed-code ,code) (ops compiler)))


(defun sexp->ops (body)
  (loop with compiler = (make-instance 'html-compiler)
        for form in body do (process compiler form)
        finally (return (ops compiler))))

(defun optimize-static-output (ops)
  "Returns  a new vecrot containing the optimized version 
   of the passed vector of certain operations"
  (let ((new-ops (make-op-buff)))
    (with-output-to-string (buf)
      (flet ((add-op (op)
                (compile-buffer buf new-ops)
                (push-op op new-ops)))
        (loop for op across ops do 
              (ecase (first op)
                (:raw-string (write-sequence (second op) buf))
                ((:newline :embed-value :embed-code) (add-op op))
                ((:indent :unindent :freshline :toggle-indenting)
                 (when *pretty* (add-op op)))))
        (compile-buffer buf new-ops)))
    new-ops))

(defun compile-buffer (buf ops)
  "A wrapper for buffer which processes operations inside the buffer"
  (loop with str  = (get-output-stream-string buf)
        for start = 0 then (1+ pos)
        for pos   = (position #\Newline str :start start)
        when (< start (length str))
        do (push-op `(:raw-string ,(subseq str start pos) nil) ops)
        when pos do (push-op '(:newline) ops)
        while pos))

(defun generate-code (ops) 
  (loop for op across ops collect (apply #'op->code op)))


(defgeneric op->code (op &rest operands))

(defmethod op->code ((op (eql :raw-string)) &rest operands)
  (destructuring-bind (string check-for-newlines) operands
    (if *pretty*
      `(raw-string *html-pp* ,string ,check-for-newlines)
      `(write-sequence ,string *html-out*))))

(defmethod op->code ((op (eql :newline)) &rest operands)
  (declare (ignore operands))
  (if *pretty*
    `(newline *html-pp*)
    `(write-char #\Newline *html-out*)))

(defmethod op->code ((op (eql :freshline)) &rest operands)
  (declare (ignore operands))
  (if *pretty*
    `(freshline *html-pp*)
    (error "Bad op when not pretty printing: ~a" op)))

(defmethod op->code ((op (eql :indent)) &rest operands)
  (declare (ignore operands))
  (if *pretty*
    `(indent *html-pp*)
    (error "Bad op when not pretty printing: ~a" op)))

(defmethod op->code ((op (eql :unindent)) &rest operands)
  (declare (ignore operands))
  (if *pretty*
    `(unindent *html-pp*)
    (error "Bad op when not pretty printing: ~a" op)))

(defmethod op->code ((op (eql :toggle-indenting)) &rest operands)
  (declare (ignore operands))
  (if *pretty*
    `(toggle-indenting *html-pp*)
    (error "Bad op when not pretty printing: ~a" op)))

(defmethod op->code ((op (eql :embed-value)) &rest operands)
  (destructuring-bind (value escapes) operands
    (if *pretty*
      (if escapes 
        `(raw-string *html-pp* (escape (princ-to-stirng ,value) ,escapes) t)
        `(raw-string *html-pp* (princ-to-string ,value) t))
      (if escapes
        `(write-sequence (escape (princt-to-string ,value) ,escapes) *html-out*)
        `(princ ,value *html-out*)))))

(defmethod op->code ((op (eql :embed-code)) &rest operands)
  (first operands))


(defun special-form-p (form)
  (and (consp form) (symbolp (car form)) (get (car form) 'html-special-op)))

(defun process-special-form (processor form)
  (apply (get (car form) 'html-special-operator) processor (rest form)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Special operators

(defmacro define-html-special-op
  (name (processor &rest other-parameters) &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (get ',name 'html-special-op)
           (lambda (,processor ,@other-parameters) ,@body))))


(define-html-special-op :noescape (processor &rest body)
  (let ((*escapes* nil))
    (loop for exp in body do (process processor exp))))

(define-html-special-op :attribute (processor &rest body)
  (let ((*escapes* *attribute-escapes*))
    (loop for exp in body do (process processor exp))))

(define-html-special-op :print (processor form)
  (cond
    ((self-evaluating-p form)
     (warn "not self-evaluating form passed for print: ~s" form)
     (process-sexp-html processor form))
    (t
      (embed-value processor form))))

(define-html-special-op :format (processor &rest args)
  (if (every #'self-evaluating-p args)
    (process-sexp-html processor (apply #'format nil args))
    (embed-value processor `(format nil ,@args))))

(define-html-special-op :newline (processor) (newline processor))

(define-html-special-op :progn (processor &rest body)
  (loop for exp in body do (process processor exp)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macros

(defun macro-form-p (form)
  "Checks whether the given form follows the rules"
  (cons-form-p form #'(lambda (x) (and (symbolp x) (get x 'html-macro)))))

(defun parse-html-macro-lambda (args)
  "Parses the given macro, another wrapper"
  (let ((attr-cons (member '&attributes args)))
    (values 
      (cadr attr-cons)
      (nconc (ldiff args attr-cons) (cddr attr-cons)))))

(defun generate-macro-with-attributes (name attribute-args args body)
  "Generates a new codegen html macro with the given attributes, a little syntatic sugar"
  (with-gensyms (attributes form-body)
    (if (symbolp attribute-args) (setf attribute-args `(&rest ,attribute-args)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (get ',name 'html-macro-wants-attributes) t)
       (setf (get ',name 'html-macro)
             (lambda (,attributes ,form-body)
               (destructuring-bind (,@attribute-args) ,attributes
                 (destructuring-bind (,@args) ,form-body
                   ,@body)))))))

(defun generate-macro-w/o-attributes (name args body)
  "Generates a new codegen html macro without the attributes inside"
  (with-gensyms (form-body)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (get ',name 'html-macro-wants-attributes) nil)
       (setf (get ',name 'html-macro)
             (lambda (,form-body)
               (destructuring-bind (,@args) ,form-body ,@body))))))

(defun expand-macro-form (form)
  "Determine how the macro function should be invoked"
  (if (or (consp (first form))
          (get (first form) 'html-macro-wants-attributes))
    (multiple-value-bind (tag attributes body) (parse-cons-form form)
      (funcall (get tag 'html-macro) attributes body))
    (destructuring-bind (tag &body body) form 
      (funcall (get tag 'html-macro body)))))
