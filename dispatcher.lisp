(require :s-xml-rpc)
(require :marshal)
(defpackage "FUNC-DISPATCH"
  (:use :common-lisp 
	:s-xml-rpc
	:marshal
	#+allegro :mop
	#+clisp :clos
	#+lispworks :clos
	#+openmcl :ccl
	#+mcl :ccl
	#+cmu :clos-mop
	#+sbcl :sb-mop)
  (:export :def-pfun :send-pfun :receive-pfun :remote-call))

(in-package :func-dispatch)

(defclass function-with-definition ()
  ((func-name :accessor function-name
	      :initform nil
	      :initarg :name)
   (func-definition :accessor function-definition
		    :initform nil
		    :initarg :definition))
  (:metaclass funcallable-standard-class))

(defgeneric update-funcallable-function (func-with-def))

(defmethod class-persistant-slots ((self function-with-definition))
    '(func-name func-definition))

(defmethod initialize-instance :after ((obj function-with-definition) &key (definition nil))
  (when (not (eql nil definition))
    (set-funcallable-instance-function obj (compile nil definition))))

(defmethod (setf function-definition) :after ((obj function-with-definition) func-def)
  (unless (eql nil func-def) (set-funcallable-instance-function obj (compile nil func-def))))

(defmethod update-funcallable-function ((obj function-with-definition))
  (let ((lambda-def (function-definition obj)))
    (unless (eql nil lambda-def)
      (set-funcallable-instance-function obj (compile nil lambda-def)))))

(defun obj->string (obj)
  (let ((str (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t)))
    (with-output-to-string (stream str)
      (prin1 (marshal obj) stream))
    str))

(defun string->obj (string)
  (with-input-from-string (stream string)
    (unmarshal (read stream))))

(defmacro def-pfun (name args &body body)
  `(progn 
     (declaim (notinline ,name))
     (defun ,name ,args ,@body)
     (setf (fdefinition ',name) 
	   (make-instance 'function-with-definition 
			  :name ',name
			  :definition '(lambda ,args ,@body)))
     ',name))

(defmacro send-pfun (name &key (host nil hostp) (port nil portp))
  (append `(xml-rpc-call 
	    (encode-xml-rpc-call "RECEIVE_PFUN" (obj->string (fdefinition ',name))))
	  (when hostp (list :host host))
	  (when portp (list :port port))))

(defun receive_pfun (pfun-str)
  (let* ((unmarshaled (string->obj pfun-str)) (symbol (intern (symbol-name (function-name unmarshaled)) :func-dispatch)))
    (update-funcallable-function unmarshaled)
    (setf (symbol-function symbol) unmarshaled)
    pfun-str))
