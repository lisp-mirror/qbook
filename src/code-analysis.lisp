;; -*- lisp -*-

(in-package :it.bese.qbook)

;;;; * Extra Code Analysis

(defvar *code-info-collectors* (make-hash-table :test 'eql))

(defun analyse-code-part (code-part)
  (awhen (gethash (first (form code-part)) *code-info-collectors*)
    (setf (descriptor code-part) (funcall it (cdr (form code-part)))))
  code-part)

(defmacro defcode-info-collector (operator args &body body)
  (with-unique-names (form)
    (let ((function-name (intern (strcat operator :-descriptor) (find-package :it.bese.qbook))))
      `(progn
         (defun ,function-name (,form)
           (destructuring-bind ,args ,form
             ,@body))
         (setf (gethash ',operator *code-info-collectors*)
               ',function-name)))))

(defclass descriptor ()
  ((name :accessor name :initarg :name)
   (docstring :accessor docstring :initarg :docstring)
   (label-prefix :accessor label-prefix :initarg :label-prefix)
   (pretty-label-prefix :accessor pretty-label-prefix :initarg :pretty-label-prefix)))

(defclass defun-descriptor (descriptor)
  ((lambda-list :accessor lambda-list :initarg :lambda-list)
   (body :accessor body :initarg :body))
  (:default-initargs
   :label-prefix "function"
   :pretty-label-prefix "Function"))

(defcode-info-collector cl:defun (name lambda-list &body body)
  (multiple-value-bind (lambda-list env)
      (arnesi::walk-lambda-list lambda-list nil nil)
    (multiple-value-bind (body docstring declarations)
        (handler-bind ((arnesi::return-from-unknown-block
                                           (lambda (c)
                                             (declare (ignore c))
                                             (invoke-restart 'arnesi::add-block))))
          (arnesi::walk-implict-progn nil body env :docstring t :declare t))
      (declare (ignore declarations))
      (make-instance 'defun-descriptor
                     :name name
                     :lambda-list lambda-list
                     :body body
                     :docstring docstring))))

(defclass defmacro-descriptor (defun-descriptor)
  ()
  (:default-initargs
   :label-prefix "macro"
   :pretty-label-prefix "Macro"))

(defcode-info-collector cl:defmacro (name lambda-list &body body)
  (multiple-value-bind (lambda-list env)
      (arnesi::walk-lambda-list lambda-list nil nil)
    (multiple-value-bind (body docstring declarations)
        (handler-bind ((arnesi::return-from-unknown-block
                                           (lambda (c)
                                             (declare (ignore c))
                                             (invoke-restart 'arnesi::add-block))))
          (arnesi::walk-implict-progn nil body env :docstring t :declare t))
      (declare (ignore declarations))
      (make-instance 'defmacro-descriptor
                     :name name
                     :lambda-list lambda-list
                     :body body
                     :docstring docstring))))

(defclass defclass-descriptor (descriptor)
  ((slots :accessor slots :initarg :slots :initform '())
   (supers :accessor supers :initarg :supers :initform '()))
  (:default-initargs
   :label-prefix "class"
    :pretty-label-prefix "Class"))

(defcode-info-collector cl:defclass (name supers slots &rest options)
  (make-instance 'defclass-descriptor
                 :name name
                 :supers supers
                 :slots (mapcar #'make-slot-descriptor slots)
                 :docstring (cdr (assoc :documentation options))))

(defclass class-slot-descriptor (descriptor)
  ())

(defun make-slot-descriptor (slot-spec)
  (destructuring-bind (name &rest options)
      (ensure-list slot-spec)
    (make-instance 'class-slot-descriptor
                   :name name
                   :docstring (getf options :documentation))))

(defclass global-variable-descriptor (descriptor)
  ())

(defclass defvar-descriptor (global-variable-descriptor)
  ()
  (:default-initargs
   :label-prefix "variable"
   :pretty-label-prefix "Variable"))

(defcode-info-collector cl:defvar (name &optional value documentation)
  (declare (ignore value))
  (make-instance 'defvar-descriptor
                 :name name
                 :docstring documentation))

(defclass defparameter-descriptor (global-variable-descriptor)
  ()
  (:default-initargs
   :label-prefix "variable"
   :pretty-label-prefix "Variable"))

(defcode-info-collector cl:defparameter (name &optional value documentation)
  (declare (ignore value))
  (make-instance 'defparameter-descriptor
                 :name name
                 :docstring documentation))

(defclass defmethod-descriptor (defun-descriptor)
  ((qualifier :accessor qualifier :initform nil :initarg :qualifier))
  (:default-initargs
   :label-prefix "method"
   :pretty-label-prefix "Method"))

(defcode-info-collector cl:defmethod (name &rest args)
  (let ((qualifier nil)
        arguments
        body)
    (when (symbolp (first args))
      (setf qualifier (pop args)))
    (setf arguments (pop args)
          body args)
    (multiple-value-bind (lambda-list env)
        (arnesi::walk-lambda-list arguments nil nil :allow-specializers t)
      (multiple-value-bind (body docstring declarations)
          (handler-bind ((arnesi::return-from-unknown-block
                          (lambda (c)
                            (declare (ignore c))
                            (invoke-restart 'arnesi::add-block))))
            (arnesi::walk-implict-progn nil body env :docstring t :declare t))
        (declare (ignore declarations))
        (make-instance 'defmethod-descriptor
                       :name name
                       :qualifier qualifier
                       :lambda-list lambda-list
                       :body body
                       :docstring docstring)))))
