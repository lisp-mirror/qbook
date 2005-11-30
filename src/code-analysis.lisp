;; -*- lisp -*-

(in-package :it.bese.qbook)

;;;; * Extra Code Analysis

(defvar *code-info-collectors* (make-hash-table))

(defvar *known-elements* (make-hash-table))

(defun register-descriptor (type descriptor)
  (push (cons (name descriptor) descriptor)
        (gethash type *known-elements*)))

(defun find-descriptor (type name)
  (when-bind elements-of-type (gethash type *known-elements*)
    (cdr (assoc name elements-of-type
                :test (lambda (a b)
                        (if (symbolp a)
                            (eq a b)
                            (and (eq (first a) (first b))
                                 (eq (second a) (second b)))))))))

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

(defun subseq-first-sentence (string limit)
  (with-output-to-string (first-sentence)
    (flet ((ret ()
             (return-from subseq-first-sentence
               (get-output-stream-string first-sentence))))
      (loop
         for char across string
         for count below limit
         if (member char (list #\. #\? #\!))
         do (write-char char first-sentence)
         and do (ret)
         else
         do (write-char char first-sentence)
         finally (ret)))))

(defgeneric docstring-first-sentence (descriptor &optional limit)
  (:documentation "Returns the first sentence of DESCRIPTOR's
docstring. Returns at most LIMIT characters (if the first
sentence is longer than LIMIT characters it will be simply
truncated. If DESCRIPTOR's docstring is NIL this function
returns nil.")
  (:method ((descriptor descriptor) &optional (limit 180))
    (subseq-first-sentence (docstring descriptor) limit)))

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
                 :docstring (second (assoc :documentation options))))

(defclass class-slot-descriptor (descriptor)
  ())

(defun make-slot-descriptor (slot-spec)
  (destructuring-bind (name &rest options)
      (ensure-list slot-spec)
    (make-instance 'class-slot-descriptor
                   :name name
                   :docstring (getf options :documentation))))

(defclass global-variable-descriptor (descriptor)
  ()
  (:default-initargs
   :label-prefix "variable"
   :pretty-label-prefix "Variable"))

(defcode-info-collector cl:defvar (name &optional value documentation)
  (declare (ignore value))
  (make-instance 'global-variable-descriptor
                 :name name
                 :docstring documentation))

(defcode-info-collector cl:defparameter (name &optional value documentation)
  (declare (ignore value))
  (make-instance 'global-variable-descriptor
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

(defclass defconstant-descriptor (global-variable-descriptor)
  ()
  (:default-initargs
   :label-prefix "constant"
   :pretty-label-prefix "Constant"))

(defcode-info-collector cl:defconstant (name value &optional docstring)
  (declare (ignore value))
  (make-instance 'defconstant-descriptor
                 :name name :docstring docstring))
