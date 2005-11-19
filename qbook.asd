;; -*- lisp -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :it.bese.qbook.system)
    (defpackage :it.bese.qbook.system
      (:documentation "ASDF System package for qbook.")
      (:use :common-lisp :asdf))))

(in-package :it.bese.qbook.system)

(defsystem :qbook
  :components ((:static-file "qbook.asd")
               (:module :src
                :components ((:file "code-analysis" :depends-on ("packages" "qbook"))
                             (:file "html" :depends-on ("packages" "qbook" "stylesheets"))
                             (:file "stylesheets" :depends-on ("packages"))
			     (:file "latex" :depends-on ("packages" "qbook"))
			     (:file "packages")
			     (:file "publish-op" :depends-on ("packages" "qbook"))
			     (:file "qbook" :depends-on ("packages")))))
  :depends-on (:arnesi :iterate :cl-ppcre :yaclml))

;;;;@include "src/qbook.lisp"

;;;;@include "src/publish-op.lisp"

;;;;@include "src/code-analysis.lisp"

;;;;@include "src/html.lisp"

;;;;@include "src/stylesheets.lisp"

;;;;@include "src/latex.lisp"

;;;;@include "src/packages.lisp"
