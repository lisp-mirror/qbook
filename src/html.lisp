;;;; -*- lisp -*-

(in-package :it.bese.qbook)

(defclass html-generator (generator)
  ((output-directory :initarg :output-directory :accessor output-directory)))

(defmethod generate (book (generator html-generator))
  (ensure-directories-exist (output-directory generator))
  (generate-table-of-contents (contents book) generator)
  (dolist (section (contents book))
    (generate-section section generator)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (yaclml:deftag-macro <qbook-page (&attribute title &body body)
    `(<:html
       (<:head
	 (<:title (<:as-html ,title))
	 (<:stylesheet "style.css")
	 (<:link :rel "alternate stylesheet" :href "print.css" :title "Print"))
       (<:body
	 (<:div :class "qbook" ,@body)))))

(defun generate-table-of-contents (sections generator)
  (with-output-to-file (*yaclml-stream*
			(merge-pathnames "index.html" (output-directory generator))                        
                        :if-exists :supersede
                        :if-does-not-exist :create)
    (<qbook-page :title (title generator)
      (<:h1 :class "title" (<:as-html (title generator)))
      (<:div :class "contents"
        (<:ul
          (dolist (section sections)
            (dolist (part section)
              (when (heading-part-p part)
                (<:div :class (strcat "contents-heading-" (depth part))
                  (<:a :href (make-anchor-link part)
                    (<:as-html (text part))))))))))))

(defun generate-section (section generator)
  (with-output-to-file
      (*yaclml-stream* (merge-pathnames (make-pathname :name (make-anchor-name (text (first section)))
                                                       :type "html")
                                        (output-directory generator))
                       :if-does-not-exist :create
                       :if-exists :supersede)
  (<qbook-page :title (title generator)
    (<:h1 :class "title" (<:as-html (title generator)))
    (<:div :class "contents")
    (publish section))))

(defmethod make-anchor-link ((h heading-part) )
  (if (= 1 (depth h))
      (strcat (make-anchor-name (text h)) ".html")
      (labels ((find-level-1 (h)
		 (if (= 1 (depth h))
		     h
		     (find-level-1 (up-part h)))))
	(strcat (make-anchor-link (find-level-1 h)) "#" (make-anchor-name (text h))))))

(defun make-anchor-name (text)
  (regex-replace-all "[^A-Za-z]" text "_"))

(defun publish (parts)
  (iterate
    (with state = nil)
    (for p in parts)
    (etypecase p
      (comment-part (setf state (write-comment p state)))
      (whitespace-part (setf state nil) (<:as-html (text p)))
      (code-part (setf state (write-code p state))))))

(defun num-lines (text)
  (iterate
    (with num-lines = 0)
    (for char in-string text)
    (when (member char '(#\Newline #\Return #\Linefeed))
      (incf num-lines))
    (finally (return num-lines))))

(defun write-code (part state)
  (ecase state
    ((nil) nil)
    (:in-comment
     (setf state nil)
     (write-string "</p>" *yaclml-stream*)
     (terpri *yaclml-stream*)))
  (let ((text (text part)))
    (setf text (yaclml::escape-as-html text))
    (setf text (regex-replace-all "(\\(|\\))"
				  text
				  "<span class=\"paren\">\\1</span>"))
    (setf text (regex-replace "^.*"
			      text
			      (strcat "<span class=\"first-line\">\\&</span><span class\"body\">")))    
    (<:pre :class "code" (<:as-is text) (<:as-is "</span>")))
  nil)

(defun write-comment (part state)
  (etypecase part
    (heading-part
     (ecase state
       ((nil))
       (:in-comment
	;; heading during a comment, break the current comment
	;; and start a new one.
	(write-string "</p>" *yaclml-stream*)
	(terpri *yaclml-stream*)))
     (flet ((heading ()
	      (<:a :name (make-anchor-name (text part)) (<:as-html (text part)))
	      (<:as-is "&nbsp;"))
	    (nav-links ()
	      (<:div :class "nav-links"
  	        (if (prev-part part)
		    (<:a :class "nav-link" :href (make-anchor-link (prev-part part)) "prev")
		    (<:span :class "dead-nav-link" "prev"))
		" | "
		(if (up-part part)
		    (<:a :class "nav-link" :href (make-anchor-link (up-part part)) "up")
		    (<:span :class "dead-nav-link" "up"))
		" | "
		(if (next-part part)
		    (<:a :href (make-anchor-link (next-part part)) "next")
		    (<:span :class "nav-link" "next"))
		" | "
		(<:a :href "index.html" "toc"))))
       (case (depth part)
	 (1 (<:h2 (heading)))
	 (2 (<:h3 (heading)))
	 (3 (<:h4 (heading)))
	 (4 (<:h5 (heading)))
	 (5 (<:h6 (heading)))
	 (t (error "Nesting too deep: ~S." (text part))))
       (nav-links))
     nil)
    (comment-part
    	;;;; regular comment
     (ecase state
       ((nil) (write-string "<p>" *yaclml-stream*))
       (:in-comment nil))
     (<:as-html (text part))
     :in-comment)))

;; Copyright (c) 2005, Edward Marco Baringer
;; All rights reserved. 
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;; 
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;    of its contributors may be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
