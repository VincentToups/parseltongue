;;; Copyright 2012, Vincent Toups
;;; This program is distributed under the terms of the GNU Lesser 
;;; General Public License (see license.txt).

;;;; parseltongue.asd

(asdf:defsystem #:parseltongue
 :serial t
 :depends-on (lisp-unit) 
 :components ((:file "package")
			  (:file "parseltongue")
			  (:file "tests")))


