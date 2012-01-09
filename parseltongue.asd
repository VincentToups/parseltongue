;;; Copyright 2012, Vincent Toups
;;; This program is distributed under the terms of the GNU Lesser 
;;; General Public License (see license.txt).

;;;; parseltongue.asd

(asdf:defsystem #:parseltongue
 :serial t
 :version "0.0.1"
 :author "Vincent Toups"
 :maintainer "Vincent Toups"
 :description "Parseltongue"
 :long-description "A monadic parser combinator library with Haskell do-like notation."
 :license "LGPL"
 :depends-on (lisp-unit) 
 :components ((:file "package")
			  (:file "parseltongue")
			  (:file "tests")))


