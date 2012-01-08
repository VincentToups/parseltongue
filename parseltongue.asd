;;;; parseltongue.asd

(asdf:defsystem #:parseltongue
 :serial t
 :depends-on (lisp-unit) 
 :components ((:file "package")
			  (:file "parseltongue")
			  (:file "tests")))


