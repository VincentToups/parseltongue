;;; Copyright 2012, Vincent Toups
;;; This program is distributed under the terms of the GNU Lesser 
;;; General Public License (see license.txt).

;;;; tests.lisp

(in-package #:parseltongue)

(define-test next
  (assert-equal 5 (next '(5 10 15)))
  (assert-equal "a" (next "abc")))

(define-test rest-of 
  (assert-equal '(10 15) 
				(rest-of '(5 10 15)))
  (assert-equal "bc" 
				(rest-of "abc")))

(define-test empty? 
  (assert (empty? '()))
  (assert (empty? ""))
  (assert-equal nil
				(empty? '(a b)))
  (assert-equal nil
				(empty? "abc")))

(define-test bind-form
  (assert (bind-form? '(a <- x)))
  (assert (not (bind-form? '(a b c))))
  (assert (not (bind-form? '(a <- b c)))))

(define-test parser-return 
  (assert-equalp
   (list (parser-pair 'a nil)
		 (parser-pair 'b nil))
   (funcall (parser-return 'a 'b) nil)))

(define-test =item 
  (assert-equalp
   (list (parser-pair "a" "bc"))
   (=item "abc"))
  (assert-equalp 
   '()
   (=item "")))

(define-test =>items
  (assert-equalp 
   (list (parser-pair '(a b c) '(d)))
   (funcall (=>items 3) '(a b c d))))

(define-test =>satisfies
  (assert-equalp 
   (list (parser-pair "a" "bc"))
		 (funcall 
		  (=>satisfies
		   (lambda (arg)
			 (string= arg "a")))
		  "abc"))
  (assert-equalp 
   (list)
   (funcall 
	(=>satisfies
	 (lambda (arg)
	   (string= arg "a")))
	"bbc")))

(define-test =>or
  (assert-equalp 
   (list (parser-pair "a" "bc"))
   (funcall (=>or 
			 (=>string "a")
			 (=>string "b"))
			"abc"))
  (assert-equalp 
   (list (parser-pair "b" "bc"))
   (funcall (=>or 
			 (=>string "a")
			 (=>string "b"))
			"bbc"))
  (assert-equalp 
   (list)
   (funcall (=>or 
			 (=>string "a")
			 (=>string "b"))
			"cbc")))

(define-test =>and
  (assert-equalp
   (list
	(parser-pair "b" "c"))
	(funcall
	 (=>and (=>string "a")
			(=>string "b"))
	 "abc"))
  (assert-equalp
   (list
	)
   (funcall
	(=>and (=>string "a")
		   (=>string "b"))
	"acc"))
  (assert-equalp
   (list
	)
   (funcall
	(=>and (=>string "a")
		   (=>string "b"))
	"bbc")))

(define-test strcat 
  (assert-equalp 
   "abcdef"
   (strcat "a" "b" "c" "d" "e" "f")))

(define-test =>maybe 
  (assert-equalp 
   (list (parser-pair "a" "bc"))
   (funcall (=>maybe (=>string "a"))
			"abc"))
  (assert-equalp 
   (list (parser-pair nil "cbc"))
   (funcall (=>maybe (=>string "a"))
			"cbc")))

(define-test =>maybe-alternative 
  (assert-equalp 
   (list (parser-pair "a" "bc"))
   (funcall (=>maybe-alternative (=>string "a") 'alt)
			"abc"))
  (assert-equalp 
   (list (parser-pair 'alt "cbc"))
   (funcall (=>maybe-alternative (=>string "a") 'alt)
			"cbc")))

(define-test =>zero-plus-more 
  (assert-equalp 
   (list (parser-pair
		  (list "a" "b" "b" "a") "rocks"))
   (funcall
	(=>zero-plus-more
	 (=>or (=>string "a")
		   (=>string "b")))
	"abbarocks"))
  (assert-equalp 
   (list (parser-pair
		  (list) "rocks"))
   (funcall
	(=>zero-plus-more
	 (=>or (=>string "a")
		   (=>string "b")))
	"rocks"))
  (assert-equalp 
   (list (parser-pair
		  (list "a" "b" "b") "rocks"))
   (funcall
	(=>zero-plus-more
	 (=>or (=>string "a")
		   (=>string "b")))
	"abbrocks")))

(define-test =>one-plus-more
  (assert-equalp 
   (list (parser-pair (list "a" "a" "a") "b"))
   (funcall (=>one-plus-more (=>string "a"))
			"aaab"))
  (assert-equalp 
   nil
   (funcall (=>one-plus-more (=>string "a"))
			"bbbb")))


(run-tests)

