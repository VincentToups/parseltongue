;;; Copyright 2012, Vincent Toups
;;; This program is distributed under the terms of the GNU Lesser 
;;; General Public License (see license.txt).
;;;; package.lisp

(defpackage #:parseltongue
  (:export 
   "NEXT"
   "REST-OF"
   "EMPTY?"
   "BIND-FORM"
   "PARSER-RETURN"
   "PARSER-BIND"
   "PARSER-PAIR"
   "=ITEM"
   "=>ITEMS"
   "DEFPARSER"
   "PARSER"
   "=>STRING"
   "=>EQUAL"
   "=>EQ"
   "=REST"
   "=>REDUCE-CONCAT"
   "PARSE/FIRST-RESULT"
   "=>ITEMS->STRING"
   "=>SATISFIES"
   "=>OR"
   "=>AND"
   "=>MAYBE"
   "=>MAYBE-ALTERNATIVE"
   "=>ZERO-PLUS-MORE"
   "=>ONE-PLUS-MORE")
  (:use #:cl #:lisp-unit))


