;;;; package.lisp

(defpackage #:parseltongue
  (:export 
   "next"
   "rest-of"
   "empty?"
   "bind-form"
   "parser-return"
   "parser-bind"
   "parser-pair"
   "=item"
   "=>items"
   "defparser"
   "parser"
   "=>string"
   "=>equal"
   "=>eq"
   "=rest"
   "=>reduce-concat"
   "parse/first-result"
   "=>items->string"
   "=>satisfies"
   "=>or"
   "=>and"
   "=>maybe"
   "=>maybe-alternative"
   "=>zero-plus-more"
   "=>one-plus-more")
  (:use #:cl #:lisp-unit))


