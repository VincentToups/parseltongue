;;; Copyright 2012, Vincent Toups
;;; This program is distributed under the terms of the GNU Lesser 
;;; General Public License (see license.txt).

;;;; parseltongue.lisp

(in-package #:parseltongue)

(defmethod next ((list list))
  (car list))

(defmethod rest-of ((list list))
  (cdr list))

(defmethod next ((string string))
  (subseq string 0 1))

(defmethod rest-of ((string string))
  (subseq string 1))

(defmethod empty? ((list list))
  (not list))

(defmethod empty? ((string string))
  (= 0 (length string)))

(defmethod prefix (o (string string))
  (concatenate 'string o string))

(defmethod prefix (o (list list))
  (cons o list))

(defmethod empty-of ((list list)) '())
(defmethod empty-of ((string string)) "")

(defun bind-form? (form)
  (if (listp form)
	  (let ((sigil (elt form 1)))
		(if (and (eq sigil '<-) 
				 (= 3 (length form)))
			t
			nil))
	  nil))

(defstruct parser-pair value input)
(defun parser-pair (value input) 
  (make-parser-pair :value value :input input))

(defun parser-bind (=p =>p)
  (lambda (input)
	(let ((rs (funcall =p input)))
	  (loop for r in rs append 
		   (funcall
			(funcall =>p (parser-pair-value r))
			(parser-pair-input r))))))

(defun parser-return (&rest items)
  (lambda (input)
	(mapcar 
	 (lambda (item)
	   (make-parser-pair :value item :input input))
	 items)))

(defun => (&rest items)
  (apply #'parser-return items))


(defmacro parser (&rest forms)
  `(labels ((m-return (&rest items) (apply #'parser-return items))
			(=> (&rest items) (apply #'parser-return items)))
	 (parser-helper ,@forms)))

(defmacro parser-helper (&rest forms)
  (labels ((bind-form? (form)
			 (if (listp form)
				 (let ((sigil (elt form 1)))
				   (if (and (eq sigil '<-) 
							(= 3 (length form)))
					   t
					   nil))
				 nil)))
	(cond
	  ((and (cdr forms)
			(bind-form? (car forms)))
	   `(parser-bind ,(elt (car forms) 2)
					 (lambda (,(elt (car forms) 0))
					   (parser-helper ,@(cdr forms)))))
	  ((cdr forms)
	   `(parser-bind ,(car forms)
					 (lambda (,(gensym))
					   (parser-helper ,@(cdr forms)))))
	  ((not (cdr forms))
	   (car forms)))))

(defmacro defun/var (name arg-list &rest body)
  `(progn
	 (defun ,name ,arg-list ,@body)
	 (declaim (function ,name))
	 (setq ,name #',name)))

(defun/var =nil (input)
  (list))

(defun/var =item (input)
  (if (not (empty? input))
	  (list (make-parser-pair :value (next input)
							  :input (rest-of input)))
	  '()))


(defmacro named-let (name bindings &rest body)
  `(labels ((,name ,(mapcar #'car bindings) ,@body))
	 (,name ,@(mapcar #'cadr bindings))))

(defun =>items (n)
  (lambda (input)
	(named-let recur 
		((acc '())
		 (n n)
		 (input input))
	  (if (or (<= n 0) (empty? input)) 
		  (list (parser-pair (reverse acc) input))
		  (recur (cons (next input) acc) (- n 1) (rest-of input))))))


(defun =>satisfies (fun)
  (parser 
   (x <- =item)
   (if (funcall fun x) (=> x) =nil)))

(defun =>or2 (=p1 =p2)
  (lambda (input)
	(let ((rs (funcall =p1 input)))
	  (if rs rs
		  (funcall =p2 input)))))

(defun =>or (&rest ps)
  (reduce #'=>or2 ps))

(defun =>and2 (=p1 =p2)
  (parser =p1 =p2))

(defun =>and (&rest ps)
  (reduce #'=>and2 ps))

(defun parser-plus (&rest ps)
  (apply #'=>and ps))

(defmacro defparser (name/args maybe-doc &rest body)
  (cond 
	((symbolp name/args)
	 (let ((name name/args)
		   (input-name (gensym))
		   (body (if (stringp maybe-doc) body (cons maybe-doc body)))
		   (doc (if (stringp maybe-doc) maybe-doc "")))
	   `(progn 
		  (declaim (function ,name))
		  (setq ,name
				(parser ,@body))
		  (defun ,name (,input-name)
			,doc
			(funcall ,name ,input-name)))))
	((listp name/args)
	 (let ((name (car name/args))
		   (args (cdr name/args))
		   (body (if (stringp maybe-doc) body (cons maybe-doc body)))
		   (doc (if (stringp maybe-doc) maybe-doc "")))
	   `(defun ,name ,args ,doc (parser ,@body))))))

(defparser (=>string s)
	(items <- (=>items (length s)))
  (let ((gs (apply #'strcat items)))
	(if (equal s gs) (m-return gs)
		=nil)))

(defparser (=>equal to)
	(i <- =item)
  (if (equal i to) (=> i) =nil))

(defparser (=>eq to)
	(i <- =item)
  (if (eq i to) (=> i) =nil))

(defun strcat (&rest s)
  (reduce (lambda (a b) (concatenate 'string a b)) s))

(defparser (=>reduce-concat =p)
	(r <- =p)
  (=> (reduce #'strcat r)))

(defun/var =rest (input)
  (list (make-parser-pair :value input :input input)))

(defun parse/first-result (=p input)
  (car (car (funcall =p input))))


(defparser (=>items->string n)
	(=>reduce-concat (=>items n)))

(defparser (=>list =p)
	(r <- =p)
  (=> (list r)))

(defun  =>maybe (=p)
  (lambda (input)
	(let ((rs (funcall =p input)))
	  (if rs rs 
		  (list (make-parser-pair :value nil :input input))))))


(defun  =>maybe-alternative (=p alt)
  (lambda (input)
	(let ((rs (funcall =p input)))
	  (if rs rs 
		  (list (make-parser-pair :value alt :input input))))))


(defun mapcar/deal (fun lst)
  "Map FUN over LST.  FUN returns a list of two items, the first
of which is a key the second of which is a value.  The VALUES are
accumulated at the KEYS in an ALIST which is returned."
  (named-let recur
	  ((a '())
	   (lst lst))
	(if (empty? lst) 
		(reverse-alist-keys a)
		(let* ((result (funcall fun (car lst)))
			   (key (car result))
			   (val (cadr result)))
		  (recur (alist-cons a key val)
				 (cdr lst))))))

(defun alist-cons (a key val)
  "CONS VAL onto the LIST held at KEY in the ALIST A."
  (named-let recur
	  ((a a)
	   (past '()))
	(cond
	  ((not a) (cons (cons key (list val)) past))
	  (t
	   (let* ((first (car a))
			  (lkey (car first))
			  (lval (cdr first)))
		 (if (equal lkey key)
			 (cons (cons lkey (cons val lval)) past)
			 (recur
			  (cdr a)
			  (cons first past))))))))

(defun alist (a key)
  "Return the value at KEY or NIL."
  (cond 
	((not a) nil)
	(t
	 (let ((first (car a)))
	   (if (equal (car first) key)
		   (cdr first)
		   (alist (cdr a) key))))))

(defun reverse-alist-keys (a)
  "Reverse the lists held at each key in A."
  (loop for (k . v) in a collect
	   (cons k (reverse v))))


(defun zero-plus-more-step (substate parser)
  "Apply PARSER to the CDR of substate.  If it succeeds, cons the
result onto the list in the CAR of substate and indicate CONTINUE
for MAPCAR/DEAL.  If PARSER on CDR of substate FAILS, then
reverse the CAR of SUBSTATE and return this value consed with the
last INPUT state."
  (let* ((mrv (parser-pair-value substate))
		 (input (parser-pair-input substate))
		 (r (funcall parser input)))
	(if r (list 
		   :continue 
		   (mapcar (lambda (subr)
					 (let ((r (parser-pair-value subr))
						   (rest (parser-pair-input subr)))
					   (parser-pair (cons r mrv) rest)))
				   r))
		(list :terminate 
			  (parser-pair (reverse mrv) input)))))


(defun =>zero-plus-more (p)
  "Produce a parser which parses P zero or more times and monadically
returns the results in a list."
  (lambda 
	  (input)
	(named-let recur 
		((terminals nil)
		 (continuers (funcall (=>list p) input)))
	  (if (empty? continuers)
		  (if (empty? terminals) 
			  (list (parser-pair nil input))
			  terminals) 
		  (let* ((split-tbl
				  (mapcar/deal 
				   (lambda (c)
					 (zero-plus-more-step c p))
				   continuers))
				 (new-continuers (alist split-tbl :continue))
				 (new-terminals (alist split-tbl :terminate)))
			(recur (append terminals new-terminals)
				   (reduce #'append new-continuers)))))))

(defparser (=>one-plus-more =p)
	(r <- =p)
  (rest <- (=>zero-plus-more =p))
  (=> (cons r rest)))



