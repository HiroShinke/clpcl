

(in-package :cl-user)
(defpackage :clpcl
  (:use :cl :ppcre :optima)
  (:export :success
	   :failure
	   :clpcl-let
	   :clpcl-bind
	   :clpcl-debug
	   :clpcl-def-parsers
	   :clpcl-regexp
	   :clpcl-lookahead
	   :clpcl-many
	   :clpcl-many-1	   
	   :clpcl-many-till
	   :clpcl-not-followed
	   :clpcl-seq
	   :clpcl-string
	   :clpcl-or
	   :clpcl-token
	   :clpcl-try
	   :clpcl-option
	   :clpcl-parse
	   :clpcl-paren
	   :clpcl-sep-by
	   :clpcl-sep-by-1
	   :clpcl-end-by
	   :clpcl-end-by-1
	   :clpcl-sep-end-by
	   :clpcl-sep-end-by-1
	   :clpcl-let*
	   :clpcl-lazy
	   :clpcl-return
	   :clpcl-chainr-1
	   :clpcl-chainl-1
	   :clpcl-*)
  )
(in-package :clpcl)

(defstruct (success (:constructor success(pos value)))
  pos
  value
  )
(defstruct (failure (:constructor failure(pos)))
  pos
  )

;;(optima:defpattern success (pos val)
;;  `(success :pos ,pos :value ,val))

;;(optima:defpattern failure (pos)
;;  `(failuren :pos ,pos))


(defun clpcl-parse (parser text)
  (funcall parser text 0))

(defun clpcl-token (p)
  (clpcl-try
   (clpcl-let
    ((nil (clpcl-regexp "\\s*"))
     (v p))
    v)
   )
  )

(defun clpcl-try (p)
  (lambda (text pos)
    (let ((r (funcall p text pos)))
      (match r
	((failure :pos pos0)
	 (declare (ignore pos0))
	 (failure pos))
	(otherwise
	 r)))))

(defun clpcl-regexp (regexp)
  (let ((scanner (create-scanner (concatenate
				  'string
				  "^"
				  regexp))))
    (lambda (text pos)
      (multiple-value-bind
	    (s e rs re)
	  (scan scanner text :start pos)
	(declare (ignore rs re))
	(if s
	    (success e (subseq text s e))
	    (failure pos))))))

(defun clpcl-string (&optional (q-char #\"))

  (lambda (text pos0)

    (let ((l (length text))
	  (pos pos0)
	  (state 0)
	  (s nil)
	  (e nil)
	  (q q-char)
	  )

      (loop
	 while (and (< pos l) (< state 2))
	 do
	   (let ((c (aref text pos)))
	     (cond
	       ((= state 0)
		(if (null q)  ;; if nil specified
		    (if (or (char= c #\") (char= c #\'))
			(setq q c)
			(setq q #\")))
		(if (char= c q)
		    (progn (setq state 1)
			   (setq s pos)
		           (setq q c)
			   )
		    (setq state 2)
		    )
		)
	       ((= state 1)
		(if (char= c q)
		    (progn (setq state 2)
			   (setq e (+ pos 1))
			   )
		    )
		)
	       )
	     (setq pos (+ pos 1))
	     )
	   )
      (if (and s e)
	  (success e (subseq text s e))
	  (failure pos0))
      )
    )
  )

(defun clpcl-debug (label p)
  (lambda (text pos)
    (let ((r (funcall p text pos)))
      (format t "label=~S,ret=~S~%" label r)
      r)))

(defun clpcl-many (p)

  (lambda (text pos)

    (let ((success t)
	  (ret nil))

      (loop
	 while
	   (let ((r (funcall p text pos)))
	     (match r
	       ((success :pos pos1 :value v)
		(setq pos pos1)
		(setq ret (cons v ret))
		t
		)
	       ((failure :pos pos1)
		(if (/= pos pos1)
		    (progn
		      (setq pos pos1)
		      (setq success nil)
		      )
		    (setq success t))
		nil)
	       )
	     )
	   )
	   
      (if success
	  (success pos (reverse ret))
	  (failure pos)
	  )
      )
    )
  )

(defun clpcl-many-1 (p)
  (clpcl-let
   ((v p)
    (vs (clpcl-many p)))
   (cons v vs)))


(defun stack-machine (ops
		      vs
		      flip)
  (loop
     while ops
     do (
	 let* ((o  (car ops))
	       (v1 (car vs))
	       (v2 (cadr vs))
	       (v  (funcall (funcall flip o) v1 v2)))
	  (setq ops (cdr ops))
	  (setq vs (cons v (cddr vs)))
	  )
       )
  (car vs)
  )

(defun chain-parser (p op eval)

  (let* ((opp   (clpcl-seq op p))
	 (chain (clpcl-seq p (clpcl-many opp))))

    (clpcl-bind-seq
     chain
     (lambda (v opvs)
       (let ((vs (cons v (mapcar #'cadr opvs)))
	     (ops (mapcar #'car opvs)))
	 (funcall eval ops vs)
	 )
       )
     )
    )
  )


(defun clpcl-chainl-1 (p op)
  (chain-parser
   p
   op
   (lambda (ops vs)
     (stack-machine ops
			  vs
			  (lambda (n) n)
			  )))
  )

(defun clpcl-chainr-1 (p op)
  (chain-parser
   p
   op
   (lambda (ops vs)
     (stack-machine (reverse ops)
			  (reverse vs)
			  (lambda (x) (lambda (n m)
					(funcall x m n)))
			  )))
  )

(defun clpcl-paren (po p pc)
  (clpcl-let (po (x p) pc) x)
  )

(defun clpcl-option (p)
  (lambda (text pos)
    (let ((r (funcall p text pos)))
      (match r
	((success) r)
	((failure :pos pos1)
	 (if (= pos pos1)
	     (success pos1 nil)
	     r))))))

(defun clpcl-sep-by (p sep)
  
  "sepBy p sep parses zero or more occurrences of p, 
   separated by sep. 
   Returns a list of values returned by p."
  (clpcl-option (clpcl-sep-by-1 p sep))
  )

(defun clpcl-sep-by-1 (p sep)

  "sepBy1 p sep parses one or more occurrences of p, 
   separated by sep. 
   Returns a list of values returned by p."

  (let* (
	 (sepp   (clpcl-let (sep (v p)) v))
	)
    (clpcl-let ((v p)
		(vs (clpcl-many sepp)))
	      (if v
		  (cons v vs)
		nil)
	      )
    )
  )

(defun clpcl-end-by (p sep)

  "endBy p sep parses zero or more occurrences of p, 
   separated and ended by sep. 
   Returns a list of values returned by p.

   allow patterns bellow.

   (nothing)
   p sep
   p sep p sep
   p sep p sep p sep

   "

  (clpcl-or (clpcl-let (sep) nil)
	    (clpcl-option (clpcl-end-by-1 p sep)))
  )

(defun clpcl-end-by-1 (p sep)

  "endBy p sep parses one or more occurrences of p, 
   separated and ended by sep. 
   Returns a list of values returned by p.

   allow patterns bellow.

   p sep
   p sep p sep
   p sep p sep p sep
  "

  (let (
	(psep   (clpcl-let ((v p) sep) v))
	)
    (clpcl-many-1 psep)
    )
  )

(defun clpcl-sep-end-by (p sep)

  "sepEndBy p sep parses zero or more occurrences of p, 
   separated and optionally ended by sep, ie. 
   haskell style statements. Returns a list of values returned by p.

   allow

   (nothing)
   sep
   p
   p sep
   p sep p 
   p sep p sep

   ....
  "

  (clpcl-or (clpcl-let (sep) nil)
	    (clpcl-option (clpcl-sep-end-by-1 p sep)))
  )

(defun clpcl-sep-end-by-1 (p sep)

  "sepEndBy p sep parses one or more occurrences of p, 
   separated and optionally ended by sep, ie. 
   haskell style statements. Returns a list of values returned by p.

   allow

   p
   p sep
   ....
   p sep p sep p
   p sep p sep p sep

   "
  (lambda (text pos)
    (let ((ret nil)
	  (err t))
      (loop
	 while
	   (let ((r (funcall p text pos)))
	     (match r
	       ((success :pos pos1 :value v)
		(setq pos pos1)
		(setq ret (cons v ret))
		(setq err nil)
		(let ((s (funcall sep text pos)))
		  (match s
		    ((success :pos pos1)
		     (setq pos pos1)
		     t)
		    ((failure :pos pos1)
		     (if (/= pos pos1)
			 (progn
			   (setq err t)
			   (setq pos pos1)))
		     nil)))
		
		)
	       ((failure :pos pos1)
		(if (/= pos pos1)
		    (progn
		      (setq err t)
		      (setq pos pos1)))
		nil)))
	   )
      (if err
	  (failure pos)
	  (success pos ret))
      )
    )
  )


(defun clpcl-many-till (p till)

    "manyTill p end applies parser p zero or more times until 
     parser end succeeds. Returns the list of values returned by p."

    (lambda (text pos)
      (let ((ret nil)
	    (err nil))
	(loop
	   while
	     (let ((r (funcall till text pos)))
	       (match r
		 ((success :pos pos1)
		  (setq pos pos1)
		  nil)
		 (otherwise
		  (let ((r1 (funcall p text pos)))
		    (match r1
		      ((success :pos pos1 :value v)
		       (setq pos pos1)
		       (setq ret (cons v ret))
		       t)
		      ((failure :pos pos1)
		       (setq pos pos1)
		       (setq err t)
		       nil))))))
	     )
	(if err
	    (failure pos)
	    (success pos ret))
	)
      )
    )

(defun clpcl-lookahead (p)

  (lambda (text pos)
    (let ((r (funcall p text pos)))
      (match r
	((success :value v)
	 (success pos v))
	(otherwise
	 r)))))

(defun clpcl-not-followed (p)

  "notFollowedBy p only succeeds when parser p fails. 
   This parser does not consume any input. 
   This parser can be used to implement the 'longest match' rule. "

  (lambda (text pos)
    (let ((r (funcall p text pos)))
      (match r
	((success)
	 (failure pos))
	((failure)
	 (success pos nil)))))
  )

(defun clpcl-seq (&rest ps)
  (lambda (text pos)
    (let ((pos0 pos)
	  (ret  nil)
	  (failed nil))
      (loop for p in ps
	 while
	   (match (funcall p text pos0)
	     ((success :pos pos1 :value v)
	      (setq pos0 pos1)
	      (setq ret (cons v ret))
	      t)
	     ((failure :pos pos1)
	      (setq pos0 pos1)
	      (setq failed t)
	      nil)
	     (otherwise
	      (error "xxxxxxx"))
	     )
	   )
      (if failed
	  (failure pos0)
	  (success pos0 (reverse ret))))))

(defun clpcl-or (&rest ps)

  (lambda (text pos)

    (let ((ret nil))

      (loop for p in ps
	 while
	   (let ((r (funcall p text pos)))
	     (match r
	       ((success)
		(setq ret r)
		nil)
	       ((failure :pos pos1)
		(if (/= pos pos1)
		    (progn
		      (setq ret r)
		      nil)
		    t
		    )
		)
	       (otherwise
		(error "clpcl-or : error")
		)
	       )
	     )
	 )
	      
      (if ret
	  ret
	  (failure pos))
      )
    )
  )

(defmacro clpcl-lazy (p0)
  `(lazy-helper (lambda () ,p0)))
  
(defun lazy-helper (p0)
  (let ((p nil))
    (lambda (text pos)      
      (if (not p) (setq p (funcall p0)))
      (funcall p text pos))))


(defun clpcl-bind (p action)
  (lambda (text pos)
    (let ((r (funcall p text pos)))
      (match r
	((success :pos pos :value v)
	 (success
	  pos
	  (funcall action v)))
	(otherwise
	 r)))))

(defun clpcl-bind-seq (p action)
  (clpcl-bind
   p
   (lambda (x)
     (apply action x))))

(defmacro clpcl-let (arglist &rest body)
  (let* ((igs nil)       ;; ignore list
	 (vs (mapcar
	      (lambda (e)
		(if (and (listp e)
			 (car e))
		    (car e)
		    (progn
		      (let ((s (gensym "clpcl-let")))
			(setq igs (cons s igs))
			s))
		    )
		)
	      arglist))
	 (ps (mapcar
	      (lambda (e)
		(if (listp e)
		    (cadr e)
		    e))
	      arglist))
	 )
    `(clpcl-bind-seq
      (clpcl-seq ,@ps)
      (lambda ,vs
	(declare (ignore ,@igs))
		 ,@body))
    )
  )


(defun clpcl-m-bind (p func)
  (lambda (text pos)
    (let ((r (funcall p text pos)))
      (match r
	((success :pos pos :value v)
	 (funcall (funcall func v) text pos))
	(otherwise
	 r)))))

(defmacro clpcl-let* (args &rest body)
  (clpcl-let*-helper args body)
  )

(defun clpcl-let*-helper (args body)  
  (match args
    ((cons (list s p) rest)
     (bind-template p (if s s (gensym "s")) rest body))
    ((cons p rest)
     (bind-template p (gensym "s") rest body))     
    (nil
     `(clpcl-return (progn ,@body))
     )
    )
  )

(defun bind-template (p s as body)
  `(clpcl-m-bind
    ,p
    (lambda (,s)
      ,(clpcl-let*-helper as body)
      )
    )
  )

(defun clpcl-return (v)
  (lambda (text pos)
    (declare (ignore text))
    (success pos v)))


(defun lazy-wrapper (p vs accm)
  (if (atom p)
      (if (and (member p vs) (not (member p accm)))
	  `(clpcl-lazy ,p)
	  p
	  )
      (cons (lazy-wrapper (car p) vs accm)
	    (lazy-wrapper (cdr p) vs accm))
      )
  )

(defun def-parsers-helper (vs ps accm)
  (if vs 
      (let ((v (car vs))
	    (p (car ps)))
	(cons `(setq ,v ,(lazy-wrapper p vs accm))
	      (def-parsers-helper (cdr vs)
				  (cdr ps)
				  (cons v accm)))))
  )

(defmacro clpcl-def-parsers (assigns &rest body)
  
  " 
  Define a set of parsers.
  The parsers will be defined in order.
  If the parser refered in righthand side is recursive and not defined at that time
  the parser reference will be wrapped in clpcl-lazy
  "

  (let ((vs (mapcar #'car assigns))
	(ps (mapcar #'cadr assigns)))
    `(let ,(mapcar (lambda (n) (list n nil)) vs)
       ,@(def-parsers-helper vs ps nil)
       ,@body
       )
    )
  )
  
