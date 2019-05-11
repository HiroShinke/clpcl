

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
	   :clpcl-eof
	   :clpcl-seq
	   :clpcl-string-literal
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

(defgeneric parse (parser text pos))

(defstruct <parser>) 

(defmethod parse ((parser <parser>) text pos)
  (error "require implement") )

;;(optima:defpattern success (pos val)
;;  `(success :pos ,pos :value ,val))

;;(optima:defpattern failure (pos)
;;  `(failuren :pos ,pos))


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


(defun clpcl-parse (parser text)
  (parse parser text 0))

(defun clpcl-token (p &optional spaces)
  (if (null spaces)
      (setq spaces (clpcl-regexp "\\s*")))
  (clpcl-try
   (clpcl-let (spaces (v p)) v)
   )
  )

(defstruct ( <try>
	     (:constructor <try>(inner) )
	     (:include <parser> ) )
  inner
  )

(defun clpcl-try (p) (<try> p) )
  
(defmethod parse ((p <try>) text pos)
    (let* ((inn (<try>-inner p))
	   (r (parse inn text pos)))
      (match r
	((failure :pos pos0)
	 (declare (ignore pos0))
	 (failure pos))
	(otherwise
	 r)
	)
      )
  )

(defstruct ( <regexp>
	     (:constructor <regexp>(regexp scanner) )
	     (:include <parser> ) )
  regexp
  scanner
  )

(defun clpcl-regexp (regexp)
  (let ((scanner (create-scanner (concatenate
				  'string
				  "^"
				  regexp))))
    (<regexp> regexp scanner)
    )
  )

(defmethod parse ((p <regexp>) text pos)
  (multiple-value-bind
	(s e rs re)
      (scan (<regexp>-scanner p) text :start pos)
    (declare (ignore rs re))
    (if s
	(success e (subseq text s e))
	(failure pos))))

(defstruct (<string>
	     (:constructor <string>(str) )
	     (:include <parser> )
	     )
  str
  )

(defun clpcl-string (str0)
  (<string> str0)
  )

(defmethod parse ((p <string>) text pos)

    (let* ((str0 (<string>-str p))
	   (len (length str0))
	   (lena (length text))
	   (str1 (subseq text pos (min (+ pos len)
				       lena))))
      (if (string= str0 str1)
	  (success (+ pos len) str1)
	  (failure pos)
	  )
      )
  )

(defstruct ( <string-literal> (:constructor <string-literal>(delim) )
			      (:include <parser>))
  delim
  )

(defun clpcl-string-literal (&optional (q-char #\"))
  (<string-literal> q-char))


(defmethod parse ((p <string-literal>) text pos)

  "This parser parses java-like string literals"

  (let* (
	 (q-char (<string-literal>-delim p))
	 (qexp  (if (null q-char)
		    "\"|\'"
		    (concatenate
		     'string
		     (list q-char))))
	 (not-qexp (if
		    (null q-char)
		    "[^\"\'\\\\]+"
		    (concatenate
		     'string
		     "[^" (list q-char) "\\\\]+")))

	 (xxx (clpcl-let ((op (clpcl-regexp qexp))
			  (ss (clpcl-many
			       (clpcl-or
				(clpcl-regexp not-qexp)
				(clpcl-regexp "\\\\'")
				(clpcl-regexp "\\\\\"")
				(clpcl-regexp "\\\\(r|n|f|t|b)")
				(clpcl-regexp "\\\\[0-7]{3}")
				(clpcl-regexp "\\\\x[0-9a-fA-F]{2}")
				(clpcl-regexp "\\\\u[0-9a-fA-F]{4}")
				(clpcl-regexp "\\\\.")
				)))
			  (cp (clpcl-regexp qexp)))
			 
			 (concatenate 'string
				      op 
				      (apply #'concatenate 'string ss )
				      cp)
			 )
	   )
	 )

    (parse xxx text pos)
    )
  )

(defparameter *debug-stack* nil)


(defstruct ( <debug> (:constructor <debug>(label parser) )
		      (:include <parser>))
  label
  parser
  )

(defun clpcl-debug (label p)
  (<debug> label p))


(defmethod parse ((p <debug>) text pos)
  (let ((label (<debug>-label p)))
    (format t (make-string (length *debug-stack*) :initial-element #\ ))
    (format t "label=~S,start,pos=~S~%" label pos)
    (push label *debug-stack*)
    (let ((r (parse (<debug>-parser p) text pos)))
      (pop *debug-stack*)
      (format t (make-string (length *debug-stack*) :initial-element #\ ))
      (format t "label=~S,end,ret=~S~%" label r)
      r)))

(defstruct ( <many> (:constructor <many>(inner) )
		    (:include <parser>))
  inner
  )

(defun clpcl-many (p)
  (<many> p))


(defmethod parse ((p <many>) text pos)

  (let ((success t)
	(ret nil))
    
    (loop
       while
	 (let ((r (parse (<many>-inner p) text pos)))
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

(defstruct (<option> (:constructor <option>(inner))
		     (:include <parser>))
  inner
  )

(defun clpcl-option (p)
  (<option> p))

(defmethod parse ((p <option>) text pos)
  (let ((r (parse (<option>-inner p) text pos)))
    (match r
      ((success) r)
      ((failure :pos pos1)
       (if (= pos pos1)
	   (success pos1 nil)
	   r)))))

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

(defstruct (<sep-end-by-1>
	     (:constructor <sep-end-by-1>(parser sep))
	     (:include <parser>))
  parser
  sep
  )

(defun clpcl-sep-end-by-1 (p sep)
  (<sep-end-by-1> p sep)
  )

(defmethod parse ((p <sep-end-by-1>) text pos)

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
  (let ((ret nil)
	(err t))
    (loop
       while
	 (let ((r (parse (<sep-end-by-1>-parser p) text pos)))
	   (match r
	     ((success :pos pos1 :value v)
	      (setq pos pos1)
	      (setq ret (cons v ret))
	      (setq err nil)
	      (let ((s (parse (<sep-end-by-1>-sep p) text pos)))
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

(defstruct (<many-till>
	     (:constructor <many-till>(parser till))
	     (:include <parser>))
  parser
  till
  )

(defun clpcl-many-till (p till)
  (<many-till> p till)
  )
  

(defmethod parse ((p <many-till>) text pos) 

  "manyTill p end applies parser p zero or more times until 
     parser end succeeds. Returns the list of values returned by p."

  (let ((ret nil)
	(err nil))
    (loop
       while
	 (let ((r (parse (<many-till>-till p) text pos)))
	   (match r
	     ((success :pos pos1)
	      (setq pos pos1)
	      nil)
	     (otherwise
	      (let ((r1 (parse (<many-till>-parser p) text pos)))
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
	(success pos (reverse ret)))
    )
  )


(defstruct (<lookahead>
	     (:constructor <lookahead>(parser))
	     (:include <parser>))
  parser
  )

(defun clpcl-lookahead (p)
  (<lookahead> p)
  )


(defmethod parse ((p <lookahead>) text pos)
  (let ((r (parse (<lookahead>-parser p) text pos)))
    (match r
      ((success :value v)
       (success pos v))
      (otherwise
       r))))

(defstruct (<not-followed>
	     (:constructor <not-followed>(parser))
	     (:include <parser>))
  parser
  )

(defun clpcl-not-followed (p)
  (<not-followed> p)
  )

(defmethod parse ((p <not-followed>) text pos)

  "notFollowedBy p only succeeds when parser p fails. 
   This parser does not consume any input. 
   This parser can be used to implement the 'longest match' rule. "

  (let ((r (parse (<not-followed>-parser p) text pos)))
    (match r
      ((success)
       (failure pos))
      ((failure)
       (success pos nil))))
  )

(defun clpcl-eof ()
  (clpcl-not-followed (clpcl-regexp "."))
  )

(defstruct(<seq> (:constructor <seq>(parsers))
		 (:include <parser>))
  parsers
  )

(defun clpcl-seq (&rest ps)
  (<seq> ps))


(defmethod parse ((x <seq>) text pos) 
  
  (let ((pos0 pos)
	(ret  nil)
	(failed nil))

    (loop for p in (<seq>-parsers x)
       while
	 (match (parse p text pos0)
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
	(success pos0 (reverse ret))))
  )


(defstruct(<or> (:constructor <or>(parsers))
		 (:include <parser>))
  parsers
  )

(defun clpcl-or (&rest ps)
  (<or> ps))


(defmethod parse ((x <or>) text pos)

  (let ((ret nil))

    (loop for p in (<or>-parsers x)
       while
	 (let ((r (parse p text pos)))
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


(defstruct ( <lazy> (:constructor <lazy>(p0) )
		    (:include <parser>))
  p0
  parser
  )

(defmacro clpcl-lazy (p0)
  `(<lazy> (lambda () ,p0)))

(defmethod parse ((p <lazy>) text pos)
  (if (not (<lazy>-parser p))
      (setf (<lazy>-parser p) (funcall (<lazy>-p0 p))))
  (parse (<lazy>-parser p) text pos)
  )


(defstruct ( <bind> (:constructor <bind>(parser func) )
		    (:include <parser>))
  parser
  func
  )

(defun clpcl-bind (p func)
  (<bind> p func) )


(defmethod parse ((p <bind>) text pos)
  (let ((r (parse (<bind>-parser p) text pos)))
    (match r
      ((success :pos pos :value v)
       (success
	pos
	(funcall (<bind>-func p) v)))
      (otherwise
       r))))

(defun clpcl-bind-seq (p action)
  (clpcl-bind
   p
   (lambda (x)
     (apply action x))))



(defstruct ( <mbind> (:constructor <mbind>(parser func))
		     (:include <parser>) )
  parser
  func
  )


(defun clpcl-m-bind (p func)
  (<mbind> p func))


(defmethod parse ((x <mbind>) text pos)
  (let ((r (parse (<mbind>-parser x) text pos)))
    (match r
      ((success :pos pos :value v)
       (parse (funcall (<mbind>-func x) v) text pos))
      (otherwise
       r))))

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


(defstruct (<return> (:constructor <return>(value))
		     (:include <parser>))
  value
  )

(defun clpcl-return (v) (<return> v))


(defmethod parse ((p <return>) text pos)
  (declare (ignore text))
  (success pos (<return>-value p)))


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
  
