

(in-package :cl-user)
(defpackage :clpcl
  (:use :cl :ppcre :optima)
  (:export :success
	   :failure
	   :clpcl-let
	   :clpcl-debug
	   :clpcl-regexp
	   :clpcl-many
	   :clpcl-seq
	   :clpcl-or
	   :clpcl-token
	   :clpcl-try
	   :clpcl-parse
	   :clpcl-let*
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
    ((nil (clpcl-regexp "\\s+"))
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
  (let ((scanner (create-scanner regexp)))
    (lambda (text pos)
      (multiple-value-bind
	    (s e rs re)
	  (scan scanner text :start pos)
	(declare (ignore rs re))
	(if s
	    (success e (subseq text s e))
	    (failure pos))))))

(defun clpcl-debug (label p)
  (lambda (text pos)
    (let ((r (funcall p text pos)))
      (format t "label=~S,ret=~S" label r)
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
		    (setq success nil)
		    (setq success t))
		t
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
		      (let ((s (intern "clpcl-let")))
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
     (bind-template p (if s s (intern "s")) rest body))
    ((cons p rest)
     (bind-template p (intern "s") rest body))     
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



    

  
