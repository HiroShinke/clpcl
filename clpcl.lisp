

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
	 if
	   (let ((r (funcall p text pos)))
	     (match r
	       ((success :pos pos1 :value v)
		(setq pos pos1)
		(setq ret (cons v ret))
		nil
		)
	       ((failure :pos pos1)
		(if (/= pos pos1)
		    (setq success nil)
		    (setq success t))
		t)
	       )
	     )
	   
	 return nil
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


(defun clpcl-seq (&rest ps)
  (lambda (text pos)
    (let ((pos0 pos)
	  (ret  nil)
	  (failed nil))
      (loop for p in ps
	 if
	   (match (funcall p text pos0)
	     ((success :pos pos1 :value v)
	      (setq pos0 pos1)
	      (setq ret (cons v ret))
	      nil)
	     ((failure :pos pos1)
	      (setq pos0 pos1)
	      (setq failed t)
	      t)
	     (otherwise
	      (error "xxxxxxx"))
	     )
	   return nil
	   )
      (if failed
	  (failure pos0)
	  (success pos0 (reverse ret))))))

(defun clpcl-or (&rest ps)

  (lambda (text pos)

    (let ((ret nil))

      (loop for p in ps
	 if
	   (let ((r (funcall p text pos)))
	     (match r
	       ((success)
		(setq ret r)
		t)
	       ((failure :pos pos1)
		(if (/= pos pos1)
		    (progn
		      (setq ret r)
		      t)
		    )
		)
	       (otherwise
		(error "clpcl-or : error")
		)
	       )
	     )
	 return nil
	 )
	      
      (if ret
	  ret
	  (failure pos))
      )
    )
  )

(defmacro clpcl-lazy (p0)
  `(clpcl--lazy-helper (lambda () ,p0)))
  
(defun clpcl--lazy-helper (p0)
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
  (let ((vs (mapcar
	     (lambda (e)
	       (if (and (listp e)
			(car e))
		   (car e)
		   (intern "clpcl-let")))
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
      (lambda ,vs ,@body))
    ))


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
     (clpcl--bind-template p (if s s (intern "s")) rest body))
    ((cons p rest)
     (clpcl--bind-template p (intern "s") rest body))     
    (nil
     `(clpcl-return (progn ,@body))
     )
    )
  )

(defun clpcl--bind-template (p s as body)
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



    

  
