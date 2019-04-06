
;;(ql:quickload :cl-ppcre :silent t)
;;(ql:quickload :optima :silent t)

;;(shadowing-import :scan :cl-ppcre)
;;(import ppcre:scan)
;;(import ppcre:create-scanner)
(use-package :ppcre)
;;(use-package :optima)
;;(shadowing-import 'optima:match)
;;(use-package :optima)

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

(defun clpcl-regexp (regexp)
  (let ((scanner (create-scanner regexp)))
    (lambda (text pos)
      (multiple-value-bind
	    (s e rs re)
	  (scan scanner text :start pos)
	(if s
	    (success e (subseq text s e))
	    (failure pos))))))

(defun clpcl-seq (&rest ps)
  (lambda (text pos)
    (let ((pos0 pos)
	  (ret  nil)
	  (failed nil))
      (loop for p in ps
	 do
	   (optima:match (funcall p text pos0)
	     ((success :pos pos1 :value v)
	      (setq pos0 pos1)
	      (setq ret (cons v ret)))
	     ((failure :pos pos1)
	      (setq pos0 pos1)
	      (setq failed t))
	     (otherwise
	      (error "xxxxxxx"))
	     )
	   )
      (if failed
	  (failure pos0)
	  (success pos0 (reverse ret))))))

(defun clpcl-bind (p action)
  (lambda (text pos)
    (let ((r (funcall p text pos)))
      (optima:match r
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
		 (intern "epcl-let")))
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



