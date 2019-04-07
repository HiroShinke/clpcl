
(in-package :cl-user)
(defpackage clpcl-test
  (:use :cl :fiveam :clpcl))
(in-package :clpcl-test)

(def-suite :clpcl)
(in-suite :clpcl)

;;(ql:quickload :clpcl)
(use-package :clpcl)

(test simple-regexp
  "test for pprp based Regexp parser"
  (is (equalp
       (success 1 "a")
       (clpcl-parse (clpcl-regexp "a") "a")
       )))

(test seq-parser
  "test for parser seq"
  (let* ((a (clpcl-regexp "a"))
	 (b (clpcl-regexp "b"))
	 (c (clpcl-regexp "c"))
	 (p (clpcl-seq a b c)))
    (is
     (equalp
      (success 3 '("a" "b" "c"))
      (clpcl-parse p "abc")
      )
     )
    )
  )

(test clpcl-let
  "test for clpcl-let"
  (let* ((a (clpcl-regexp "a"))
	 (b (clpcl-regexp "b"))
	 (c (clpcl-regexp "c"))
	 (p (clpcl-let ((x a)
			(y b)
			(z c))
		       (list x y z))))
    (is
     (equalp
      (success 3 '("a" "b" "c"))
      (clpcl-parse p "abc"))
     )
    )
  )

