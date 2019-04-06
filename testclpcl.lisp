
(ql:quickload :fiveam)
(ql:quickload :clpcl)

(use-package :fiveam)

(test simple-regexp
  "test for pprp based Regexp parser"
  (is (clpcl-parse (clpcl-regexp "a") "a")
      (success 1 "a")))

(test seq-parser
  "test for parser seq"
  (let* ((a (clpcl-regexp "a"))
	 (b (clpcl-regexp "b"))
	 (c (clpcl-regexp "c"))
	 (p (clpcl-seq a b c)))
    (is 
     (clpcl-parse p "abc")
     (success 3 '("a" "b" "c")))))

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
     (clpcl-parse p "abc")
     (success 3 '("a" "b" "c")))
    )
  )

(run!)

