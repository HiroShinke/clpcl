
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
       ))
  (is (equalp
       (failure 0)
       (clpcl-parse (clpcl-regexp "a") "xa")
       ))
  )

(test token-parser
  "test for pprp based Regexp parser"
  (is (equalp
       (success 4 "a")
       (let* ((a (clpcl-regexp "a"))
	      (p (clpcl-token a)))
	 (clpcl-parse p "   a"))))
  (is (equalp
       (failure 0)
       (let* ((a (clpcl-regexp "a"))
  	      (p (clpcl-token a)))
  	 (clpcl-parse p "   b"))))
  )

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
    (is
     (equalp
      (failure 0)
      (clpcl-parse p "xbc")
      )
     )
    (is
     (equalp
      (failure 1)
      (clpcl-parse p "axc")
      )
     )
    (is
     (equalp
      (failure 2)
      (clpcl-parse p "abx")
      )
     )
    )
  )

(test many-parser
  "test for parser seq"
  (let* ((a (clpcl-regexp "a"))
	 (b (clpcl-regexp "b"))
	 (p (clpcl-many a))
	 (seq (clpcl-let ((x a)
			  (y b)) (concatenate 'string x y)))
	 (p2 (clpcl-many seq)))
    (is
     (equalp
      (success 3 '("a" "a" "a"))
      (clpcl-parse p "aaa")
      )
     )
    (is
     (equalp
      (success 0 '())
      (clpcl-parse p "xaa")
      )
     )
    (is
     (equalp
      (success 1 '("a"))
      (clpcl-parse p "axa")
      )
     )
    (is
     (equalp
      (success 2 '("a" "a"))
      (clpcl-parse p "aax")
      )
     )
    (is
     (equalp
      (success 6 '("ab" "ab" "ab"))
      (clpcl-parse p2 "ababab")
      )
     )
    (is
     (equalp
      (failure 1)
      (clpcl-parse p2 "axabab")
      )
     )
    (is
     (equalp
      (failure 3)
      (clpcl-parse p2 "abaxab")
      )
     )
    (is
     (equalp
      (failure 5)
      (clpcl-parse p2 "ababax")
      )
     )
    )
  )

(test debug-parser
  "test for parser seq"
  (let* ((a (clpcl-regexp "a"))
	 (p (clpcl-debug "debug" a)))
    (is
     (equalp
      (success 1 "a")
      (clpcl-parse p "aaa")
      )
     )
    )
  )

(test or-parser
  "test for parser or"
  (let* ((a (clpcl-regexp "a"))
	 (b (clpcl-regexp "b"))
	 (c (clpcl-regexp "c"))
	 (p (clpcl-or a b c)))
    (is
     (equalp
      (success 1 "a")
      (clpcl-parse p "ax")
      )
     )
    (is
     (equalp
      (success 1 "b")
      (clpcl-parse p "bx")
      )
     )
    (is
     (equalp
      (success 1 "c")
      (clpcl-parse p "cx")
      )
     )
    (is
     (equalp
      (failure 0)
      (clpcl-parse p "xx")
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
    (is
     (equalp
      (failure 0)
      (clpcl-parse p "xbc"))
     )
    (is
     (equalp
      (failure 1)
      (clpcl-parse p "axc"))
     )
    (is
     (equalp
      (failure 2)
      (clpcl-parse p "abx"))
     )
    )
  )

(test clpcl-let*
  "test for clpcl-let*"
  (let* ((a (clpcl-regexp "a"))
	 (b (clpcl-regexp "b"))
	 (c (clpcl-regexp "c"))
	 (p (clpcl-let* ((x a)
			(y b)
			(z c))
		       (list x y z))))
    (is
     (equalp
      (success 3 '("a" "b" "c"))
      (clpcl-parse p "abc"))
     )
    (is
     (equalp
      (failure 0)
      (clpcl-parse p "xbc"))
     )
    (is
     (equalp
      (failure 1)
      (clpcl-parse p "axc"))
     )
    (is
     (equalp
      (failure 2)
      (clpcl-parse p "abx"))
     )
    )
  )

(test clpcl-chainl-1
  "test for clpcl-let*"
  (let* ((d  (clpcl-let
	      ((s (clpcl-regexp "\\d+")))
	      (parse-integer s)))
	 (op (clpcl-let
	      ((nil (clpcl-regexp "\\+")))
	      #'+))
	 (c (clpcl-chainl-1 d op))
	 )
    (is
     (equalp
      (success 3 3)
      (clpcl-parse c "1+2"))
     )
    (is
     (equalp
      (success 9 15)
      (clpcl-parse c "1+2+3+4+5"))
     )
    )
  )

(test clpcl-paren
  "test for clpcl-pare"
  (let* ((a (clpcl-regexp "a"))
	 (b (clpcl-regexp "b"))
	 (c (clpcl-regexp "c"))
	 (p (clpcl-paren a b c)))
    (is
     (equalp
      (success 3 "b")
      (clpcl-parse p "abc"))
     )
    )
  )

(test clpcl-option
  "test for clpcl-option"
  (let* ((a (clpcl-regexp "a"))
	 (p (clpcl-option a)))
    (is
     (equalp
      (success 1 "a")
      (clpcl-parse p "a"))
     )
    (is
     (equalp
      (success 0 nil)
      (clpcl-parse p "b"))
     )
    )
  )

(test clpcl-sep-by
  "test for clpcl-option"
  (let* ((a (clpcl-regexp "a"))
	 (sep (clpcl-regexp ","))
	 (p (clpcl-sep-by a sep)))
    (is
     (equalp
      (success 1 '("a"))
      (clpcl-parse p "a"))
     )
    (is
     (equalp
      (success 0 '())
      (clpcl-parse p "b"))
     )
    (is
     (equalp
      (success 5 '("a" "a" "a"))
      (clpcl-parse p "a,a,a")
      )
     )
    (is
     (equalp
      (failure 4)
      (clpcl-parse p "a,a,x")
      )
     )
    )
  )

(test clpcl-sep-by-1
  "test for clpcl-option"
  (let* ((a (clpcl-regexp "a"))
	 (sep (clpcl-regexp ","))
	 (p (clpcl-sep-by-1 a sep)))
    (is
     (equalp
      (success 1 '("a"))
      (clpcl-parse p "a"))
     )
    (is
     (equalp
      (failure 0)
      (clpcl-parse p "b"))
     )
    (is
     (equalp
      (success 5 '("a" "a" "a"))
      (clpcl-parse p "a,a,a")
      )
     )
    (is
     (equalp
      (failure 4)
      (clpcl-parse p "a,a,x")
      )
     )
    )
  )

(test clpcl-end-by
  "test for clpcl-option"
  (let* ((a (clpcl-regexp "a"))
	 (sep (clpcl-regexp ";"))
	 (p (clpcl-end-by a sep)))
    (is
     (equalp
      (failure 1)
      (clpcl-parse p "a"))
     )
    (is
     (equalp
      (success 2 '("a"))
      (clpcl-parse p "a;"))
     )
    (is
     (equalp
      (success 1 '())
      (clpcl-parse p ";b"))
     )
    (is
     (equalp
      (success 6 '("a" "a" "a"))
      (clpcl-parse p "a;a;a;")
      )
     )
    (is
     (equalp
      (failure 3)
      (clpcl-parse p "a;axa;")
      )
     )
    (is
     (equalp
      (failure 1)
      (clpcl-parse p "aaa;")
      )
     )
    )
  )


(test clpcl-end-by-1
  "test for clpcl-option"
  (let* ((a (clpcl-regexp "a"))
	 (sep (clpcl-regexp ";"))
	 (p (clpcl-end-by-1 a sep)))
    (is
     (equalp
      (failure 1)
      (clpcl-parse p "a"))
     )
    (is
     (equalp
      (success 2 '("a"))
      (clpcl-parse p "a;"))
     )
    (is
     (equalp
      (failure 0)
      (clpcl-parse p "b"))
     )
    (is
     (equalp
      (success 6 '("a" "a" "a"))
      (clpcl-parse p "a;a;a;")
      )
     )
    (is
     (equalp
      (failure 3)
      (clpcl-parse p "a;axa;")
      )
     )
    )
  )

(test clpcl-sep-end-by-1
  "test for clpcl-option"
  (let* ((a (clpcl-regexp "a"))
	 (sep (clpcl-regexp ";"))
	 (p (clpcl-sep-end-by-1 a sep)))
    (is
     (equalp
      (success 1 '("a"))
      (clpcl-parse p "a"))
     )
    (is
     (equalp
      (success 2 '("a"))
      (clpcl-parse p "a;"))
     )
    (is
     (equalp
      (failure 0)
      (clpcl-parse p ";b"))
     )
    (is
     (equalp
      (success 6 '("a" "a" "a"))
      (clpcl-parse p "a;a;a;")
      )
     )
    (is
     (equalp
      (success 1 '("a"))
      (clpcl-parse p "aaa;")
      )
     )
    )
  )


(test clpcl-sep-end-by
  "test for clpcl-option"
  (let* ((a (clpcl-regexp "a"))
	 (sep (clpcl-regexp ";"))
	 (p (clpcl-sep-end-by a sep)))
    (is
     (equalp
      (success 1 '("a"))
      (clpcl-parse p "a"))
     )
    (is
     (equalp
      (success 2 '("a"))
      (clpcl-parse p "a;"))
     )
    (is
     (equalp
      (success 1 '())
      (clpcl-parse p ";b"))
     )
    (is
     (equalp
      (success 6 '("a" "a" "a"))
      (clpcl-parse p "a;a;a;")
      )
     )
    (is
     (equalp
      (success 5 '("a" "a" "a"))
      (clpcl-parse p "a;a;a")
      )
     )
    (is
     (equalp
      (success 1 '("a"))
      (clpcl-parse p "aaa;")
      )
     )
    )
  )





