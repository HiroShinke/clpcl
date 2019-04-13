

(in-package :cl-user)
(defpackage clpcl-calc
  (:use :cl :clpcl)
  (:export :clpcl-calc
	   ))
(in-package :clpcl-calc)


(defun token-regexp (str)
;;  (clpcl-debug (format "token, str=%s" str)
  (clpcl-token (clpcl-regexp str))
;;  )
  )

(defun clpcl-calc (text)

  " expr    = term   `chainl1` addop
    term    = factor `chainl1` mulop
    factor  = parens expr <|> integer
   
    mulop   =   do{ symbol \"*\"; return (*)}
    <|> do{ symbol \"/\"; return (div) }
   
    addop   =   do{ symbol \"+\"; return (+) }
    <|> do{ symbol \"-\"; return (-) }
   "
  (let* (
	 (multop (clpcl-or
		  (clpcl-let ((nil (token-regexp "\\*"))) #'*)
		  (clpcl-let ((nil (token-regexp "/"))) #'/)))
	 
	 (addop  (clpcl-or
		  (clpcl-let ((nil (token-regexp "\\+"))) #'+)
		  (clpcl-let ((nil (token-regexp "-"))) #'-)))
	 
	 (digit (clpcl-bind (token-regexp "\\d+")
			    #'parse-integer))
	 (factor (clpcl-or
		  (clpcl-paren (token-regexp "\\(")
			       (clpcl-lazy expr)
			       (token-regexp "\\)"))
		  digit))

	 (term (clpcl-chainl-1 factor multop))
	 (expr (clpcl-chainl-1 term addop))
	 )
    (clpcl-parse expr text)
    )
  )

;; (ert-deftest calc1 ()
;;   (should (equal 100 (clpcl-ret-value (clpcl-calc-string "100"))))
;;   (should (equal 3 (clpcl-ret-value (clpcl-calc-string "1 + 2"))))
;;   (should (equal 2 (clpcl-ret-value (clpcl-calc-string "3 - 1"))))
;;   (should (equal 2 (clpcl-ret-value (clpcl-calc-string "4 / 2"))))
;;   (should (equal 8 (clpcl-ret-value (clpcl-calc-string "4 * 2"))))
;;   (should (equal 7 (clpcl-ret-value (clpcl-calc-string "1 + 2 * 3"))))
;;   (should (equal 9 (clpcl-ret-value (clpcl-calc-string "(1 + 2) * 3"))))
;;   (should (equal 8 (clpcl-ret-value (clpcl-calc-string " 2 * (1 + 3)"))))
;;   )

