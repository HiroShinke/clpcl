

(in-package :cl-user)
(defpackage test-clpcl-asd
  (:use :cl :asdf :uiop))
(in-package :test-clpcl-asd)

(defsystem "testclpcl"
  :version "0.0.1"
  :author "hiro.shinke"
  :depends-on (:clpcl
  	       :fiveam
	       )
  :components ((:file "testclpcl"))
  :perform (test-op (o s)
		    (symbol-call :fiveam :run! :clpcl)))


	
	       
