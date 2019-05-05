

(in-package :cl-user)
(defpackage test-clpcl-asd
  (:use :cl :asdf :uiop))
(in-package :test-clpcl-asd)

(defsystem "testclpcl2"
  :version "0.0.1"
  :author "hiro.shinke"
  :depends-on (:clpcl2
  	       :fiveam
	       )
  :components ((:file "testclpcl2"))
  :perform (test-op (o s)
		    (symbol-call :fiveam :run! :clpcl2)))


