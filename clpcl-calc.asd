

(in-package :cl-user)
(defpackage clpcl-calc-asd
  (:use :cl :asdf :uiop))
(in-package :clpcl-calc-asd)

(defsystem "clpcl-calc"
  :version "0.0.1"
  :author "hiro.shinke"
  :depends-on (:clpcl
	       )
  :components ((:file "clpcl-calc"))
)


	
	       
