(defsystem "clpcl2"
  :version "0.0.1"
  :author "hiro.shinke"
  :depends-on (:cl-ppcre
  	       :optima
	       )
  :components ((:file "clpcl2"))
  :in-order-to ((test-op (test-op testclpcl2))))

	
	       
