(defsystem "clpcl"
  :version "0.0.1"
  :author "hiro.shinke"
  :depends-on (:cl-ppcre
  	       :optima
	       )
  :components ((:file "clpcl"))
  :in-order-to ((test-op (test-op testclpcl))))

	
	       
