(asdf:defsystem #:biomine-tv
  :depends-on (#:bordeaux-threads
	       #:flexi-streams
	       #:babel
	       #:object-system
	       #:mcclim)
  :components ((:file "protocol"
		      "biomine-tv"
		      "text-client")))