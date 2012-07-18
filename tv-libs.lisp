;;;;
;;;; Helper to load library dependencies for core
;;;;
(ql:quickload '("usocket" "cl-json" "bordeaux-threads" "flexi-streams"
		"cl-ppcre" "mcclim" "ironclad" "babel" "mcclim"))
(sb-ext:save-lisp-and-die "tv.core" :purify t :compression t)