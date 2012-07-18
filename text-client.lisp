;;;; Copyright (c) 2012, Atte Hinkka <atte.hinkka@iki.fi>
;;;; 
;;;; Permission to use, copy, modify, and/or distribute this software for any
;;;; purpose with or without fee is hereby granted, provided that the above
;;;; copyright notice and this permission notice appear in all copies.
;;;; 
;;;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
(defpackage #:text-client
  (:use	#:common-lisp #:object-system #:tv-protocol)
  (:export :main))

(in-package :text-client)

(defun connect-to-server (host port &key (object-handler nil)
			  (after-connection nil))
  (let*
      ((socket (usocket:socket-connect host port :protocol :stream :element-type '(unsigned-byte 8)))
       (stream (usocket:socket-stream socket)))

    (when after-connection
      (funcall after-connection host port stream))

    (loop
       (let 
	   ((ready-socket (usocket:wait-for-input socket :timeout 0.5 :ready-only t)))
	 (if ready-socket
	     (let
		 ((read-object (read-object stream)))

	       (when object-handler
		 (funcall object-handler read-object))))))))

(defun main (host port)
  (format *error-output* "Trying to connect to server at ~a:~a~%" host port)
     (handler-case
	 (connect-to-server host port
			    :object-handler
			    #'(lambda (obj)
				(format *error-output* "Received ~a~%" obj)
				(force-output *error-output*))
			    :after-connection
			    #'(lambda (host port stream)
				(format *error-output*
					"Succesfully connected to the server at ~a:~a~%"
					host port)
				(let ((subscription (make-subscribe-object)))
				  (serialize subscription stream)
				  (force-output stream)
				  (format *error-output* "Sent subscription object ~a~%" subscription))))
       (connection-refused-error (cre)
	 (format *error-output* "~a: bailing out!~%" cre))
       (timeout-error (toe)
	 (format *error-output* "~a: bailing out!~%" toe))
       (error (e)
	 (format *error-output* "~a: bailing out!~%" e))))
