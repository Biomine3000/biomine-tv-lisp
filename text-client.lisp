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
  (:use	#:common-lisp #:object-system #:tv-protocol #:tv-services)
  (:export :main))

(in-package :text-client)

(defun connect-to-server (host port &key (object-handler nil)
			  (after-connection nil))
  (let*
      ((socket (usocket:socket-connect host port :protocol :stream :element-type '(unsigned-byte 8)))
       (stream (usocket:socket-stream socket)))

    (when after-connection
      (funcall after-connection host port socket stream))

    (loop
       (let 
	   ((ready-socket (usocket:wait-for-input socket :timeout 0.5 :ready-only t)))
	 (if ready-socket
	     (let
		 ((read-object (read-object stream)))

	       (when object-handler
		 (funcall object-handler read-object))))))))

(defun getenv (name &optional default)
  #+CMU
  (let ((x (assoc name ext:*environment-list*
		  :test #'string=)))
    (if x (cdr x) default))
  #-CMU
  (or
   #+Allegro (sys:getenv name)
   #+CLISP (ext:getenv name)
   #+ECL (si:getenv name)
   #+SBCL (sb-unix::posix-getenv name)
   #+ccl (ccl:getenv name)
   #+LISPWORKS (lispworks:environment-variable name)
   default))

(defun main (host port)
  (format *error-output* "Trying to connect to server at ~a:~a~%" host port)
     (handler-case
	 (connect-to-server host port
			    :object-handler
			    #'(lambda (obj)
				(format *error-output* "Received ~a~%" obj)
				(force-output *error-output*))
			    :after-connection
			    #'(lambda (host port socket stream)
				(format *error-output*
					"Succesfully connected to the server at ~a:~a~%"
					host port)
				(let ((subscription (make-subscribe-object))
				      (client-registration (make-service-call-object
							    "clients"
							    :parameters
							    (acons :request "join"
								   (acons :client (package-name :text-client)
									  (acons
									   :user (getenv "USER") nil)))))
				      (client-list (make-service-call-object
						    "clients"
						    :parameters
						    (acons :request "list" nil))))

				  (serialize subscription stream)
				  (force-output stream)
				  (format *error-output* "Sent subscription object ~a~%" subscription)
				  (multiple-value-bind (reply time-elapsed)
					(wait-for-reply-to socket stream subscription)
				    (if reply
					(format *error-output*
						"Received subscription reply ~a in ~$~%"
						reply time-elapsed)
					(format *error-output* "No reply to subscription!~%")))

				  (serialize client-registration stream)
				  (force-output stream)
				  (format *error-output* "Sent client registration ~a~%" client-list)
				  (multiple-value-bind (reply time-elapsed)
					(wait-for-reply-to socket stream client-list)
				    (if reply
					(format *error-output*
						"Received registration reply ~a in ~$~%"
						reply time-elapsed)
					(format *error-output* "No reply to client registration!~%")))

				  (serialize client-list stream)
				  (force-output stream)
				  (format *error-output* "Sent client list request ~a~%" client-list)
				  (multiple-value-bind (reply time-elapsed)
					(wait-for-reply-to socket stream client-list)
				    (if reply
					(format *error-output*
						"Received client list reply ~a in ~$~%"
						reply time-elapsed)
					(format *error-output* "No reply to client-list!~%"))))))
       (usocket:connection-refused-error (cre)
	 (format *error-output* "~a: bailing out!~%" cre))
       (usocket:timeout-error (toe)
	 (format *error-output* "~a: bailing out!~%" toe))
       (error (e)
	 (format *error-output* "~a: bailing out!~%" e))))
