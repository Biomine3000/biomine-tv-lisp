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
(defpackage #:biomine-tv
  (:use #:clim
	#:clim-lisp
	#:usocket
	#:bordeaux-threads
	#:object-system
	#:tv-protocol)
  (:export :main))

(in-package :biomine-tv)

(defun current-thread-name ()
  (thread-name (current-thread)))
;;;
;;; CLIM definitions
;;;
(define-application-frame tv ()
  ((current-object :initform nil))
  (:pointer-documentation t)
  (:panes
   (objects :application :width 300 :height 600
	    :incremental-redisplay t
	    :display-function 'display-cached-objects)
   (flow :application :width 850 :height 400
	    :incremental-redisplay t
	    :display-function 'display-flow)
   (interactor :interactor :width 850 :height 200))
  (:layouts
   (default
       (horizontally ()
	   objects
	 (vertically ()
	   flow interactor)))))

(define-presentation-type business-object () :inherit-from 'string)
(define-presentation-type object-type () :inherit-from 'string)
(define-presentation-type payload () :inherit-from 'string)
(define-presentation-type payload-size () :inherit-from 'string)

(define-presentation-method present (object (type business-object) stream view &key)
  (declare (ignore view))
  (with-slots (object-type metadata payload) object
    (with-slots (content-type) object-type
      (let
	  ((serializing-stream (make-string-output-stream :element-type 'character))
	   (payload (if (and payload (textual-p object-type))
			(babel:octets-to-string payload))))
	(if object-type
	    (progn
	      (serialize object-type serializing-stream)
	      (format stream "Business Object of type ~s: ~:[NONDESCRIPT~;~a~]"
		      (get-output-stream-string serializing-stream)
		      (textual-p object-type) payload))
	    (progn
	      (format stream "Business Object ")
	      (serialize-alist-metadata stream metadata)))))))

(define-presentation-method present (object (type payload-size) stream view &key)
  (declare (ignore view))
  (format stream "~a" object))

(defmethod display-object ((object business-object) stream)
  (with-slots (object-type payload payload-size) object
    (let
	((serializing-stream (make-string-output-stream :element-type 'character)))
      (serialize object-type serializing-stream)

      (with-text-face (stream :italic)
	(write-string "Type: " stream))
      (with-output-as-presentation (stream object 'object-type)
	(write-string (get-output-stream-string serializing-stream) stream))
      (terpri stream)

      (with-text-face (stream :italic)
	(write-string "Payload size: " stream))
      (present payload-size 'payload-size :stream stream)
      (terpri stream)

      (if (textual-p object-type)
	  (progn
	    (with-text-face (stream :italic)
	      (write-string "Plaintext content: " stream))
	    (with-output-as-presentation (stream object 'payload)
	      (write-string (babel:octets-to-string payload) stream))
	    (terpri stream))))))

(defmethod display-flow ((frame tv) stream)
  (let ((flow
	 (with-lock-held (*flow-lock*)
	   (reverse *flow*))))
    (dolist (object flow)
      (updating-output (stream :unique-id object)
	(present object 'business-object :stream stream)
	(terpri stream)))))

(defmethod display-cached-objects ((frame tv) stream)
  (dolist (object *object-cache*)
    (updating-output (stream :unique-id object)
      (present object 'business-object :stream stream)
      (terpri stream))))

(define-tv-command com-select-object
    ((object 'business-object :gesture :select))
  (setf (slot-value *application-frame* 'current-object) object))

(define-tv-command (com-quit :name t) ()
  (frame-exit *application-frame*))

(define-tv-command (com-make-plaintext-object :name t) ((payload 'string))
  (let
      ((object (make-plaintext-business-object payload)))
    (push object *object-cache*)))

(define-tv-command (com-make-subscribe-object :name t) ()
  (let
      ((object (make-subscribe-object)))
    (push object *object-cache*)))

(define-tv-command (com-send-business-object :name t) ((message-object 'business-object))
  (bt:with-lock-held (*outbound-lock*)
    (bt:with-lock-held (*flow-lock*)
      (push message-object *outbound-queue*)
      (push message-object *flow*))))

;; (define-tv-command com-change-payload
;;     ((object 'payload :gesture :select))
;;   (let ((new-contents (accept 'string :stream (frame-standard-input *application-frame*)
;; 			      :prompt "Contents" :default (payload object))))
;;     (setf (payload object) new-contents)))

(define-tv-command (com-redisplay :name t) ()
    (pane-needs-redisplay (get-frame-pane *application-frame* 'objects)))

;;;
;;; TV proper
;;;
(defvar *outbound-queue* '())
(defvar *flow* '())
(defvar *object-cache* '())
(defvar *outbound-lock* (bt:make-lock "outbound-lock"))
(defvar *flow-lock* (bt:make-lock "flow-lock"))

(defun run-user-interface ()
  (let
      ((frame (make-application-frame 'tv)))
    (setf (frame-pretty-name frame) "BIOMINE TV")
    (run-frame-top-level frame)))

(defclass repaint-event (device-event) () (:default-initargs :modifier-state 0))

(defmethod handle-event ((client application-pane) (event repaint-event))
  (with-application-frame (frame)
    (redisplay-frame-pane frame client)))

(defun run-server-connection (host port)
  (format *error-output* "Current thread for connect-to-object-server: ~a~%" (current-thread-name))
  (loop
     (format *error-output* "Trying to connect to server at ~a:~a~%" host port)
     (handler-case
	 (let*
	     ((socket (socket-connect host port :protocol :stream :element-type '(unsigned-byte 8)))
	     ;; ((socket (socket-connect host port :protocol :stream :element-type 'character))
	      (stream (socket-stream socket)))
	   (format *error-output* "Succesfully connected to the server at ~a:~a~%" host port)
	   (loop
	      (bt:with-lock-held (*outbound-lock*)
		(if (> (length *outbound-queue*) 0)
		    (let
			((object (pop *outbound-queue*)))
		      (serialize object stream)
		      (force-output stream)
		      (format t "Sent ~a~%" object))))
	      (let 
		  ((ready-socket (wait-for-input socket :timeout 0.5 :ready-only t)))
		(if ready-socket
		    (let
			((read-object (read-object stream)))
		      (format *error-output* "Received object ~a~%" read-object)
		      (force-output *error-output*)
		      (bt:with-lock-held (*flow-lock*)
			(push 
			 read-object
			 *flow*))
		      (let*
			  ((frame (find-application-frame 'tv))
			   (top-level-sheet (frame-top-level-sheet frame))
			   (pane (find-pane-named frame 'flow))
			   (event (make-instance 'repaint-event :sheet pane)))
			(queue-event top-level-sheet event)))))))
       (connection-refused-error (cre)
	 (format *error-output* "~a: bailing out!~%" cre))
       (timeout-error (toe)
	 (format *error-output* "~a: bailing out!~%" toe))
       (error (e)
	 (format *error-output* "~a: bailing out!~%" e)))
     (sleep 1)))

(defun kill-tv-threads ()
  (dolist (thread (all-threads))
    (if (or
	 (string-equal "network-thread" (thread-name thread))
	 (string-equal "ui-thread" (thread-name thread)))
	(destroy-thread thread))))

(defun main (host port)
  (let
      ((ui-thread (make-thread #'run-user-interface :name "ui-thread"))
       (network-thread (make-thread
			#'(lambda () (run-server-connection host port)) :name "network-thread")))
    (format *error-output* "Current thread for main: ~a~%" (current-thread-name))

    (join-thread ui-thread)
    ;; (join-thread network-thread)
    (destroy-thread network-thread)
    (cl-user::quit)))
