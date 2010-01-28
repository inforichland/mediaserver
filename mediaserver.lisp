(in-package :cl-user)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (asdf:oos 'asdf:load-op :cl-fad))

(defpackage :cl-mediaserver
  (:use :cl-fad
	:cl)
  (:export :search-media))

(in-package :cl-mediaserver)

;; the media server code

(defparameter *songs* (make-array
		       100
		       :element-type 'pathname
		       :adjustable t
		       :fill-pointer 0))

(defun search-media (directory)
  (declare (ftype (function (string) nil) search-media))
  (cl-fad:walk-directory
   directory
   #'(lambda (path)
       (when (equal "mp3" (pathname-type path))
	 (vector-push-extend path *songs*))))
  (values))

