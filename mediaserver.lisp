(in-package :cl-user)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (asdf:oos 'asdf:load-op :cl-fad))

(defpackage :cl-mediaserver
  (:use :cl-fad
	:cl)
  (:export :search-media))

(in-package :cl-mediaserver)

;; the media server code

(defparameter *songs* nil)

(defun search-media (directory)
  (declare (ftype (function (string) (vector t)) search-media))
  (let ((songs (make-array 100 :element-type 'pathname :adjustable t :fill-pointer 0)))
    (cl-fad:walk-directory
     directory
     #'(lambda (path)
	 (when (equal "mp3" (pathname-type path))
	   (vector-push-extend path songs))))
    songs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ID3 parsing code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; all we really need out of the ID3
;; tag is the title, artist, album, genre

(defstruct song-info
  title
  artist
  album
  genre)

(defun synchsafe (bytes)
  (reduce #'(lambda (x a)
	      (logior (ash x 7) a)) bytes :initial-value 0))

(defun read-n-from (stream n &optional (start 0))
  (if (not (open-stream-p stream))
      nil
      (let ((arr (make-array n)))
	(file-position stream start)
	(read-sequence arr stream)
	arr)))

(defun id3-version (filename)
  (with-open-file (file filename)
    (let* ((len (file-length file))
	   (tagpos (- len 128)))
      (cond ((< len 128) 'unknown)
	    ((string= (coerce (read-n-from file 3) 'string) "ID3") 'id3v2)
	    (t
	     (if (string= (coerce (read-n-from file 3 tagpos) 'string) "TAG")
		 'id3v2
		 'unknown))))))
